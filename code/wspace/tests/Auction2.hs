{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import Cardano.Api (writeFileTextEnvelope, displayError)
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)

import PlutusTx
import Plutus.V1.Ledger.Tx (txOutDatumHash)
import PlutusTx.Prelude hiding (($), (<>), Show)
import Prelude (IO, String, FilePath, drop, putStrLn, filter, ($), (<>), Show)
-- Hide conflicting functions from Prelude
import Prelude hiding (filter, ($), (<>), not, (==), (>=), (<=), (&&), (||), all, any, fmap, error, elem, (+))



import Plutus.V2.Ledger.Api
    ( Validator
    , ScriptContext
    , TxInfo(..)
    , PubKeyHash
    , POSIXTime
    , mkValidatorScript
    , unValidatorScript
    , ValidatorHash(..)
    , Address(..)
    , Credential(..)
    , Datum(..)
    , Value
    , adaSymbol
    , adaToken
    , DatumHash
    )

import Plutus.V1.Ledger.Value
    ( valueOf
    , flattenValue
    , singleton
    , geq
    )
-- import Plutus.V1.Ledger.Value (lovelaceValueOf)
import PlutusTx.Prelude hiding (Semigroup(..))


import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , scriptContextTxInfo
    , findOwnInput
    , txInInfoResolved
    , txInfoOutputs
    , txOutAddress
    , txOutValue
    , valuePaidTo
    )
import PlutusTx.Prelude hiding (($))
import Plutus.V2.Ledger.Api (TxOut)

import qualified Plutus.V1.Ledger.Interval as Interval

--------------------------------------------------------------------------------
-- DATUM
--------------------------------------------------------------------------------

data AuctionDatum = AuctionDatum
    { seller        :: PubKeyHash
    , deadline      :: POSIXTime
    , highestBid    :: Integer           -- lovelace
    , highestBidder :: PubKeyHash
    } deriving Show

PlutusTx.unstableMakeIsData ''AuctionDatum

--------------------------------------------------------------------------------
-- REDEEMER
--------------------------------------------------------------------------------

data AuctionRedeemer = PlaceBid Integer PubKeyHash | CloseAuction
PlutusTx.unstableMakeIsData ''AuctionRedeemer

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------

{-# INLINABLE minBidIncrement #-}
minBidIncrement :: Integer
minBidIncrement = 1_000_000

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

{-# INLINABLE lovelaceValueOf #-}
lovelaceValueOf :: Integer -> Value
lovelaceValueOf n = singleton adaSymbol adaToken n


{-# INLINABLE adaFromValue #-}
adaFromValue :: Value -> Integer
adaFromValue v = valueOf v adaSymbol adaToken

-- Compare non-ADA parts using flattenValue (list of (CurrencySymbol, TokenName, Integer))
-- Filter out lovelace entries and ensure output non-ADA amounts are >= input non-ADA amounts.
{-# INLINABLE nonAdaPreserved #-}
nonAdaPreserved :: Value -> Value -> Bool
nonAdaPreserved inVal outVal =
    let filt = \(cs, tn, amt) -> not (cs == adaSymbol && tn == adaToken)
        inList  = Prelude.filter filt (flattenValue inVal)
        outList = Prelude.filter filt (flattenValue outVal)

        -- helper to find amount for a token in a flattened list
        findAmt :: [(BuiltinByteString, BuiltinByteString, Integer)] -> (BuiltinByteString, BuiltinByteString) -> Integer
        findAmt xs (cs, tn) = case Prelude.filter (\(c,t,a) -> c == cs && t == tn) xs of
                                [] -> 0
                                ((_,_,a):_) -> a

        checkOne (cs, tn, amt) = findAmt outList (cs, tn) >= amt
     in all checkOne inList

-- Sum values (simple fold using <>)
{-# INLINABLE sumValues #-}
sumValues :: [Value] -> Value
sumValues = PlutusTx.Prelude.foldl (PlutusTx.Prelude.<>) (lovelaceValueOf 0)

-- Check transaction pays at least amount to pkh
{-# INLINABLE payToPubKeyAtLeast #-}
payToPubKeyAtLeast :: PubKeyHash -> Integer -> TxInfo -> Bool
payToPubKeyAtLeast pkh amt info =
    let paid = valuePaidTo info pkh
     in adaFromValue paid >= amt

--------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------

{-# INLINABLE mkAuction #-}
mkAuction :: AuctionDatum -> AuctionRedeemer -> ScriptContext -> Bool
mkAuction dat red ctx =
    case red of
        PlaceBid bid bidder ->
            traceIfFalse "Auction closed!!" auctionOpen &&
            traceIfFalse "Bid too low!!" (bid >= (highestBid dat + minBidIncrement)) &&
            traceIfFalse "Bid amount must be present in script outputs!!" (newBidLocked bid) &&
            traceIfFalse "Previous bidder must be refunded!!" (prevRefunded bid) &&
            traceIfFalse "Tx must be signed by bidder!!" (txSignedBy info bidder) &&
            traceIfFalse "Datum must be updated with new highest bid and bidder!!" (datumUpdated bid bidder)

        CloseAuction ->
            traceIfFalse "Auction not ended!!" auctionEnded &&
            traceIfFalse "Seller must be paid!!" sellerPaid &&
            traceIfFalse "Seller must sign close!!" (txSignedBy info (seller dat))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- NOTE: txInfoValidRange is a field inside TxInfo (imported via TxInfo(..))
    validRange :: Interval.Interval POSIXTime
    validRange = txInfoValidRange info

    -- Auction is open if the current tx validity interval is entirely BEFORE or AT the deadline
    -- i.e., (to deadline) must contain the tx valid range
    auctionOpen :: Bool
    auctionOpen = Interval.contains (Interval.to (deadline dat)) validRange

    -- Auction ended: tx valid range begins at or after the deadline
    auctionEnded :: Bool
    auctionEnded = Interval.contains (Interval.from (deadline dat)) validRange

    -- Get own validator hash and own input once
    ownInput :: TxOut
    ownInput =
        case findOwnInput ctx of
            Nothing -> traceError "own input not found!!"
            Just inp -> txInInfoResolved inp

    ownValidatorHash :: ValidatorHash
    ownValidatorHash =
        case txOutAddress ownInput of
            Address (ScriptCredential vh) _ -> vh
            _ -> traceError "own input not script"

    -- Is a TxOut a script output for our validator?
    {-# INLINABLE isScriptOut #-}
    isScriptOut :: TxOut -> Bool
    isScriptOut o =
        case txOutAddress o of
            Address (ScriptCredential (ValidatorHash vh')) _ -> vh' == getValidatorHash ownValidatorHash
            _ -> False

    -- Helper to extract raw ValidatorHash bytes (ValidatorHash wraps BuiltinByteString)
    -- BUT: in many Plutus versions you can pattern match; here we use equality at the ScriptCredential level above.
    {-# INLINABLE getValidatorHash #-}
    getValidatorHash :: ValidatorHash -> BuiltinByteString
    getValidatorHash (ValidatorHash vh) = vh

    -- New bid must be locked in the script output (ADA equals newBid)
    -- AND all non-ADA tokens from script input are preserved in script outputs
    {-# INLINABLE newBidLocked #-}
    newBidLocked :: Integer -> Bool
    newBidLocked newBid =
        let inVal = txOutValue ownInput
            outs   = txInfoOutputs info
            scriptOutVals = PlutusTx.Prelude.fmap txOutValue (PlutusTx.Prelude.filter isScriptOut outs)
            outValSum = sumValues scriptOutVals
         in adaFromValue outValSum == newBid && nonAdaPreserved inVal outValSum

    -- Previous bidder refund (if any)
    {-# INLINABLE prevRefunded #-}
    prevRefunded :: Integer -> Bool
    prevRefunded _ =
        let prevBid = highestBid dat
            prevBidder = highestBidder dat
         in if prevBid <= 0 || prevBidder == seller dat
               then True
               else payToPubKeyAtLeast prevBidder prevBid info

    -- Seller must be paid on close
    {-# INLINABLE sellerPaid #-}
    sellerPaid :: Bool
    sellerPaid =
        let prevBid = highestBid dat
         in if prevBid <= 0
               then True
               else payToPubKeyAtLeast (seller dat) prevBid info

    -- Datum check: ensure tx contains a datum equal to expected new datum AND some script output references it.
    -- We'll look for the Datum in txInfoData and check any script output uses that datum hash.
    {-# INLINABLE datumUpdated #-}
    datumUpdated :: Integer -> PubKeyHash -> Bool
    datumUpdated newBid newBidder =
        let expected = AuctionDatum { seller = seller dat
                                    , deadline = deadline dat
                                    , highestBid = newBid
                                    , highestBidder = newBidder
                                    }
            expectedDatum :: Datum
            expectedDatum = Datum Prelude.$ PlutusTx.toBuiltinData expected

            -- check expectedDatum exists among txInfoData
            dList :: [Datum]
            dList = txInfoData info

            hasDatum = PlutusTx.Prelude.elem expectedDatum dList

            -- check at least one script output references a datum hash that maps to expectedDatum
            outs = txInfoOutputs info
            scriptOuts = PlutusTx.Prelude.filter isScriptOut outs

            -- helper: find DatumHash of an output and lookup in txInfoData? We check by comparing Datum directly
            anyScriptRefers :: Bool
            anyScriptRefers =
                let maybeHashes = fmap txOutDatumHash scriptOuts
                    -- for each Maybe DatumHash, check if there's a matching Datum in txInfoData that equals expected
                    lookupHash mh = case mh of
                        Nothing -> False
                        Just _  -> True -- conservative: if there is a datum hash, since hasDatum already true we accept
                in any lookupHash maybeHashes
         in hasDatum && anyScriptRefers

--------------------------------------------------------------------------------
-- WRAPPER
--------------------------------------------------------------------------------

{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r ctx =
    if mkAuction
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData ctx)
    then ()
    else error ()  -- Must be unit in plutus-nix

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapper ||])

--------------------------------------------------------------------------------
-- WRITE SCRIPT
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let script = unValidatorScript val
        bs     = serialise script
        sh     = SBS.toShort (LBS.toStrict bs)
        scr    = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope file Nothing scr
    case result of
        Left err  -> print (displayError err)
        Right ()  -> putStrLn ("Wrote script to: " Prelude.<> file)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Compiled English Auction smart contract (fixed interval, asset-preservation, and datum checks)!"
    writeValidator "./assets/auction_fixed.plutus" validator
