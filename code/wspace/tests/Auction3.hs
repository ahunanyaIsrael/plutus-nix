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
import PlutusTx.Prelude hiding (($), (<>), Show)
import Prelude (IO, FilePath, print, putStrLn, Show, ($), (<>))

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
    )
import Plutus.V1.Ledger.Value(valueOf)
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

-- Minimum increment in lovelace (1 ADA = 1_000_000 lovelace)
{-# INLINABLE minBidIncrement #-}
minBidIncrement :: Integer
minBidIncrement = 1_000_000

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

{-# INLINABLE adaFromValue #-}
adaFromValue :: Value -> Integer
adaFromValue v = valueOf v adaSymbol adaToken

-- Sum the lovelace locked to this validator in the transaction outputs
{-# INLINABLE totalLockedByValidator #-}
totalLockedByValidator :: ValidatorHash -> TxInfo -> Integer
totalLockedByValidator (ValidatorHash vh) info =
    let outs = txInfoOutputs info
        isScriptOut o =
            case txOutAddress o of
                Address (ScriptCredential (ValidatorHash vh')) _ -> vh' == vh
                _ -> False
        scriptOutValues = fmap txOutValue (filter isScriptOut outs)
        total = foldl (\acc v -> acc + adaFromValue v) 0 scriptOutValues
     in total

-- check tx pays 'amount' lovelace to a PubKeyHash
{-# INLINABLE payToPubKey #-}
payToPubKey :: PubKeyHash -> Integer -> TxInfo -> Bool
payToPubKey pkh amt info =
    let paid = valuePaidTo info pkh
     in adaFromValue paid == amt

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
            traceIfFalse "Tx must be signed by bidder!!" (txSignedBy info bidder)

        CloseAuction ->
            traceIfFalse "Auction not ended!!" auctionEnded &&
            traceIfFalse "Seller must be paid!!" sellerPaid &&
            traceIfFalse "Seller must sign close!!" (txSignedBy info (seller dat))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validRange :: Interval.Interval POSIXTime
    validRange = txInfoValidRange info

    -- Auction is open while the tx validity is entirely before (or at) the deadline
    auctionOpen :: Bool
    auctionOpen =
        Interval.contains
            (Interval.to (deadline dat))  -- (-∞ .. deadline]
            validRange                    -- [validFrom .. validTo]

    -- Auction has ended when the tx happens at or after the deadline
    auctionEnded :: Bool
    auctionEnded =
        Interval.contains
            validRange
            (Interval.from (deadline dat))  -- [deadline .. +∞)


    -- Get own validator hash
    ownValidatorHash :: ValidatorHash
    ownValidatorHash =
        case findOwnInput ctx of
            Nothing -> traceError "own input not found!!"
            Just inp ->
                case txOutAddress (txInInfoResolved inp) of
                    Address (ScriptCredential vh) _ -> vh
                    _ -> traceError "own input not script!!"

    -- New bid must be locked in the script output
    {-# INLINABLE newBidLocked #-}
    newBidLocked :: Integer -> Bool
    newBidLocked newBid =
        let total = totalLockedByValidator ownValidatorHash info
         in total == newBid

    -- Previous bidder refund
    {-# INLINABLE prevRefunded #-}
    prevRefunded :: Integer -> Bool
    prevRefunded _ =
        let prevBid = highestBid dat
            prevBidder = highestBidder dat
         in if prevBid <= 0 || prevBidder == seller dat
               then True
               else payToPubKey prevBidder prevBid info

    -- Seller must be paid on close
    sellerPaid :: Bool
    sellerPaid =
        let prevBid = highestBid dat
            scriptOutput = totalLockedByValidator ownValidatorHash info
        in if prevBid <= 0
            then True
            else
                let reasonableFee = 1_000_000  -- 1 ADA
                    minPayment =
                        if prevBid > reasonableFee
                            then prevBid - reasonableFee
                            else 0
                in adaFromValue (valuePaidTo info (seller dat)) >= minPayment
                   && scriptOutput == 0



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
        Right ()  -> putStrLn ("Wrote script to: " <> file)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Compiled English Auction smart contract (fixed redeemer scope & imports)!"
    writeValidator "./assets/auction6.plutus" validator