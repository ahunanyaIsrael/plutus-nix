{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Prelude                        (Show, IO(), putStrLn, String)
import qualified Prelude

import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)

import           PlutusTx.Builtins              (divideInteger)

import qualified Plutus.V2.Ledger.Api           as V2
import           Plutus.V2.Ledger.Api
    (BuiltinData, POSIXTime(..), Validator, ScriptContext(..),
     TxInfo(..), PubKeyHash, unValidatorScript)

import qualified Plutus.V2.Ledger.Contexts      as Ctx

import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as LBS8
import qualified Codec.Serialise                 as Serialise
import           System.Directory                (createDirectoryIfMissing)
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Base16.Lazy     as B16

------------------------------------------------------------
-- DATUM & REDEEMER
------------------------------------------------------------

data StreamDatum = StreamDatum
    { sdSender     :: PubKeyHash
    , sdRecipient  :: PubKeyHash
    , sdRatePerSec :: Integer
    , sdStart      :: POSIXTime
    , sdEnd        :: POSIXTime
    , sdClaimed    :: Integer
    }
    deriving Show

data StreamRedeemer = Claim | Cancel
    deriving Show

PlutusTx.makeIsDataIndexed ''StreamDatum [('StreamDatum,0)]
PlutusTx.makeIsDataIndexed ''StreamRedeemer [('Claim,0),('Cancel,1)]
PlutusTx.makeLift ''StreamDatum
PlutusTx.makeLift ''StreamRedeemer

------------------------------------------------------------
-- TIME HELPERS
------------------------------------------------------------

{-# INLINABLE getUpperPOSIX #-}
getUpperPOSIX :: TxInfo -> Maybe POSIXTime
getUpperPOSIX txinfo =
    case Ctx.txInfoValidRange txinfo of
        V2.Interval _ (V2.UpperBound (V2.Finite t) _) -> Just t
        _                                             -> Nothing

{-# INLINABLE posixDiffSeconds #-}
posixDiffSeconds :: POSIXTime -> POSIXTime -> Integer
posixDiffSeconds (POSIXTime a) (POSIXTime b) =
    divideInteger (a - b) 1000

{-# INLINABLE clamp #-}
clamp :: POSIXTime -> POSIXTime -> POSIXTime -> POSIXTime
clamp low high x =
    let x' = if x < low then low else x
    in if x' > high then high else x'

{-# INLINABLE accruedAt #-}
accruedAt :: POSIXTime -> POSIXTime -> POSIXTime -> Integer -> Integer
accruedAt start end now rate =
    let capped = clamp start end now
        elapsed = posixDiffSeconds capped start
    in if elapsed <= 0 then 0 else elapsed * rate

------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------

{-# INLINABLE validateStream #-}
validateStream :: StreamDatum -> StreamRedeemer -> ScriptContext -> Bool
validateStream sd red ctx =
    let info = Ctx.scriptContextTxInfo ctx

        now = case getUpperPOSIX info of
                Just t  -> t
                Nothing -> sdEnd sd

        totalAccrued = accruedAt (sdStart sd) (sdEnd sd) now (sdRatePerSec sd)
        claimableRaw = totalAccrued - sdClaimed sd
        claimable = if claimableRaw < 0 then 0 else claimableRaw

        sigs = Ctx.txInfoSignatories info
        signedByRecipient = sdRecipient sd `elem` sigs
        signedBySender    = sdSender sd `elem` sigs

    in case red of
        Claim ->
            traceIfFalse "recipient must sign claim" signedByRecipient &&
            traceIfFalse "nothing to claim" (claimable > 0)

        Cancel ->
            traceIfFalse "sender must sign cancel" signedBySender

------------------------------------------------------------
-- WRAPPER
------------------------------------------------------------

{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped d r c =
    case ( PlutusTx.fromBuiltinData d :: Maybe StreamDatum
         , PlutusTx.fromBuiltinData r :: Maybe StreamRedeemer
         , PlutusTx.fromBuiltinData c :: Maybe ScriptContext
         ) of
        (Just sd, Just red, Just ctx) ->
            if validateStream sd red ctx
               then ()
               else traceError "stream validation failed"
        _ -> traceError "bad redeemer/datum/context"

validator :: Validator
validator =
    V2.mkValidatorScript $$(PlutusTx.compile [|| mkWrapped ||])

------------------------------------------------------------
-- WRITE CARDANO-CLI-COMPATIBLE JSON
------------------------------------------------------------

writeValidatorJSON :: String -> V2.Validator -> IO ()
writeValidatorJSON filePath val = do
    let script = unValidatorScript val
        serialised = Serialise.serialise script
        cborHex = B16.encode serialised

        -- Convert lazy ByteString to String safely
        cborHexStr :: String
        cborHexStr = LBS8.unpack cborHex

        jsonObj = Aeson.object
          [ "type" Aeson..= ("PlutusV2" :: String)
          , "description" Aeson..= ("Streaming payment validator" :: String)
          , "cborHex" Aeson..= cborHexStr
          ]

    createDirectoryIfMissing True "./compiled"
    LBS.writeFile filePath (Aeson.encode jsonObj)

------------------------------------------------------------
-- MAIN
------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Writing validator to ./compiled/stream-validator.plutus"
    writeValidatorJSON "./compiled/stream-validator.plutus" validator
    putStrLn "Done."

