{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Main where

import PlutusTx
import PlutusTx.Prelude hiding (($), (<>), Show)
import Prelude (IO, FilePath, print, putStrLn, Show, ($), (<>))

-- Ledger types
import Plutus.V2.Ledger.Api
    ( BuiltinData
    , Validator
    , PubKeyHash
    , POSIXTime
    , ScriptContext(..)
    , TxInfo(..)
    , TxOut(..)
    , mkValidatorScript
    , Datum(..)
    , Value
    , txOutDatum
    , OutputDatum(..)       -- imports OutputDatum, OutputDatumHash, NoOutputDatum
    )

-- Context helpers
import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    )

import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value   -- provides valueOf

-- Serialization / Cardano API
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api           (writeFileTextEnvelope, displayError, PlutusScriptV2)
import Cardano.Api.Shelley   (PlutusScript(..))


--------------------------------------------------------------------------------
-- DATUM
--------------------------------------------------------------------------------

data AllowanceDatum = AllowanceDatum
    { student     :: PubKeyHash
    , nextUnlock  :: POSIXTime
    , interval    :: POSIXTime
    , amountPer   :: Integer
    , periodsLeft :: Integer
    }
    deriving Show

unstableMakeIsData ''AllowanceDatum


--------------------------------------------------------------------------------
-- REDEEMER
--------------------------------------------------------------------------------

data AllowanceRedeemer = Withdraw
PlutusTx.makeIsDataIndexed ''AllowanceRedeemer [('Withdraw, 0)]


--------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------

{-# INLINABLE mkAllowance #-}
mkAllowance :: AllowanceDatum -> AllowanceRedeemer -> ScriptContext -> Bool
mkAllowance dat _ ctx =
    traceIfFalse "Student signature missing!!" signed &&
    traceIfFalse "Unlock time not reached" unlocked &&
    traceIfFalse "Incorrect payout to student" payoutCorrect &&
    traceIfFalse "Continuing output wrong" continuingOK
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    --------------------------------------------------------------------------
    -- 1. Student must sign
    --------------------------------------------------------------------------
    signed :: Bool
    signed = txSignedBy info (student dat)

    --------------------------------------------------------------------------
    -- 2. Deadline check
    --------------------------------------------------------------------------
    unlocked :: Bool
    unlocked = Interval.from (nextUnlock dat) `Interval.contains` txInfoValidRange info

    --------------------------------------------------------------------------
    -- 3. Student must be paid amountPer lovelace
    --------------------------------------------------------------------------
    payoutCorrect :: Bool
    payoutCorrect =
        let paid = valuePaidTo info (student dat)
            ada  = Value.valueOf paid Value.adaSymbol Value.adaToken
        in ada == amountPer dat

    --------------------------------------------------------------------------
    -- 4. Continuing outputs
    --------------------------------------------------------------------------
    continuing :: [TxOut]
    continuing = getContinuingOutputs ctx

    continuingOK :: Bool
    continuingOK =
        if periodsLeft dat > 1
        then case continuing of
            [o] -> validateNextDatum o
            _   -> traceError "Expected exactly one continuing output"
        else null continuing

    --------------------------------------------------------------------------
    -- Check continuing datum
    --------------------------------------------------------------------------
    validateNextDatum :: TxOut -> Bool
    validateNextDatum o =
        case txOutDatum o of
            NoOutputDatum ->
                traceError "Missing datum"
            OutputDatumHash _ ->
                traceError "Expected inline datum, got hash"
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d :: Maybe AllowanceDatum of
                    Nothing -> traceError "Bad datum"
                    Just nd ->
                           student nd     == student dat
                        && interval nd    == interval dat
                        && amountPer nd   == amountPer dat
                        && nextUnlock nd  == nextUnlock dat + interval dat
                        && periodsLeft nd == periodsLeft dat - 1


--------------------------------------------------------------------------------
-- WRAPPER
--------------------------------------------------------------------------------

{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r c =
    let dat = unsafeFromBuiltinData d
        red = unsafeFromBuiltinData r
        ctx = unsafeFromBuiltinData c
    in if mkAllowance dat red ctx
          then ()
          else error ()

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapper ||])


--------------------------------------------------------------------------------
-- WRITE SCRIPT
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let bs  = serialise val
        sh  = SBS.toShort (LBS.toStrict bs)
        scr = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope file Nothing scr
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote script to: " <> file)


--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Compiled Allowance smart contract!"
    writeValidator "./assets/allowance.plutus" validator
