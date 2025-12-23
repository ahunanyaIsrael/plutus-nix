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
{-# LANGUAGE NamedFieldPuns #-}

module Main where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import Cardano.Api (writeFileTextEnvelope, displayError)
import Cardano.Api.Shelley
    ( PlutusScript (..)
    , PlutusScriptV2
    )

import PlutusTx
import PlutusTx.Prelude hiding (($), (<>), Show)
import Prelude (IO, FilePath, print, putStrLn, Show, ($), (<>))

import Plutus.V2.Ledger.Api
    ( Validator
    , ScriptContext(..)
    , TxInfo(..)
    , TxOut(..)
    , Datum(..)
    , Value
    , PubKeyHash
    , POSIXTime
    , ScriptPurpose(..)
    , mkValidatorScript
    , OutputDatum (..)      
    , unValidatorScript   
    )

import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    , txInInfoOutRef              -- FIXED
    , txInInfoResolved            -- FIXED
    )

import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value    as Value
import Plutus.V2.Ledger.Api (ScriptPurpose(Spending), TxOutRef)


--------------------------------------------------------------------------------
-- DATUM (UNCHANGED)
--------------------------------------------------------------------------------

data AllowanceDatum = AllowanceDatum
    { parent      :: PubKeyHash  -- NEW: Parent who created the allowance
    , student     :: PubKeyHash
    , nextUnlock  :: POSIXTime
    , interval    :: POSIXTime
    , amountPer   :: Integer
    , periodsLeft :: Integer
    }
    deriving Show

unstableMakeIsData ''AllowanceDatum


--------------------------------------------------------------------------------
-- REDEEMER (UPDATED: Added Cancel)
--------------------------------------------------------------------------------

data AllowanceRedeemer 
    = Withdraw     -- Student withdraws allowance
    | Cancel       -- Parent cancels and reclaims funds
    deriving Show

PlutusTx.makeIsDataIndexed ''AllowanceRedeemer 
    [ ('Withdraw, 0)
    , ('Cancel,   1)
    ]


--------------------------------------------------------------------------------
-- VALIDATOR (UPDATED: Added cancel logic)
--------------------------------------------------------------------------------

{-# INLINABLE mkAllowance #-}
mkAllowance :: AllowanceDatum -> AllowanceRedeemer -> ScriptContext -> Bool
mkAllowance dat red ctx =
    case red of
        Withdraw -> validateWithdraw dat ctx
        Cancel   -> validateCancel dat ctx

-- Validation for student withdrawal
validateWithdraw :: AllowanceDatum -> ScriptContext -> Bool
validateWithdraw dat ctx =
    traceIfFalse "Student signature missing!!" signed &&
    traceIfFalse "Unlock time not reached" unlocked &&
    traceIfFalse "Incorrect payout to student" payoutCorrect &&
    traceIfFalse "Continuing output wrong" continuingOK
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- 1. Student must sign
    signed :: Bool
    signed = txSignedBy info (student dat)

    -- 2. Deadline check
    unlocked :: Bool
    unlocked = Interval.from (nextUnlock dat)
              `Interval.contains`
              txInfoValidRange info

    -- 3. Student must be paid AT LEAST amountPer lovelace
    payoutCorrect :: Bool
    payoutCorrect =
        let paid = valuePaidTo info (student dat)
            ada  = Value.valueOf paid Value.adaSymbol Value.adaToken
        in ada >= amountPer dat

    -- 4. Continuing outputs
    continuing :: [TxOut]
    continuing = getContinuingOutputs ctx

    continuingOK :: Bool
    continuingOK =
        if periodsLeft dat > 1
        then case continuing of
            [o] -> validateNextDatum o
            _   -> traceError "Expected exactly one continuing output"
        else null continuing

    -- Check continuing datum
    validateNextDatum :: TxOut -> Bool
    validateNextDatum o =
        case txOutDatum o of
            NoOutputDatum      -> traceError "Missing datum"
            OutputDatumHash _  -> traceError "Expected inline datum"
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d of
                    Nothing  -> traceError "Bad datum"
                    Just nd ->
                           student nd     == student dat
                        && interval nd    == interval dat
                        && amountPer nd   == amountPer dat
                        && nextUnlock nd  == nextUnlock dat + interval dat
                        && periodsLeft nd == periodsLeft dat - 1

-- Validation for parent cancellation
validateCancel :: AllowanceDatum -> ScriptContext -> Bool
validateCancel dat ctx =
    traceIfFalse "Parent signature missing!" parentSigned &&
    traceIfFalse "Funds not returned to parent" fundsReturned &&
    traceIfFalse "No continuing outputs allowed" (noContinuingOutputs ctx)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- 1. Parent (from datum) must sign
    parentSigned :: Bool
    parentSigned = txSignedBy info (parent dat)

    -- 2. All funds must return to parent
    fundsReturned :: Bool
    fundsReturned =
        let paidToParent = valuePaidTo info (parent dat)
            -- Get the total value in the script UTxO being spent
            scriptValue = case findScriptInput ctx of
                Just (_, val) -> val
                Nothing -> traceError "Could not find script input"
            -- Check parent receives at least the script value (minus fees)
            parentReceives = Value.valueOf paidToParent Value.adaSymbol Value.adaToken
            scriptAda = Value.valueOf scriptValue Value.adaSymbol Value.adaToken
        in parentReceives >= scriptAda  -- Parent gets at least the script value

    -- Helper to find our script input
    findScriptInput :: ScriptContext -> Maybe (TxOut, Value)
    findScriptInput ctx =
        case scriptContextPurpose ctx of
            Spending txOutRef ->
                let info   = scriptContextTxInfo ctx
                    inputs = txInfoInputs info
                in case find (\i -> txInInfoOutRef i == txOutRef) inputs of
                    Just txIn ->
                        let out = txInInfoResolved txIn
                        in Just (out, txOutValue out)
                    Nothing -> Nothing

            _ -> Nothing

    -- 3. No continuing outputs allowed
noContinuingOutputs :: ScriptContext -> Bool
noContinuingOutputs ctx = null (getContinuingOutputs ctx)

--------------------------------------------------------------------------------
-- WRAPPER (UNCHANGED)
--------------------------------------------------------------------------------

{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r c =
    if mkAllowance
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData c)
    then ()
    else error ()


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
    putStrLn "Compiled Allowance smart contract (WITH CANCEL FUNCTION)!"
    writeValidator "./assets/allowance2.plutus" validator