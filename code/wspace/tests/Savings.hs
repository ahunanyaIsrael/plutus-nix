-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

-- module Savings where

-- import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode)
-- import PlutusTx.Prelude hiding (($), (<>))
-- import Plutus.V2.Ledger.Api
--     ( BuiltinData
--     , Validator
--     , mkValidatorScript
--     , PubKeyHash
--     , POSIXTime
--     )
-- import Plutus.V2.Ledger.Contexts
--     ( ScriptContext(..)
--     , TxInfo(..)
--     , txSignedBy
--     )
-- import Plutus.V1.Ledger.Interval (contains, from)
-- import Prelude (IO, print, putStrLn, FilePath, ($), (<>))
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import Codec.Serialise (serialise)
-- import Cardano.Api (writeFileTextEnvelope, displayError, PlutusScriptV2)
-- import Cardano.Api.Shelley (PlutusScript (..))

-- -- ====================================================
-- --  Minimal Time-Locked Savings Validator Logic
-- -- ====================================================

-- {-# INLINABLE mkSavingsValidator #-}
-- mkSavingsValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
-- mkSavingsValidator owner deadline _ ctx =
--     traceIfFalse "Owner signature missing" signedByOwner &&
--     traceIfFalse "Too early to withdraw" pastDeadline
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     signedByOwner :: Bool
--     signedByOwner = txSignedBy info owner

--     pastDeadline :: Bool
--     pastDeadline = from deadline `contains` txInfoValidRange info

-- -- ====================================================
-- --  Wrapper for Plutus
-- -- ====================================================

-- {-# INLINABLE wrap #-}
-- wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrap o d c =
--     let ownerPkh = unsafeFromBuiltinData o
--         dl       = unsafeFromBuiltinData d
--         ctx      = unsafeFromBuiltinData c
--         red      = ()  -- unit redeemer
--     in if mkSavingsValidator ownerPkh dl red ctx
--           then ()
--           else error ()

-- compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- compiledCode = $$(compile [|| wrap ||])

-- validator :: Validator
-- validator = mkValidatorScript compiledCode

-- -- ====================================================
-- --  Script Export Helper
-- -- ====================================================

-- writeValidator :: FilePath -> Validator -> IO ()
-- writeValidator file val = do
--     let scriptSerialised = serialise val
--         scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
--         script :: PlutusScript PlutusScriptV2
--         script = PlutusScriptSerialised scriptShort
--     result <- writeFileTextEnvelope file Nothing script
--     case result of
--         Left err -> print (displayError err)
--         Right () -> putStrLn ("Wrote validator to: " <> file)

-- -- ====================================================
-- --  Main
-- -- ====================================================

-- main :: IO ()
-- main = do
--     putStrLn "Time-locked savings validator compiled successfully!"
--     writeValidator "./assets/time-locked-savings.plutus" validator

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Savings where

import PlutusTx (compile, unstableMakeIsData, CompiledCode, unsafeFromBuiltinData)

import PlutusTx.Prelude hiding (($), (<>), Show)
import Plutus.V2.Ledger.Api
    ( BuiltinData
    , Validator
    , mkValidatorScript
    , PubKeyHash
    , POSIXTime
    )
import Plutus.V2.Ledger.Contexts
    ( ScriptContext(..)
    , TxInfo(..)
    , txSignedBy
    )
import Plutus.V1.Ledger.Interval (contains, from)
import Prelude (IO, print, putStrLn, FilePath, ($), (<>), Show)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api (writeFileTextEnvelope, displayError, PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (..))

-- ====================================================
--  Datum Type (Extended)
-- ====================================================

data SavingsDatum = SavingsDatum
    { sdOwner    :: PubKeyHash           -- who can withdraw
    , sdDeadline :: POSIXTime            -- unlock time
    , sdGoal     :: BuiltinByteString    -- goal name
    , sdAmount   :: Integer              -- amount in lovelace
    }
    deriving (Show)

PlutusTx.unstableMakeIsData ''SavingsDatum

-- ====================================================
--  Validator Logic
-- ====================================================

{-# INLINABLE mkSavingsValidator #-}
mkSavingsValidator :: SavingsDatum -> () -> ScriptContext -> Bool
mkSavingsValidator dat _ ctx =
    traceIfFalse "Owner signature missing" signedByOwner &&
    traceIfFalse "Too early to withdraw" pastDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info (sdOwner dat)

    pastDeadline :: Bool
    pastDeadline = from (sdDeadline dat) `contains` txInfoValidRange info

-- ====================================================
--  Wrapper for Plutus
-- ====================================================

{-# INLINABLE wrap #-}
wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrap d r c =
    let datum = unsafeFromBuiltinData d
        ctx   = unsafeFromBuiltinData c
    in if mkSavingsValidator datum () ctx
          then ()
          else error ()

compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode = $$(compile [|| wrap ||])

validator :: Validator
validator = mkValidatorScript compiledCode

-- ====================================================
--  Script Export Helper
-- ====================================================

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let scriptSerialised = serialise val
        scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
        script :: PlutusScript PlutusScriptV2
        script = PlutusScriptSerialised scriptShort
    result <- writeFileTextEnvelope file Nothing script
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote validator to: " <> file)

-- ====================================================
--  Main
-- ====================================================

main :: IO ()
main = do
    putStrLn "Time-locked savings validator with extended datum compiled successfully!"
    writeValidator "./assets/time-locked-savings.plutus" validator
