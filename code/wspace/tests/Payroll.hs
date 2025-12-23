{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}

module Main where

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V2.Ledger.Api
import Prelude (Show, IO, putStrLn,print, FilePath, (<>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api           (writeFileTextEnvelope, displayError, PlutusScriptV2)
import Cardano.Api.Shelley   (PlutusScript(..))
import qualified Plutus.V1.Ledger.Interval as Interval
------------------------------------------------------------------------------------------
-- Payroll Types
------------------------------------------------------------------------------------------

data Employee = Employee
  { empPkh            :: PubKeyHash
  , empSalary         :: Integer          -- ADA in lovelace
  , empLastPay        :: POSIXTime
  , empNextPay        :: POSIXTime
  }
PlutusTx.unstableMakeIsData ''Employee


data PayrollDatum = PayrollDatum
  { owner       :: PubKeyHash
  , employees   :: [Employee]
  }
PlutusTx.unstableMakeIsData ''PayrollDatum


data PayrollAction =
      AddEmployee Employee
    | UpdateEmployee Employee
    | RemoveEmployee PubKeyHash
    | WithdrawSalary
PlutusTx.unstableMakeIsData ''PayrollAction


------------------------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------------------------

findEmployee :: PubKeyHash -> [Employee] -> Maybe Employee
findEmployee _ [] = Nothing
findEmployee pkh (e:es) =
  if empPkh e == pkh then Just e else findEmployee pkh es

------------------------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------------------------

{-# INLINABLE mkPayrollValidator #-}
mkPayrollValidator :: PayrollDatum -> PayrollAction -> ScriptContext -> Bool
mkPayrollValidator dat action ctx =
    case action of

      ---------------------------------------------------
      -- Only owner can add/update/remove employees
      ---------------------------------------------------
      AddEmployee emp ->
          traceIfFalse "not owner" isOwner &&
          traceIfFalse "employee exists!!" (isNothing $ findEmployee (empPkh emp) (employees dat))

      UpdateEmployee emp ->
          traceIfFalse "not owner" isOwner &&
          traceIfFalse "employee not found!!" (isJust $ findEmployee (empPkh emp) (employees dat))

      RemoveEmployee pkh ->
          traceIfFalse "not owner" isOwner &&
          traceIfFalse "employee not found!!" (isJust $ findEmployee pkh (employees dat))

      ---------------------------------------------------
      -- Employee withdraws salary (monthly)
      ---------------------------------------------------
      WithdrawSalary ->
        let signer = case txInfoSignatories info of
                      [s] -> s
                      _   -> traceError "expected single signer"
        in case findEmployee signer (employees dat) of
            Nothing -> traceError "not employee!!"
            Just emp ->
              let validRange = txInfoValidRange info
              in traceIfFalse "too early!!" (Interval.from (empNextPay emp) `Interval.contains` validRange)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    isOwner :: Bool
    isOwner = owner dat `elem` txInfoSignatories info

------------------------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------------------------

{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r c =
    let dat = unsafeFromBuiltinData d
        red = unsafeFromBuiltinData r
        ctx = unsafeFromBuiltinData c
    in if mkPayrollValidator dat red ctx
          then ()
          else traceError "Validation failed!!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapper ||])

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let bs  = serialise val
        sh  = SBS.toShort (LBS.toStrict bs)
        scr = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope file Nothing scr
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote script to: " <> file)

main :: IO ()
main = writeValidator "./assets/payroll1.plutus" validator
