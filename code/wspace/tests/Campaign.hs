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
    , TxOutRef
    )

import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    , txInInfoOutRef
    , txInInfoResolved
    )

import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value    as Value
import PlutusTx (fromBuiltinData)

--------------------------------------------------------------------------------
-- DATUM
--------------------------------------------------------------------------------

data CampaignDatum = CampaignDatum
  { owner        :: PubKeyHash
  , campaignGoal :: Integer     -- minimum target (lovelace)
  , deadline     :: POSIXTime   -- no donations after this
  , donorCount   :: Integer     -- number of distinct donations
  , topDonor     :: PubKeyHash  -- highest donor
  , topDonation  :: Integer     -- highest single donation
  }
  deriving Show

-- Derive on-chain data conversions
PlutusTx.unstableMakeIsData ''CampaignDatum
-- The above generates ToData and FromData. It also supplies UnsafeFromData machinery used on-chain.

--------------------------------------------------------------------------------
-- REDEEMER
--------------------------------------------------------------------------------

data DonationRedeemer = Donate | Withdraw
  deriving Show

PlutusTx.makeIsDataIndexed ''DonationRedeemer [ ('Donate, 0), ('Withdraw, 1) ]

--------------------------------------------------------------------------------
-- VALIDATOR LOGIC
--------------------------------------------------------------------------------

{-# INLINABLE findScriptInput #-}
-- Find the UTxO input that spends from this script (the spent script input) and return (TxOut, value)
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

{-# INLINABLE validateDonate #-}
validateDonate :: CampaignDatum -> ScriptContext -> Bool
validateDonate oldDat ctx =
    traceIfFalse "Donate: must be before or at deadline" beforeDeadline &&
    traceIfFalse "Donate: must include a signer (donor)" hasSigner &&
    traceIfFalse "Donate: must produce exactly one continuing script output" oneContinuing &&
    traceIfFalse "Donate: new datum validation failed" newDatumOk &&
    traceIfFalse "Donate: donation must be > 0" donationPositive
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- 1. before deadline: txInfoValidRange must be contained by (to deadline)
    beforeDeadline :: Bool
    beforeDeadline = Interval.to (deadline oldDat) `Interval.contains` txInfoValidRange info

    -- 2. pick a donor from signatories (require at least one signer)
    signers :: [PubKeyHash]
    signers = txInfoSignatories info

    hasSigner :: Bool
    hasSigner = not (null signers)

    donor :: PubKeyHash
    donor = case signers of
              (s:_) -> s
              []    -> traceError "No signer found"

    -- 3. continuing outputs (script outputs) - there must be exactly one (the updated campaign)
    continuing :: [TxOut]
    continuing = getContinuingOutputs ctx

    oneContinuing :: Bool
    oneContinuing = case continuing of
                      [_] -> True
                      _   -> False

    -- 4. the spent script input value and the continuing output value -> donation = new - old
    scriptValue :: Integer
    scriptValue = case findScriptInput ctx of
                    Just (_, v) -> Value.valueOf v Value.adaSymbol Value.adaToken
                    Nothing     -> traceError "Could not find script input"

    newOutput :: TxOut
    newOutput = case continuing of
                  [o] -> o
                  _   -> traceError "Expected exactly one continuing output"

    newValue :: Integer
    newValue = Value.valueOf (txOutValue newOutput) Value.adaSymbol Value.adaToken

    donationAmount :: Integer
    donationAmount = newValue - scriptValue

    donationPositive :: Bool
    donationPositive = donationAmount > 0

    -- 5. read new datum from continuing output (must be inline datum)
    newDatum :: CampaignDatum
    newDatum =
      case txOutDatum newOutput of
        OutputDatum (Datum d) ->
            case PlutusTx.fromBuiltinData d of
              Just cd -> cd
              Nothing -> traceError "Bad datum in continuing output"
        OutputDatumHash _ -> traceError "Expected inline datum (not hash)"
        NoOutputDatum     -> traceError "Missing datum in continuing output"

    -- 6. Validate that fields were updated correctly:
    --    - owner, campaignGoal, deadline must be unchanged
    --    - donorCount increased by 1
    --    - topDonor/topDonation updated if donation > old topDonation, otherwise unchanged
    newDatumOk :: Bool
    newDatumOk =
         owner newDatum        == owner oldDat
      && campaignGoal newDatum == campaignGoal oldDat
      && deadline newDatum     == deadline oldDat
      && donorCount newDatum   == donorCount oldDat + 1
      && if donationAmount > topDonation oldDat
            then topDonation newDatum == donationAmount
                 && topDonor newDatum == donor
            else topDonation newDatum == topDonation oldDat
                 && topDonor newDatum == topDonor oldDat

{-# INLINABLE validateWithdraw #-}
validateWithdraw :: CampaignDatum -> ScriptContext -> Bool
validateWithdraw dat ctx =
    traceIfFalse "Withdraw: must be after deadline" afterDeadline &&
    traceIfFalse "Withdraw: only owner can withdraw" signedByOwner &&
    traceIfFalse "Withdraw: must reach goal" goalReached &&
    traceIfFalse "Withdraw: no continuing outputs allowed" noContinuing
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- after deadline: txInfoValidRange must be contained by (from deadline)
    afterDeadline :: Bool
    afterDeadline = Interval.from (deadline dat) `Interval.contains` txInfoValidRange info

    signedByOwner :: Bool
    signedByOwner = txSignedBy info (owner dat)

    -- Total paid to owner in this transaction
    paidToOwner :: Integer
    paidToOwner = let v = valuePaidTo info (owner dat)
                  in Value.valueOf v Value.adaSymbol Value.adaToken

    goalReached :: Bool
    goalReached = paidToOwner >= campaignGoal dat

    noContinuing :: Bool
    noContinuing = null (getContinuingOutputs ctx)

{-# INLINABLE mkDonationValidator #-}
mkDonationValidator :: CampaignDatum -> DonationRedeemer -> ScriptContext -> Bool
mkDonationValidator dat red ctx =
  case red of
    Donate   -> validateDonate dat ctx
    Withdraw -> validateWithdraw dat ctx

--------------------------------------------------------------------------------
-- WRAPPER / COMPILATION
--------------------------------------------------------------------------------

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    if mkDonationValidator
         (unsafeFromBuiltinData d)
         (unsafeFromBuiltinData r)
         (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])

--------------------------------------------------------------------------------
-- WRITE SCRIPT (to .plutus)
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
    putStrLn "Compiled Donation smart contract (Plutus V2)"
    writeValidator "./assets/donation.plutus" validator
