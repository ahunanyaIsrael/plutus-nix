{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- SimpleLock.hs
-- A minimal Plutus V2 validator that locks funds to a PubKeyHash (the owner's pubkey hash).
-- The script succeeds when a transaction spending the script output is signed by that PubKeyHash.
-- The `main` at the bottom serialises the validator to a .plutus file (using cardano-api's helpers).

module Main where

import           PlutusTx.Prelude        hiding (Semigroup(..), unless)
import qualified PlutusTx
import qualified Prelude                 as Hask

import qualified Plutus.V2.Ledger.Api    as PlutusV2
import           Plutus.V2.Ledger.Contexts (ScriptContext(..), txSignedBy)

import qualified Ledger.Typed.Scripts    as Scripts
import qualified Ledger                  as Ledger

-- For serialisation to a .plutus file
import           Codec.Serialise         (serialise)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as SBS
import           Cardano.Api             (writeFileTextEnvelope)
import           Cardano.Api.Shelley     (PlutusScript (..), PlutusScriptV2)

-- | Validator logic:
-- Datum: PubKeyHash (the owner's pubkey hash)
-- Redeemer: ()
-- Condition: the spending transaction must be signed by the PubKeyHash in the datum.

{-# INLINABLE mkValidator #-}
mkValidator :: PlutusV2.PubKeyHash -> () -> ScriptContext -> Bool
mkValidator ownerPkh _ ctx =
    txSignedBy (scriptContextTxInfo ctx) ownerPkh

-- Boilerplate to create a typed validator
data Locking

instance Scripts.ValidatorTypes Locking where
    type instance DatumType Locking = PlutusV2.PubKeyHash
    type instance RedeemerType Locking = ()

typedValidator :: Scripts.TypedValidator Locking
typedValidator = Scripts.mkTypedValidator @Locking
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| Scripts.wrapValidator @PlutusV2.PubKeyHash @() ||])

validator :: PlutusV2.Validator
validator = Scripts.validatorScript typedValidator

validatorHash :: PlutusV2.ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

scriptAddress :: Ledger.Address
scriptAddress = Ledger.scriptAddress validator

-- Serialise the validator to a .plutus file (CBOR-wrapped script)
plutusScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
plutusScript v = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ serialise (PlutusV2.unValidatorScript v)

writePlutusFile :: FilePath -> PlutusV2.Validator -> IO ()
writePlutusFile fp v = do
    let s = plutusScript v
    r <- writeFileTextEnvelope fp Nothing s
    case r of
      Left err -> Hask.putStrLn $ "Error writing script: " <> Hask.show err
      Right () -> Hask.putStrLn $ "Wrote script to: " <> fp

-- Simple CLI: write the compiled validator to "simple-lock.plutus"
main :: IO ()
main = writePlutusFile "simple-lock.plutus" validator

-- NOTE: To use this file you will need a build environment with dependencies:
-- * plutus-ledger (V2), plutus-tx, cardano-api (compatible), codec-serialise, etc.
-- Typical workflow is to place this module in a project based on the `plutus-apps` repo or
-- a cabal project that depends on the Plutus libraries, then build and run the program to
-- produce `simple-lock.plutus`.

-- Example usage (high-level):
-- 1. Save this file as SimpleLock.hs inside a project with the right Plutus/cardano-api deps.
-- 2. Build and run the program (e.g. via cabal or nix-shell for plutus-apps):
--      cabal build && cabal run simple-lock
--    Or with nix-shell in plutus-apps:
--      nix-shell --run "cabal run simple-lock"
--    After running, you should get `simple-lock.plutus` in the working dir.
-- 3. Get the script address and lock funds to it using cardano-cli:
--      cardano-cli address build --payment-script-file simple-lock.plutus --testnet-magic <MAGIC> --out-file script.addr
--    (or --mainnet depending on your network)
-- 4. To unlock (spend) the funds, create a transaction that spends the script UTXO and is signed
--    by the private key corresponding to the PubKeyHash used as the datum on the locked UTXO.

-- The validator is intentionally minimal so it's easy to inspect and modify.
-- For example, you can change the datum type to include an expiry time, or require a specific redeemer value.
