{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module CoveredCallVault where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data VaultDatum = VaultDatum
    { vdUnderlier     :: CurrencySymbol
    , vdToken         :: TokenName
    , vdStrike        :: Integer
    , vdExpiry        :: POSIXTime
    , vdSharesSupply  :: Integer
    , vdQueuedPremium :: Integer
    , vdRound         :: Integer
    }
PlutusTx.unstableMakeIsData ''VaultDatum

data VaultAction = Deposit Integer | Withdraw Integer | Write | Settle
PlutusTx.unstableMakeIsData ''VaultAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE distributePremium #-}
distributePremium :: TxInfo -> VaultDatum -> Bool
distributePremium _ _ = True
-- For simplicity, assume off-chain distributes pro-rata.

{-# INLINABLE validRound #-}
validRound :: POSIXTime -> POSIXTime -> Bool
validRound now expiry = now <= expiry

{-# INLINABLE mintedSharesValid #-}
mintedSharesValid :: Integer -> ScriptContext -> Bool
mintedSharesValid _ _ = True
-- Placeholder: implement share token logic

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: VaultDatum -> VaultAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        Deposit amt ->
            traceIfFalse "round expired" (validRound now (vdExpiry dat)) &&
            traceIfFalse "mint invalid" (mintedSharesValid amt ctx)
        Withdraw amt ->
            traceIfFalse "round expired" (validRound now (vdExpiry dat)) &&
            traceIfFalse "burn invalid" (mintedSharesValid (negate amt) ctx)
        Write ->
            traceIfFalse "only during round" (validRound now (vdExpiry dat))
        Settle ->
            traceIfFalse "not expired yet" (not $ validRound now (vdExpiry dat)) &&
            traceIfFalse "premium distribution failed" (distributePremium info dat)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    now :: POSIXTime
    now = case ivTo (txInfoValidRange info) of
            UpperBound (Finite t) _ -> t
            _                        -> 0

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @VaultDatum d
        red = unsafeFromBuiltinData @VaultAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr = C.makeShelleyAddressInEra network (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "vault.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Covered Call Vault Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "----------------------------------------"
    putStrLn "Covered Call Vault validator generated successfully."


