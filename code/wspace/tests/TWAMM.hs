{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module TWAMM where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data LongOrderDatum = LongOrderDatum
    { loOwner     :: PubKeyHash
    , loDirection :: Bool        -- True = Buy, False = Sell
    , loTotal     :: Integer
    , loStart     :: POSIXTime
    , loEnd       :: POSIXTime
    , loExecuted  :: Integer
    }
PlutusTx.unstableMakeIsData ''LongOrderDatum

data PoolDatum = PoolDatum
    { poolX        :: Integer
    , poolY        :: Integer
    , poolVirtualX :: Integer
    , poolVirtualY :: Integer
    }
PlutusTx.unstableMakeIsData ''PoolDatum

data TWAMMAction = ExecuteSlice | Cancel
PlutusTx.unstableMakeIsData ''TWAMMAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE timeSlice #-}
timeSlice :: LongOrderDatum -> POSIXTime -> Integer
timeSlice lo current =
    let duration = ediff (loEnd lo) (loStart lo)
        rate     = safeDivide (loTotal lo) duration
        elapsed  = ediff current (loStart lo)
        sliceAmt = rate * elapsed
    in sliceAmt - loExecuted lo

{-# INLINABLE ediff #-}
ediff :: POSIXTime -> POSIXTime -> Integer
ediff a b = getPOSIXTime a - getPOSIXTime b

{-# INLINABLE safeDivide #-}
safeDivide :: Integer -> Integer -> Integer
safeDivide x y = Builtins.divideInteger x y

------------------------------------------------------------------------
-- Validator
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: LongOrderDatum -> TWAMMAction -> ScriptContext -> Bool
mkValidator lo action ctx =
    case action of
        ExecuteSlice ->
            traceIfFalse "execution before start" (txTimeAfter lo loStart) &&
            traceIfFalse "execution after end"   (txTimeBefore lo loEnd) &&
            traceIfFalse "slice > remaining"     (slice <= remaining) &&
            traceIfFalse "pool math inconsistent" poolInvariant
        Cancel ->
            traceIfFalse "only owner can cancel" (txSignedBy info (loOwner lo))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    current :: POSIXTime
    current = case ivFrom $ txInfoValidRange info of
                 Interval.LowerBound (Interval.Finite t) _ -> t
                 _ -> loEnd lo

    slice :: Integer
    slice = timeSlice lo current

    remaining :: Integer
    remaining = loTotal lo - loExecuted lo

    poolInvariant :: Bool
    poolInvariant = True -- Placeholder for AMM math checks (update virtual reserves accordingly)

    txTimeAfter :: LongOrderDatum -> (LongOrderDatum -> POSIXTime) -> Bool
    txTimeAfter lo f = current >= f lo

    txTimeBefore :: LongOrderDatum -> (LongOrderDatum -> POSIXTime) -> Bool
    txTimeBefore lo f = current <= f lo

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @LongOrderDatum d
        red = unsafeFromBuiltinData @TWAMMAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Address
------------------------------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        strictBS = LBS.toStrict bytes          -- Lazy ByteString -> ByteString
        builtin = Builtins.toBuiltin strictBS  -- ByteString -> BuiltinByteString
    in ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = LBS.toStrict $ Serialise.serialise val
        plutusScript = CS.PlutusScriptSerialised (SBS.toShort serialised)
        scriptHash = C.hashScript (C.PlutusScript CS.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr = C.makeShelleyAddressInEra
                        network
                        (C.PaymentCredentialByScript scriptHash)
                        C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: P.FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "twamm-validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- TWAMM Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "TWAMM validator generated successfully."

