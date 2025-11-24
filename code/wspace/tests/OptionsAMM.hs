{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module OptionsAMM where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (Value, valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

----------------------------------------
-- Datum Types
----------------------------------------

data OptionType = Call | Put
PlutusTx.unstableMakeIsData ''OptionType

data OptionSeriesDatum = OptionSeriesDatum
    { osUnderlier      :: CurrencySymbol
    , osStrike         :: Integer
    , osExpiry         :: POSIXTime
    , osType           :: OptionType
    , osCollateralReq  :: Integer
    }
PlutusTx.unstableMakeIsData ''OptionSeriesDatum

data PoolDatum = PoolDatum
    { pdResOption      :: Integer
    , pdResCollateral  :: Integer
    , pdFeeBps         :: Integer
    , pdPricingParams  :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''PoolDatum

data OptionsAction = Swap | AddLiq | RemoveLiq | Exercise
PlutusTx.unstableMakeIsData ''OptionsAction

----------------------------------------
-- Helpers
----------------------------------------

{-# INLINABLE scriptInputContainsToken #-}
scriptInputContainsToken :: ScriptContext -> CurrencySymbol -> TokenName -> Integer -> Bool
scriptInputContainsToken ctx cs tn amt =
    case findOwnInput ctx of
        Nothing -> traceError "no script input found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= amt

{-# INLINABLE afterExpiry #-}
afterExpiry :: POSIXTime -> TxInfo -> Bool
afterExpiry expiry info =
    let txRange = txInfoValidRange info
    in Interval.contains (Interval.from (expiry + 1)) txRange

{-# INLINABLE valuePaidToPubKey #-}
valuePaidToPubKey :: TxInfo -> PubKeyHash -> Value
valuePaidToPubKey info pkh =
    mconcat [txOutValue o | o <- txInfoOutputs info, txOutAddress o == pubKeyHashAddress pkh]

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

{-# INLINABLE findExerciser #-}
findExerciser :: TxInfo -> PubKeyHash
findExerciser tx = case txInfoSignatories tx of
                     [pkh] -> pkh
                     _     -> traceError "cannot identify exerciser"

{-# INLINABLE collateralSufficient #-}
collateralSufficient :: OptionSeriesDatum -> ScriptContext -> Bool
collateralSufficient od ctx =
    let v = txOutValue $ txInInfoResolved $ maybe (traceError "no input") id $ findOwnInput ctx
    in valueOf v adaSymbol adaToken >= osCollateralReq od

{-# INLINABLE canExercise #-}
canExercise :: OptionSeriesDatum -> TxInfo -> Bool
canExercise od info =
    afterExpiry (osExpiry od) info &&
    let v = valuePaidToPubKey info (findExerciser info)
    in valueOf v adaSymbol adaToken >= osCollateralReq od

----------------------------------------
-- Validator Logic
----------------------------------------

{-# INLINABLE mkOptionsValidator #-}
mkOptionsValidator :: OptionSeriesDatum -> OptionsAction -> ScriptContext -> Bool
mkOptionsValidator od action ctx =
    let info = scriptContextTxInfo ctx
    in case action of
        AddLiq    -> traceIfFalse "collateral must be sufficient" (collateralSufficient od ctx)
        RemoveLiq -> True
        Swap      -> True
        Exercise  -> traceIfFalse "option expired or not enough collateral" (canExercise od info)

----------------------------------------
-- Boilerplate
----------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let od  = unsafeFromBuiltinData @OptionSeriesDatum d
        act = unsafeFromBuiltinData @OptionsAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkOptionsValidator od act ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

----------------------------------------
-- Validator Hash + Addresses
----------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in ValidatorHash builtin

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
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

----------------------------------------
-- File writing
----------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

----------------------------------------
-- Main
----------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "options_amm.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Options AMM Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Options AMM validator generated successfully."
