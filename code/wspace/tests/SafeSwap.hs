{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module SafeSwap where

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
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-- Datum & Redeemer

data RouteDatum = RouteDatum
    { rdHops      :: [CurrencySymbol]         -- pool identifiers
    , rdInAsset   :: (CurrencySymbol, TokenName)
    , rdOutAsset  :: (CurrencySymbol, TokenName)
    , rdMinOut    :: Integer
    , rdDeadline  :: POSIXTime
    , rdRecipient :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''RouteDatum

data RouteAction = Execute { pathProof :: [CurrencySymbol] }
PlutusTx.unstableMakeIsData ''RouteAction

-- Helpers

{-# INLINABLE containsAllHops #-}
containsAllHops :: [CurrencySymbol] -> [CurrencySymbol] -> Bool
containsAllHops required actual = all (`elem` actual) required

{-# INLINABLE outputAtLeast #-}
outputAtLeast :: TxInfo -> PubKeyHash -> (CurrencySymbol, TokenName) -> Integer -> Bool
outputAtLeast info pkh (cs, tn) amt =
    let v = valuePaidTo info pkh
    in valueOf v cs tn >= amt

{-# INLINABLE beforeDeadline #-}
beforeDeadline :: POSIXTime -> POSIXTimeRange -> Bool
beforeDeadline dl range = Interval.contains (Interval.to dl) range

-- Validator logic

{-# INLINABLE mkValidator #-}
mkValidator :: RouteDatum -> RouteAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx

        txRange :: POSIXTimeRange
        txRange = txInfoValidRange info
    in
    case action of
        Execute proof ->
            traceIfFalse "missing hops"    (containsAllHops (rdHops dat) proof) &&
            traceIfFalse "minOut not met"  (outputAtLeast info (rdRecipient dat) (rdOutAsset dat) (rdMinOut dat)) &&
            traceIfFalse "deadline passed" (beforeDeadline (rdDeadline dat) txRange)

-- Boilerplate

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @RouteDatum d
        red = unsafeFromBuiltinData @RouteAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-- On-chain and off-chain addresses

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
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
        shelleyAddr =
            C.makeShelleyAddressInEra
            network
            (C.PaymentCredentialByScript scriptHash)
            C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)



-- File writing

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path


-- Main

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    
    writeValidator "safeSwap.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Safe Swap Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Safe Swap validator generated successfully."

