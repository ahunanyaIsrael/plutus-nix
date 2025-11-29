{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module MultiHopRouter where

import Prelude (IO, String, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as V
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data Hop = Hop
    { hopPool   :: BuiltinByteString
    , hopIn     :: (CurrencySymbol, TokenName)
    , hopOut    :: (CurrencySymbol, TokenName)
    , hopMinOut :: Integer
    }
PlutusTx.unstableMakeIsData ''Hop

data RouterDatum = RouterDatum
PlutusTx.unstableMakeIsData ''RouterDatum

data RouterAction = Route
    { routeHops      :: [Hop]
    , routeMinOut    :: Integer
    , routeRecipient :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''RouterAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE valueOfAsset #-}
valueOfAsset :: V.Value -> (CurrencySymbol, TokenName) -> Integer
valueOfAsset v (cs, tn) = V.valueOf v cs tn

{-# INLINABLE txPaidTo #-}
txPaidTo :: TxInfo -> PubKeyHash -> (CurrencySymbol, TokenName) -> Integer
txPaidTo info pkh asset =
    let outs = [ txOutValue o | o <- txInfoOutputs info, txOutAddress o == pubKeyHashAddress pkh ]
    in foldr (\v acc -> acc + valueOfAsset v asset) 0 outs

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: RouterDatum -> RouterAction -> ScriptContext -> Bool
mkValidator _dat act ctx =
    let info = scriptContextTxInfo ctx
        hops = routeHops act
        recipient = routeRecipient act
        minOut = routeMinOut act
    in traceIfFalse "no hops provided" (length hops > 0) &&
       traceIfFalse "recipient not receiving enough output" (recipientGetsMin info recipient hops minOut)

{-# INLINABLE recipientGetsMin #-}
recipientGetsMin :: TxInfo -> PubKeyHash -> [Hop] -> Integer -> Bool
recipientGetsMin info recipient hops minOut =
    let totalOut = foldr (\hop acc -> acc + txPaidTo info recipient (hopOut hop)) 0 hops
    in totalOut >= minOut

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @RouterDatum d
        red = unsafeFromBuiltinData @RouterAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Address
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
        shelleyAddr =
            C.makeShelleyAddressInEra
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
    writeValidator "multiHopRouter.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Multi-hop Router Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------------"
    putStrLn "Multi-hop Router validator generated successfully."
