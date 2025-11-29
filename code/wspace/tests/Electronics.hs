{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Electronics  where

import           PlutusTx
import           PlutusTx.Prelude        hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api    (BuiltinData, ScriptContext (..),
                                          Validator, mkValidatorScript,
                                          TxOut (..), TxInfo (..), Value, adaSymbol, adaToken)
import qualified Plutus.V2.Ledger.Api    as V2
import qualified Plutus.V1.Ledger.Value  as V1
import           Prelude                 (IO, putStrLn, show)

import           Cardano.Api             (writeFileTextEnvelope)
import           Cardano.Api.Shelley     (PlutusScript(..), PlutusScriptV2)
import qualified Codec.Serialise         as C
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as SBS

--------------------------------------------------------------------------------
-- DATA TYPES
--------------------------------------------------------------------------------

data RMAState = Pending | Testing | Approved | Rejected
PlutusTx.unstableMakeIsData ''RMAState

data RMADatum = RMADatum
    { rCustomer :: V2.PubKeyHash
    , rDeviceId :: BuiltinByteString
    , rDeposit  :: Integer
    , rState    :: RMAState
    , rExpiry   :: Integer           -- warranty expiry timestamp
    , rProof    :: BuiltinByteString -- optional proof-of-test
    }
PlutusTx.unstableMakeIsData ''RMADatum

data RMAAction = StartTest | Approve | Reject
PlutusTx.unstableMakeIsData ''RMAAction

--------------------------------------------------------------------------------
-- VALIDATORS
--------------------------------------------------------------------------------

{-# INLINABLE validWarranty #-}
validWarranty :: RMADatum -> Integer -> Bool
validWarranty datum currentTime = currentTime <= rExpiry datum

{-# INLINABLE validTransition #-}
validTransition :: RMAState -> RMAAction -> Bool
validTransition Pending    StartTest = True
validTransition Testing    Approve   = True
validTransition Testing    Reject    = True
validTransition _          _         = False

{-# INLINABLE depositCorrect #-}
depositCorrect :: RMADatum -> Value -> Bool
depositCorrect datum val =
    let adaHeld = V1.valueOf val adaSymbol adaToken
    in adaHeld >= rDeposit datum

{-# INLINABLE getContinuingOutputs #-}
getContinuingOutputs :: ScriptContext -> [V2.TxOut]
getContinuingOutputs ctx = V2.txInfoOutputs $ V2.scriptContextTxInfo ctx

{-# INLINABLE mkWarrantyRMAValidator #-}
mkWarrantyRMAValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWarrantyRMAValidator dat red ctx =
    let
        datum      = unsafeFromBuiltinData dat  :: RMADatum
        action     = unsafeFromBuiltinData red  :: RMAAction
        context    = unsafeFromBuiltinData ctx  :: ScriptContext
        info       = V2.scriptContextTxInfo context
        currentTime = case V2.ivTo (V2.txInfoValidRange info) of
                        V2.UpperBound (V2.Finite (V2.POSIXTime t)) _ -> t
                        _ -> 0
        outputVal = case getContinuingOutputs context of
                      [o] -> V2.txOutValue o
                      _   -> mempty  -- empty Value instead of 0
    in
    if validTransition (rState datum) action
       && validWarranty datum currentTime
       && depositCorrect datum outputVal
       then ()
       else error ()

--------------------------------------------------------------------------------
-- COMPILE VALIDATOR
--------------------------------------------------------------------------------

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkWarrantyRMAValidator ||])

plutusScript :: PlutusScript PlutusScriptV2
plutusScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ C.serialise validator

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    result <- writeFileTextEnvelope "warrantyRMA.plutus" Nothing plutusScript
    case result of
        Left err -> putStrLn $ "Error writing file: " ++ show err
        Right () -> putStrLn "warrantyRMA.plutus created successfully."
