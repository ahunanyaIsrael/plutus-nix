{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Electronics  where

import           PlutusTx
import           PlutusTx.Prelude        hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api    (BuiltinData, ScriptContext (..),
                                          Validator, mkValidatorScript)
import qualified Plutus.V2.Ledger.Api    as V2

import           Cardano.Api             (writeFileTextEnvelope)
import           Cardano.Api.Shelley     (PlutusScript(..), PlutusScriptV2)
import qualified Codec.Serialise         as C
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as SBS
import           Prelude                 (IO, putStrLn, show)

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
    }
PlutusTx.unstableMakeIsData ''RMADatum

data RMAAction = StartTest | Approve | Reject
PlutusTx.unstableMakeIsData ''RMAAction

--------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------

{-# INLINABLE validTransition #-}
validTransition :: RMAState -> RMAAction -> Bool
validTransition Pending    StartTest = True
validTransition Testing    Approve   = True
validTransition Testing    Reject    = True
validTransition _          _         = False

{-# INLINABLE mkWarrantyRMAValidator #-}
mkWarrantyRMAValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWarrantyRMAValidator dat red ctx =
    let
        datum  = unsafeFromBuiltinData dat  :: RMADatum
        action = unsafeFromBuiltinData red  :: RMAAction
    in
    if validTransition (rState datum) action
       then ()
       else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkWarrantyRMAValidator ||])

--------------------------------------------------------------------------------
-- SERIALIZE TO .PLUTUS
--------------------------------------------------------------------------------

plutusScript :: PlutusScript PlutusScriptV2
plutusScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ C.serialise validator

main :: IO ()
main = do
    result <- writeFileTextEnvelope "warrantyRMA.plutus" Nothing plutusScript
    case result of
        Left err -> putStrLn $ "Error writing file: " ++ show err
        Right () -> putStrLn "warrantyRMA.plutus created successfully."
