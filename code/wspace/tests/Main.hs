-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}

-- module Main where

-- import PlutusTx
-- import PlutusTx.Prelude (traceIfFalse, BuiltinString, Bool(..), ($), Integer, (&&))
-- import Plutus.V2.Ledger.Api 
--     ( Validator
--     , mkValidatorScript
--     , PubKeyHash
--     , POSIXTime
--     , ScriptContext
--     , TxInfo
--     , txInfoValidRange
--     )      
-- import Plutus.V2.Ledger.Contexts ( txSignedBy)   
-- import Prelude (Show, IO, putStrLn)
-- import Plutus.V1.Ledger.Interval (contains, to, from)
-- import PlutusTx.Builtins (toBuiltin)

-- -- Every doner gets NFTs/benefits based on their level of donation

-- --  Compaign Donation for specified beneficiaries
-- data CampaignDatum = CampaignDatum {
--     beneficiary :: PubKeyHash,
--     goal        :: Integer,
--     deadline    :: POSIXTime
-- } deriving (Show)

-- PlutusTx.unstableMakeIsData ''CampaignDatum -- converts data to bytes format (0 and 1s) that can be stored and transmitted -this because cardano doesnt understand haskell data structure it only understands binary data 

-- data CampaignActions = Donate | Withdraw | Refund 
--     deriving (Show)

-- PlutusTx.unstableMakeIsData ''CampaignActions -- converts data to bytes format (0 and 1s)

-- mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- mkCampaignValidator dat red ctx = 
--     case red of
--         Donate   -> traceIfFalse (toBuiltin "Too late to donate") canDonate -- if canDonate is not true msg else run canDonate
--         Withdraw -> traceIfFalse (toBuiltin "Not allowed to withdraw") canWithdraw
--         Refund   -> traceIfFalse "Refund fail" refundCheck

--         where 
--             info :: TxInfo -- TxInfo is a detailed record that contains all relevant information about the transaction
--             info = scriptContextTxInfo ctx --Retrieves scriptContextTxInfo from scriptt context (ctx) which returns TxInfo 

--             canDonate :: Bool  -- - is the transaction time with the deadline
--             canDonate = to (deadline dat) `contains` (txInfoValidRange info) -- info provides  txInfoValidRange

--             canWithdraw :: Bool
--             canWithdraw = 
--                 txSignedBy info (beneficiary dat) && --Check if the transaction was signed by the beneficiary specified in the datum.
--                 from (deadline dat) `contains` txInfoValidRange info

--             refundCheck :: Bool
--             refundCheck = True 

-- wrap :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- wrap = mkCampaignValidator

-- validator :: Validator
-- validator = mkValidatorScript $$(PlutusTx.compile [|| wrap ||])



-- -- {-# INLINABLE traceIfFalse #-}
-- -- traceIfFalse :: BuiltinString -> Bool -> Bool
-- -- traceIfFalse msg condition =
-- --     if condition
-- --         then True
-- --         else trace msg False

-- -- data ScriptContext = ScriptContext
-- --     { scriptContextTxInfo :: TxInfo
-- --     , scriptContextPurpose :: ScriptPurpose
-- --     }

-- -- data TxInfo = TxInfo
-- --     { txInfoInputs      :: [TxInInfo]
-- --     , txInfoOutputs     :: [TxOut]
-- --     , txInfoFee         :: Value
-- --     , txInfoMint        :: Value
-- --     , txInfoDCert       :: [DCert]
-- --     , txInfoWdrl        :: Map StakingCredential Integer
-- --     , txInfoValidRange  :: POSIXTimeRange
-- --     , txInfoSignatories :: [PubKeyHash]
-- --     , txInfoData        :: [(DatumHash, Datum)]
-- --     , txInfoId          :: TxId
-- --     }

-- -- to =>  Creates an interval from -∞ to an upper bound
-- -- contain => Checks if one interval is within another
-- -- contains :: Interval a -> Interval a -> Bool

-- {--
-- txSignedBy :: TxInfo -> PubKeyHash -> Bool
-- checks if a transaction is signed by a given key


-- --}

-- main :: IO ()
-- main = putStrLn "Campaign validator compiled successfully!"



{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Main where

-- Plutus imports
import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode, unstableMakeIsData)
import PlutusTx.Prelude hiding (($), (<>)) -- hide conflicting operators
import Plutus.V2.Ledger.Contexts (txSignedBy, ScriptContext (..), TxInfo (..))
import Plutus.V2.Ledger.Api (BuiltinData, Validator, mkValidatorScript, POSIXTime, PubKeyHash)
import Plutus.V1.Ledger.Interval (contains, to, from)

-- Standard Haskell imports
import Prelude (IO, Show, print, putStrLn, FilePath, ($), (<>))
import GHC.Generics (Generic)

-- File and serialization imports
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api
    ( writeFileTextEnvelope
    , displayError
    , PlutusScript (..)
    , PlutusScriptV2
    )
import Cardano.Api.Shelley (PlutusScript (..))

-- ===========================
-- | Custom On-chain Types   |
-- ===========================

data CampaignDatum = CampaignDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving (Show, Generic)

data CampaignActions = Donate | Withdraw
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''CampaignDatum
PlutusTx.unstableMakeIsData ''CampaignActions

-- ===========================
-- | Validator Logic         |
-- ===========================

{-# INLINABLE mkCampaignValidator #-}
mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
mkCampaignValidator dat red ctx =
    case red of
        Donate   -> traceIfFalse "Too late to donate" canDonate
        Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    canDonate :: Bool
    canDonate = to (deadline dat) `contains` txInfoValidRange info

    canWithdraw :: Bool
    canWithdraw =
        txSignedBy info (beneficiary dat) &&
        from (deadline dat) `contains` txInfoValidRange info

-- ===========================
-- | Wrapper & Compilation   |
-- ===========================

{-# INLINABLE wrap #-}
wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrap d r c =
    let dat = unsafeFromBuiltinData d
        red = unsafeFromBuiltinData r
        ctx = unsafeFromBuiltinData c
    in if mkCampaignValidator dat red ctx then () else error ()

compiledCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode = $$(PlutusTx.compile [|| wrap ||])

validator :: Validator
validator = mkValidatorScript compiledCode

-- ===========================
-- | Write Validator to File |
-- ===========================

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file validator = do
    let scriptSerialised = serialise validator
        scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
        script = PlutusScriptSerialised scriptShort :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope file Nothing script
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote validator to: " <> file)

-- ===========================
-- | Main                    |
-- ===========================

main :: IO ()
main = do
    putStrLn "Validator compiled successfully!"
    writeValidator "campaign-validator.plutus" validator
