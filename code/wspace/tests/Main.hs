-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

-- module Main where

-- -- Plutus imports
-- import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode, unstableMakeIsData)
-- import PlutusTx.Prelude hiding (($), (<>)) -- hide conflicting operators
-- import Plutus.V2.Ledger.Contexts (txSignedBy, ScriptContext (..), TxInfo (..))
-- import Plutus.V2.Ledger.Api (BuiltinData, Validator, mkValidatorScript, POSIXTime, PubKeyHash)
-- import Plutus.V1.Ledger.Interval (contains, to, from)

-- -- Standard Haskell imports
-- import Prelude (IO, Show, print, putStrLn, FilePath, ($), (<>))
-- import GHC.Generics (Generic)

-- -- File and serialization imports
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import Codec.Serialise (serialise)
-- import Cardano.Api
--     ( writeFileTextEnvelope
--     , displayError
--     , PlutusScript (..)
--     , PlutusScriptV2
--     )
-- import Cardano.Api.Shelley (PlutusScript (..))

-- -- ===========================
-- -- | Custom On-chain Types   |
-- -- ===========================

-- data CampaignDatum = CampaignDatum
--     { beneficiary  :: PubKeyHash
--     , deadline     :: POSIXTime
--     , goal         :: Integer
--     , totalDonated :: Integer
--     } deriving (Show, Generic)

-- data CampaignActions = Donate | Withdraw | Refund
--     deriving (Show, Generic)

-- PlutusTx.unstableMakeIsData ''CampaignDatum -- converts data to bytes format (0 and 1s) that can be stored and transmitted -this because cardano doesnt understand haskell data structure it only understands binary data 
-- PlutusTx.unstableMakeIsData ''CampaignActions

-- -- ===========================
-- -- | Validator Logic         |
-- -- ===========================

-- {-# INLINABLE mkCampaignValidator #-}
-- mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- mkCampaignValidator dat red ctx =
--     case red of
--         Donate   -> traceIfFalse "Too late to donate" canDonate -- if canDonate is not true msg else run canDonate
--         Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw
--         Refund   -> traceIfFalse "Cannot Refund" canRefund
--   where
--     info :: TxInfo -- TxInfo is a detailed record that contains all relevant information about the transaction
--     info = scriptContextTxInfo ctx --Retrieves scriptContextTxInfo from scriptt context (ctx) which returns TxInfo 

--     canDonate :: Bool -- is the transaction time with the deadline
--     canDonate = to (deadline dat) `contains` txInfoValidRange info

--     canWithdraw :: Bool
--     canWithdraw =
--         txSignedBy info (beneficiary dat) && --Check if the transaction was signed by the beneficiary specified in the datum.
--         from (deadline dat) `contains` txInfoValidRange info

--     canRefund :: Bool
--     canRefund = from (deadline dat) `contains` txInfoValidRange info &&
--         totalDonated dat < goal dat
-- -- ===========================
-- -- | Wrapper & Compilation   |
-- -- ===========================

-- {-# INLINABLE wrap #-}
-- wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrap d r c =
--     let dat = unsafeFromBuiltinData d
--         red = unsafeFromBuiltinData r
--         ctx = unsafeFromBuiltinData c
--     in if mkCampaignValidator dat red ctx then () else error ()

-- compiledCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- compiledCode = $$(PlutusTx.compile [|| wrap ||])

-- validator :: Validator
-- validator = mkValidatorScript compiledCode

-- -- ===========================
-- -- | Write Validator to File |
-- -- ===========================

-- writeValidator :: FilePath -> Validator -> IO ()
-- writeValidator file validator = do
--     let scriptSerialised = serialise validator
--         scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
--         script = PlutusScriptSerialised scriptShort :: PlutusScript PlutusScriptV2
--     result <- writeFileTextEnvelope file Nothing script
--     case result of
--         Left err -> print (displayError err)
--         Right () -> putStrLn ("Wrote validator to: " <> file)

-- -- ===========================
-- -- | Main                    |
-- -- ===========================

-- main :: IO ()
-- main = do
--     putStrLn "Validator compiled successfully!"
--     writeValidator "campaign-validator.plutus" validator

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


-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

-- module Main where

-- -- Plutus imports
-- import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode, unstableMakeIsData)
-- import PlutusTx.Prelude hiding (($), (<>))  -- hide conflicting operators
-- import Plutus.V2.Ledger.Contexts (txSignedBy, ScriptContext(..), TxInfo(..))
-- import Plutus.V2.Ledger.Api (BuiltinData, Validator, mkValidatorScript, POSIXTime, PubKeyHash)
-- import Plutus.V1.Ledger.Interval (contains, to, from)

-- -- Standard Haskell imports
-- import Prelude (IO, Show, print, putStrLn, FilePath, ($), (<>))
-- import GHC.Generics (Generic)

-- -- File and serialization imports
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import Codec.Serialise (serialise)
-- import Cardano.Api
--     ( writeFileTextEnvelope
--     , displayError
--     , PlutusScript (..)
--     , PlutusScriptV2
--     )
-- import Cardano.Api.Shelley (PlutusScript (..))

-- -- ===========================
-- -- | Minimum donation (lovelace)
-- -- ===========================
-- {-# INLINABLE minDonation #-}
-- minDonation :: Integer
-- minDonation = 1000000  -- 1 ADA

-- -- Placeholder for computing donation amounts sent to this script
-- {-# INLINABLE donationAmount #-}
-- donationAmount :: ScriptContext -> Integer
-- donationAmount _ctx = minDonation
-- -- TODO: replace with actual sum of inputs/outputs going to this validator

-- -- ===========================
-- -- | Custom On-chain Types
-- -- ===========================

-- data CampaignDatum = CampaignDatum
--     { beneficiary  :: PubKeyHash
--     , deadline     :: POSIXTime
--     , goal         :: Integer
--     , totalDonated :: Integer
--     } deriving (Show, Generic)

-- data CampaignActions = Donate | Withdraw | Refund
--     deriving (Show, Generic)

-- PlutusTx.unstableMakeIsData ''CampaignDatum
-- PlutusTx.unstableMakeIsData ''CampaignActions

-- -- ===========================
-- -- | Validator Logic
-- -- ===========================

-- {-# INLINABLE mkCampaignValidator #-}
-- mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- mkCampaignValidator dat red ctx =
--     case red of
--         Donate   -> traceIfFalse "Too late to donate" canDonate
--         Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw
--         Refund   -> traceIfFalse "Cannot refund" canRefund
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     -- Can donate if within deadline and amount >= minDonation
--     canDonate :: Bool
--     canDonate = to (deadline dat) `contains` txInfoValidRange info &&
--                 donationAmount ctx >= minDonation

--     -- Can withdraw only if signed by beneficiary after deadline
--     canWithdraw :: Bool
--     canWithdraw =
--         txSignedBy info (beneficiary dat) &&
--         from (deadline dat) `contains` txInfoValidRange info

--     -- Can refund if deadline passed but goal not reached
--     canRefund :: Bool
--     canRefund = from (deadline dat) `contains` txInfoValidRange info &&
--                 totalDonated dat < goal dat

-- -- ===========================
-- -- | Wrapper & Compilation
-- -- ===========================

-- {-# INLINABLE wrap #-}
-- wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrap d r c =
--     let dat = unsafeFromBuiltinData d
--         red = unsafeFromBuiltinData r
--         ctx = unsafeFromBuiltinData c
--     in if mkCampaignValidator dat red ctx then () else error ()

-- compiledCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- compiledCode = $$(PlutusTx.compile [|| wrap ||])

-- validator :: Validator
-- validator = mkValidatorScript compiledCode

-- -- ===========================
-- -- | Write Validator to File
-- -- ===========================

-- writeValidator :: FilePath -> Validator -> IO ()
-- writeValidator file validator = do
--     let scriptSerialised = serialise validator
--         scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
--         script = PlutusScriptSerialised scriptShort :: PlutusScript PlutusScriptV2
--     result <- writeFileTextEnvelope file Nothing script
--     case result of
--         Left err -> print (displayError err)
--         Right () -> putStrLn ("Wrote validator to: " <> file)

-- -- ===========================
-- -- | Main
-- -- ===========================

-- main :: IO ()
-- main = do
--     putStrLn "Validator compiled successfully!"
--     writeValidator "campaign-validator.plutus" validator

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

-- module Main where

-- import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode, unstableMakeIsData)
-- import PlutusTx.Prelude hiding (($), (<>))
-- import Plutus.V2.Ledger.Contexts 
--     ( txSignedBy
--     , ScriptContext(..)
--     , TxInfo(..)
--     , TxOut(..)
--     , TxInInfo(..)
--     , scriptContextPurpose
--     , txInInfoResolved
--     , ScriptPurpose(..)
--     , TxOutRef(..)
--     )
-- import Plutus.V2.Ledger.Api 
--     ( BuiltinData
--     , Validator
--     , mkValidatorScript
--     , POSIXTime
--     , PubKeyHash
--     , ValidatorHash
--     , Address(..)
--     , Credential(..)
--     )
-- import Plutus.V1.Ledger.Interval (contains, to, from)
-- import Plutus.V1.Ledger.Value (Value, flattenValue, adaSymbol, adaToken)

-- import Prelude (IO, Show, print, putStrLn, FilePath, ($), (<>))
-- import GHC.Generics (Generic)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import Codec.Serialise (serialise)
-- import Cardano.Api
--     ( writeFileTextEnvelope
--     , displayError
--     , PlutusScriptV2
--     )
-- import Cardano.Api.Shelley (PlutusScript (..))
-- import qualified Data.List as L   -- <- qualified import to fix ambiguity

-- -- ===========================
-- -- | Donation parameters     |
-- -- ===========================

-- {-# INLINABLE minDonation #-}
-- minDonation :: Integer
-- minDonation = 1000000  -- 1 ADA in lovelace

-- {-# INLINABLE getLovelace #-}
-- getLovelace :: Value -> Integer
-- getLovelace v =
--     foldl (\acc (cs, tn, amt) -> if cs == adaSymbol && tn == adaToken then acc + amt else acc) 0 (flattenValue v)

-- {-# INLINABLE addrValidatorHash #-}
-- addrValidatorHash :: Address -> Maybe ValidatorHash
-- addrValidatorHash (Address (ScriptCredential vh) _) = Just vh
-- addrValidatorHash _                                 = Nothing

-- {-# INLINABLE ownValidatorHash #-}
-- ownValidatorHash :: ScriptContext -> ValidatorHash
-- ownValidatorHash ctx = 
--     case scriptContextPurpose ctx of
--         Spending txOutRef ->
--             let info = scriptContextTxInfo ctx
--                 mInput = L.find (\i -> txInInfoOutRef i == txOutRef) (txInfoInputs info)
--             in case mInput of
--                 Just txIn -> case addrValidatorHash (txOutAddress $ txInInfoResolved txIn) of
--                                Just h  -> h
--                                Nothing -> traceError "Invalid script input"
--                 Nothing -> traceError "Input not found"
--         _ -> traceError "Not spending from script"

-- {-# INLINABLE donationAmount #-}
-- donationAmount :: ScriptContext -> Integer
-- donationAmount ctx =
--     let
--         info :: TxInfo
--         info = scriptContextTxInfo ctx
--         ownHash = ownValidatorHash ctx

--         outputsToScript :: [Value]
--         outputsToScript = [ txOutValue o | o <- txInfoOutputs info
--                                          , Just h <- [addrValidatorHash $ txOutAddress o]
--                                          , h == ownHash ]
--     in sum (map getLovelace outputsToScript)

-- -- ===========================
-- -- | Custom On-chain Types   |
-- -- ===========================

-- data CampaignDatum = CampaignDatum
--     { beneficiary  :: PubKeyHash
--     , deadline     :: POSIXTime
--     , goal         :: Integer
--     , totalDonated :: Integer
--     } deriving (Show, Generic)

-- data CampaignActions = Donate | Withdraw | Refund
--     deriving (Show, Generic)

-- PlutusTx.unstableMakeIsData ''CampaignDatum
-- PlutusTx.unstableMakeIsData ''CampaignActions

-- -- ===========================
-- -- | Validator Logic         |
-- -- ===========================

-- {-# INLINABLE mkCampaignValidator #-}
-- mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- mkCampaignValidator dat red ctx =
--     case red of
--         Donate   -> traceIfFalse "Too late to donate or amount too low" canDonate
--         Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw
--         Refund   -> traceIfFalse "Cannot refund" canRefund
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     canDonate :: Bool
--     canDonate = to (deadline dat) `contains` txInfoValidRange info &&
--                 donationAmount ctx >= minDonation

--     canWithdraw :: Bool
--     canWithdraw = txSignedBy info (beneficiary dat) &&
--                   from (deadline dat) `contains` txInfoValidRange info

--     canRefund :: Bool
--     canRefund = from (deadline dat) `contains` txInfoValidRange info &&
--                 totalDonated dat < goal dat

-- {-# INLINABLE wrap #-}
-- wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrap d r c =
--     let dat = unsafeFromBuiltinData d
--         red = unsafeFromBuiltinData r
--         ctx = unsafeFromBuiltinData c
--     in if mkCampaignValidator dat red ctx then () else error ()

-- compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- compiledCode = $$(compile [|| wrap ||])

-- validator :: Validator
-- validator = mkValidatorScript compiledCode

-- -- ===========================
-- -- | Write Validator to File |
-- -- ===========================

-- writeValidator :: FilePath -> Validator -> IO ()
-- writeValidator file validator = do
--     let scriptSerialised = serialise validator
--         scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
--         script :: PlutusScript PlutusScriptV2
--         script = PlutusScriptSerialised scriptShort
--     result <- writeFileTextEnvelope file Nothing script
--     case result of
--         Left err -> print (displayError err)
--         Right () -> putStrLn ("Wrote validator to: " <> file)

-- -- ===========================
-- -- | Main                    |
-- -- ===========================

-- main :: IO ()
-- main = do
--     putStrLn "Validator compiled successfully!"
--     writeValidator "campaign-validator.plutus" validator

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Main where

import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode, unstableMakeIsData)
import PlutusTx.Prelude hiding (($), (<>))
import Plutus.V2.Ledger.Contexts 
    ( txSignedBy
    , ScriptContext(..)
    , TxInfo(..)
    , TxOut(..)
    , TxInInfo(..)
    , scriptContextPurpose
    , txInInfoResolved
    , ScriptPurpose(..)
    , TxOutRef(..)
    , findDatum
    )
import Plutus.V2.Ledger.Api 
    ( BuiltinData
    , Validator
    , mkValidatorScript
    , POSIXTime
    , PubKeyHash
    , ValidatorHash
    , Address(..)
    , Credential(..)
    , Datum(..)
    , DatumHash
    , OutputDatum(..)
    )
import Plutus.V1.Ledger.Interval (contains, to, from)
import Plutus.V1.Ledger.Value (Value, flattenValue, adaSymbol, adaToken)

import Prelude (IO, Show, print, putStrLn, FilePath, ($), (<>))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api
    ( writeFileTextEnvelope
    , displayError
    , PlutusScriptV2
    )
import Cardano.Api.Shelley (PlutusScript (..))
import qualified Data.List as L

-- ===========================
-- | Donation parameters     |
-- ===========================

{-# INLINABLE minDonation #-}
minDonation :: Integer
minDonation = 1000000  -- 1 ADA in lovelace

{-# INLINABLE getLovelace #-}
getLovelace :: Value -> Integer
getLovelace v =
    foldl (\acc (cs, tn, amt) -> if cs == adaSymbol && tn == adaToken then acc + amt else acc) 0 (flattenValue v)

{-# INLINABLE addrValidatorHash #-}
addrValidatorHash :: Address -> Maybe ValidatorHash
addrValidatorHash (Address (ScriptCredential vh) _) = Just vh
addrValidatorHash _                                 = Nothing

{-# INLINABLE ownValidatorHash #-}
ownValidatorHash :: ScriptContext -> ValidatorHash
ownValidatorHash ctx = 
    case scriptContextPurpose ctx of
        Spending txOutRef ->
            let info = scriptContextTxInfo ctx
                mInput = L.find (\i -> txInInfoOutRef i == txOutRef) (txInfoInputs info)
            in case mInput of
                Just txIn -> case addrValidatorHash (txOutAddress $ txInInfoResolved txIn) of
                               Just h  -> h
                               Nothing -> traceError "Invalid script input"
                Nothing -> traceError "Input not found"
        _ -> traceError "Not spending from script"

{-# INLINABLE donationAmount #-}
donationAmount :: ScriptContext -> Integer
donationAmount ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx
        ownHash = ownValidatorHash ctx

        outputsToScript :: [Value]
        outputsToScript = [ txOutValue o | o <- txInfoOutputs info
                                         , Just h <- [addrValidatorHash $ txOutAddress o]
                                         , h == ownHash ]
    in sum (map getLovelace outputsToScript)

-- ===========================
-- | Custom On-chain Types   |
-- ===========================

data CampaignDatum = CampaignDatum
    { beneficiary  :: PubKeyHash
    , deadline     :: POSIXTime
    , goal         :: Integer
    , totalDonated :: Integer
    } deriving (Show, Generic)

data CampaignActions = Donate | Withdraw | Refund
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''CampaignDatum
PlutusTx.unstableMakeIsData ''CampaignActions

-- ===========================
-- | On-chain equality       |
-- ===========================

{-# INLINABLE eqCampaignDatum #-}
eqCampaignDatum :: CampaignDatum -> CampaignDatum -> Bool
eqCampaignDatum d1 d2 =
    beneficiary d1 == beneficiary d2 &&
    deadline d1    == deadline d2 &&
    goal d1        == goal d2 &&
    totalDonated d1 == totalDonated d2

-- ===========================
-- | Validator Logic         |
-- ===========================

{-# INLINABLE mkCampaignValidator #-}
mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
mkCampaignValidator dat red ctx =
    case red of
        Donate   -> traceIfFalse "Too late to donate or amount too low" canDonate &&
                    traceIfFalse "Output datum not updated correctly" datumUpdated
        Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw
        Refund   -> traceIfFalse "Cannot refund" canRefund
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    ownHash = ownValidatorHash ctx

    -- Donation logic
    canDonate :: Bool
    canDonate = to (deadline dat) `contains` txInfoValidRange info &&
                donationAmount ctx >= minDonation

    updatedDatum :: CampaignDatum
    updatedDatum = dat { totalDonated = totalDonated dat + donationAmount ctx }

    datumUpdated :: Bool
    datumUpdated =
        let outputs = [ o | o <- txInfoOutputs info
                          , Just h <- [addrValidatorHash $ txOutAddress o]
                          , h == ownHash ]
        in case outputs of
            [o] -> case txOutDatum o of
                     OutputDatumHash dh -> case findDatum dh info of
                                              Just (Datum d') -> eqCampaignDatum (unsafeFromBuiltinData d') updatedDatum
                                              Nothing         -> False
                     _ -> False
            _   -> False

    -- Withdraw logic
    canWithdraw :: Bool
    canWithdraw = txSignedBy info (beneficiary dat) &&
                  from (deadline dat) `contains` txInfoValidRange info &&
                  totalDonated dat >= goal dat

    -- Refund logic
    canRefund :: Bool
    canRefund = from (deadline dat) `contains` txInfoValidRange info &&
                totalDonated dat < goal dat

{-# INLINABLE wrap #-}
wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrap d r c =
    let dat = unsafeFromBuiltinData d
        red = unsafeFromBuiltinData r
        ctx = unsafeFromBuiltinData c
    in if mkCampaignValidator dat red ctx then () else error ()

compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode = $$(compile [|| wrap ||])

validator :: Validator
validator = mkValidatorScript compiledCode

-- ===========================
-- | Write Validator to File |
-- ===========================

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file validator = do
    let scriptSerialised = serialise validator
        scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
        script :: PlutusScript PlutusScriptV2
        script = PlutusScriptSerialised scriptShort
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
