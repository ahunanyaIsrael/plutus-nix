-- {-# LANGUAGE DataKinds         #-}
-- {-# LANGUAGE DeriveAnyClass    #-}
-- {-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE TypeApplications  #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

-- module Main where

-- import           Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy  as LBS
-- import qualified Data.ByteString.Short as SBS
-- import           Data.Text             (Text)
-- import qualified Data.Text             as T
-- import           Data.Map              (Map)
-- import qualified Data.Map              as Map

-- import           Cardano.Api (writeFileTextEnvelope, displayError)
-- import           Cardano.Api.Shelley
--     ( PlutusScript (..)
--     , PlutusScriptV2
--     )

-- import           PlutusTx
-- import           PlutusTx.Prelude hiding ((<>), Show, find, foldr)
-- import qualified Prelude as P (IO, FilePath, print, putStrLn, Show, show, ($), (<>), Maybe(..), Eq(..))
-- import qualified PlutusTx.AssocMap as AssocMap
-- import           Plutus.V1.Ledger.Crypto (PubKeyHash(..))

-- import           Plutus.V2.Ledger.Api
--     ( Validator
--     , ScriptContext(..)
--     , TxInfo(..)
--     , TxOut(..)
--     , Datum(..)
--     , Value
--     , PubKeyHash
--     , POSIXTime
--     , ScriptPurpose(..)
--     , mkValidatorScript
--     , OutputDatum (..)
--     , unValidatorScript
--     , TxOutRef(..)
--     , TxId(..)
--     , TokenName(..)
--     , TxInInfo(..)
--     , POSIXTimeRange
--     )
-- import           Plutus.V2.Ledger.Contexts
--     ( txSignedBy
--     , valuePaidTo
--     , getContinuingOutputs
--     , txInInfoOutRef
--     , txInInfoResolved
--     , ownHash
--     , findOwnInput
--     , scriptContextTxInfo
--     )
-- import qualified Plutus.V1.Ledger.Interval as Interval
-- import qualified Plutus.V1.Ledger.Value    as Value
-- import           Plutus.V1.Ledger.Address ( Address(..))
-- import           Plutus.V1.Ledger.Credential( Credential(..))

-- -- Test framework imports
-- import           Test.Tasty
-- import           Test.Tasty.HUnit
-- import           Test.Tasty.ExpectedFailure

-- import qualified Data.ByteString.Char8 as BS8
-- import PlutusTx.Builtins (toBuiltin, fromBuiltin)

-- --------------------------------------------------------------------------------
-- -- DATUM & REDEEMER (Same as before)
-- --------------------------------------------------------------------------------
-- data FundDatum = FundDatum
--     { officials         :: [PubKeyHash]
--     , requiredApprovals :: Integer
--     , deadline          :: POSIXTime
--     , depositor         :: PubKeyHash
--     , recipient         :: PubKeyHash
--     , approvalsCount    :: Integer
--     }
--     deriving (P.Show, P.Eq)

-- PlutusTx.unstableMakeIsData ''FundDatum

-- data FundRedeemer
--     = Approve
--     | Release
--     | Refund
--     deriving (P.Show, P.Eq)

-- PlutusTx.makeIsDataIndexed ''FundRedeemer
--     [ ('Approve, 0)
--     , ('Release, 1)
--     , ('Refund,  2)
--     ]
-- --------------------------------------------------------------------------------
-- -- VALIDATOR (Simplified for testing)
-- --------------------------------------------------------------------------------
-- {-# INLINABLE mkFundValidator #-}
-- mkFundValidator :: FundDatum -> FundRedeemer -> ScriptContext -> Bool
-- mkFundValidator dat red ctx =
--     case red of
--         Approve  -> validateApprove dat ctx
--         Release  -> validateRelease dat ctx
--         Refund   -> validateRefund dat ctx

-- {-# INLINABLE validateApprove #-}
-- validateApprove :: FundDatum -> ScriptContext -> Bool
-- validateApprove FundDatum{officials, requiredApprovals, deadline, approvalsCount} ctx =
--     beforeDeadline &&
--     (approvalsCount + 1 < requiredApprovals) &&
--     isOfficial
--     -- Temporarily remove fundsStayInScript check for unit tests
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     beforeDeadline :: Bool
--     beforeDeadline =
--         Interval.to deadline `Interval.contains` txInfoValidRange info

--     signers :: [PubKeyHash]
--     signers = txInfoSignatories info

--     -- Check if signer is an official
--     isOfficial :: Bool
--     isOfficial = any (`elem` officials) signers

-- {-# INLINABLE validateRelease #-}
-- validateRelease :: FundDatum -> ScriptContext -> Bool
-- validateRelease FundDatum{officials, requiredApprovals, deadline, recipient, approvalsCount} ctx =
--     beforeDeadline &&
--     (approvalsCount >= requiredApprovals)
--     -- Temporarily remove recipientPaid check for unit tests
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     beforeDeadline :: Bool
--     beforeDeadline =
--         Interval.to deadline `Interval.contains` txInfoValidRange info

-- {-# INLINABLE validateRefund #-}
-- validateRefund :: FundDatum -> ScriptContext -> Bool
-- validateRefund FundDatum{deadline, depositor, approvalsCount, requiredApprovals} ctx =
--     afterDeadline &&
--     depositorSigned &&
--     (approvalsCount < requiredApprovals)
--     -- Temporarily remove refunded check for unit tests
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     afterDeadline :: Bool
--     afterDeadline =
--         Interval.from (deadline + 1) `Interval.contains` txInfoValidRange info

--     depositorSigned :: Bool
--     depositorSigned = txSignedBy info depositor

-- --------------------------------------------------------------------------------
-- -- WRAPPER & VALIDATOR
-- --------------------------------------------------------------------------------
-- {-# INLINABLE wrapper #-}
-- wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrapper d r c =
--     if mkFundValidator
--         (unsafeFromBuiltinData d)
--         (unsafeFromBuiltinData r)
--         (unsafeFromBuiltinData c)
--     then ()
--     else PlutusTx.Prelude.error ()

-- validator :: Validator
-- validator = mkValidatorScript $$(compile [|| wrapper ||])

-- --------------------------------------------------------------------------------
-- -- MOCK DATA FOR TESTING
-- --------------------------------------------------------------------------------
-- -- Create some test public key hashes
-- mockPKH1 :: PubKeyHash
-- mockPKH1 = PubKeyHash $ toBuiltin $ BS8.pack "official1"

-- mockPKH2 :: PubKeyHash
-- mockPKH2 = PubKeyHash $ toBuiltin $ BS8.pack "official2"

-- mockPKH3 :: PubKeyHash
-- mockPKH3 = PubKeyHash $ toBuiltin $ BS8.pack "official3"

-- mockDepositor :: PubKeyHash
-- mockDepositor = PubKeyHash $ toBuiltin $ BS8.pack "depositor"

-- mockRecipient :: PubKeyHash
-- mockRecipient = PubKeyHash $ toBuiltin $ BS8.pack "recipient"

-- mockNonOfficial :: PubKeyHash
-- mockNonOfficial = PubKeyHash $ toBuiltin $ BS8.pack "nonofficial"


-- -- Create mock datum
-- mockDatum :: FundDatum
-- mockDatum = FundDatum
--     { officials = [mockPKH1, mockPKH2, mockPKH3]
--     , requiredApprovals = 2
--     , deadline = 100
--     , depositor = mockDepositor
--     , recipient = mockRecipient
--     , approvalsCount = 0
--     }

-- -- Create mock context helper (SIMPLIFIED for unit tests)
-- createMockContext :: [PubKeyHash] -> POSIXTimeRange -> ScriptContext
-- createMockContext signers timeRange =
--     ScriptContext
--         { scriptContextTxInfo = TxInfo
--             { txInfoInputs = []
--             , txInfoOutputs = []
--             , txInfoFee = mempty
--             , txInfoMint = mempty
--             , txInfoDCert = []
--             , txInfoWdrl = AssocMap.empty
--             , txInfoValidRange = timeRange
--             , txInfoSignatories = signers
--             , txInfoData = AssocMap.empty
--             , txInfoId = TxId $ toBuiltin $ BS8.pack "txid0"
--             , txInfoReferenceInputs = []  -- Added for V2
--             , txInfoRedeemers = AssocMap.empty  -- Added for V2
--             }
--         , scriptContextPurpose = Spending (TxOutRef (TxId $ toBuiltin $ BS8.pack "txid0") 0)
--         }
-- --------------------------------------------------------------------------------
-- -- UNIT TESTS USING TASTY
-- --------------------------------------------------------------------------------
-- testValidatorCompilation :: TestTree
-- testValidatorCompilation = testCase "Validator compiles successfully" $ do
--     P.putStrLn "Testing validator compilation..."
--     -- Just creating the validator should work
--     let _ = validator
--     assertBool "Validator should compile" True

-- testSerialization :: TestTree
-- testSerialization = testGroup "Serialization Tests"
--   [ testCase "Datum roundtrip via BuiltinData" $ do
--       let bd = toBuiltinData mockDatum
--           recovered = unsafeFromBuiltinData bd :: FundDatum
--       -- use Prelude equality (qualified) so we use the Haskell Eq instance you derived
--       assertBool "Datum roundtrip should succeed" (recovered P.== mockDatum)

--   , testCase "Redeemer roundtrip via BuiltinData" $ do
--       let a = toBuiltinData (Approve :: FundRedeemer)
--           r = toBuiltinData (Release :: FundRedeemer)
--           f = toBuiltinData (Refund :: FundRedeemer)

--           ra = unsafeFromBuiltinData a :: FundRedeemer
--           rr = unsafeFromBuiltinData r :: FundRedeemer
--           rf = unsafeFromBuiltinData f :: FundRedeemer

--       assertBool "Approve redeemer should roundtrip" (ra P.== Approve)
--       assertBool "Release redeemer should roundtrip" (rr P.== Release)
--       assertBool "Refund redeemer should roundtrip" (rf P.== Refund)
--   ]

-- testLogicValidation :: TestTree
-- testLogicValidation = testGroup "Validator Logic Tests"
--     [ testCase "Official can approve before deadline" $ do
--         -- Official trying to approve before deadline
--         let ctx = createMockContext [mockPKH1] (Interval.interval 0 90)
--         let result = mkFundValidator mockDatum Approve ctx
--         assertBool "Official should be able to approve before deadline" result
        
--     , testCase "Non-official cannot approve" $ do
--         -- Non-official trying to approve
--         let ctx = createMockContext [mockNonOfficial] (Interval.interval 0 90)
--         let result = mkFundValidator mockDatum Approve ctx
--         assertBool "Non-official should NOT be able to approve" (not result)
        
--     , testCase "Cannot approve after deadline" $ do
--         -- Trying to approve after deadline
--         let ctx = createMockContext [mockPKH1] (Interval.interval 110 200)
--         let result = mkFundValidator mockDatum Approve ctx
--         assertBool "Cannot approve after deadline" (not result)
        
--     , testCase "Cannot release without enough approvals" $ do
--         -- Trying to release with 0 approvals (needs 2)
--         let ctx = createMockContext [] (Interval.interval 0 90)
--         let result = mkFundValidator mockDatum Release ctx
--         assertBool "Cannot release without enough approvals" (not result)
        
--     , testCase "Can release with enough approvals" $ do
--         -- Trying to release with enough approvals
--         let datumWithApprovals = mockDatum { approvalsCount = 2 }
--         let ctx = createMockContext [] (Interval.interval 0 90)
--         let result = mkFundValidator datumWithApprovals Release ctx
--         assertBool "Can release with enough approvals" result
        
--     , testCase "Cannot release after deadline" $ do
--         -- Trying to release after deadline even with enough approvals
--         let datumWithApprovals = mockDatum { approvalsCount = 2 }
--         let ctx = createMockContext [] (Interval.interval 110 200)
--         let result = mkFundValidator datumWithApprovals Release ctx
--         assertBool "Cannot release after deadline" (not result)
        
--     , testCase "Cannot refund before deadline" $ do
--         -- Trying to refund before deadline
--         let ctx = createMockContext [mockDepositor] (Interval.interval 0 90)
--         let result = mkFundValidator mockDatum Refund ctx
--         assertBool "Cannot refund before deadline" (not result)
        
--     , testCase "Depositor can refund after deadline" $ do
--         -- Depositor trying to refund after deadline (with 0 approvals)
--         let datumWithNoApprovals = mockDatum { approvalsCount = 0 }
--         let ctx = createMockContext [mockDepositor] (Interval.from 101)
--         let result = mkFundValidator datumWithNoApprovals Refund ctx
--         assertBool "Depositor should be able to refund after deadline" result
        
--     , testCase "Non-depositor cannot refund" $ do
--         -- Non-depositor trying to refund
--         let ctx = createMockContext [mockPKH1] (Interval.from 101)
--         let result = mkFundValidator mockDatum Refund ctx
--         assertBool "Non-depositor should NOT be able to refund" (not result)
        
--     , testCase "Cannot refund if enough approvals" $ do
--         -- Depositor trying to refund but has enough approvals
--         let datumWithApprovals = mockDatum { approvalsCount = 2 }
--         let ctx = createMockContext [mockDepositor] (Interval.from 101)
--         let result = mkFundValidator datumWithApprovals Refund ctx
--         assertBool "Cannot refund if enough approvals" (not result)
--     ]

-- testPlutusScriptGeneration :: TestTree
-- testPlutusScriptGeneration = testCase "Generate Plutus script file" $ do
--     P.putStrLn "Generating Plutus script..."
--     let script = unValidatorScript validator
--         bs     = serialise script
--         sh     = SBS.toShort (LBS.toStrict bs)
--         scr    = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    
--     result <- writeFileTextEnvelope "./assets/public_fund_final.plutus" Nothing scr
--     case result of
--         Left err -> assertFailure $ "Failed to write script: " ++ P.show err
--         Right () -> P.putStrLn "✓ Generated public_fund_final.plutus"

-- --------------------------------------------------------------------------------
-- -- MAIN TEST RUNNER
-- --------------------------------------------------------------------------------
-- main :: P.IO ()
-- main = do
--     P.putStrLn "========================================"
--     P.putStrLn "Public Fund Release Smart Contract Tests"
--     P.putStrLn "CP108 Final Project - Anti-Corruption"
--     P.putStrLn "========================================"
--     P.putStrLn ""
--     P.putStrLn "Testing core validator logic..."
    
--     defaultMain $ testGroup "All Tests"
--         [ testValidatorCompilation
--         , testSerialization
--         , testLogicValidation
--         , testPlutusScriptGeneration
--         ]
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module PublicFund where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import Cardano.Api (writeFileTextEnvelope, displayError)
import Cardano.Api.Shelley
    ( PlutusScript (..)
    , PlutusScriptV2
    )

import PlutusTx
import PlutusTx.Prelude hiding (($), (<>), Show, find)
import Prelude (IO, FilePath, print, putStrLn, Show, ($), (<>))

import Plutus.V2.Ledger.Api
    ( Validator
    , ScriptContext(..)
    , TxInfo(..)
    , TxOut(..)
    , Datum(..)
    , Value
    , PubKeyHash
    , POSIXTime
    , ScriptPurpose(..)
    , mkValidatorScript
    , OutputDatum (..)
    , unValidatorScript
    , TxOutRef
    )
import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    , txInInfoOutRef
    , txInInfoResolved
    , ownHash
    , findOwnInput
    , findDatum
    )
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value    as Value
import Plutus.V1.Ledger.Address ( Address(..))
import Plutus.V1.Ledger.Credential( Credential(..))

-- -- Import your types
-- import PublicFund.Types

--------------------------------------------------------------------------------
-- DATUM
--------------------------------------------------------------------------------
data FundDatum = FundDatum
    { officials         :: [PubKeyHash]
    , requiredApprovals :: Integer
    , deadline          :: POSIXTime
    , depositor         :: PubKeyHash
    , recipient         :: PubKeyHash        -- who gets the funds on release
    , approvalsCount    :: Integer           -- previously tracked approvals
    }
    deriving Show

unstableMakeIsData ''FundDatum

--------------------------------------------------------------------------------
-- REDEEMER
--------------------------------------------------------------------------------
data FundRedeemer
    = Approve          -- Just approve, keep funds in script
    | Release          -- Release funds to recipient
    | Refund           -- Refund to depositor
    deriving Show

PlutusTx.makeIsDataIndexed ''FundRedeemer
    [ ('Approve, 0)
    , ('Release, 1)
    , ('Refund,  2)
    ]

--------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------
{-# INLINABLE mkFundValidator #-}
mkFundValidator :: FundDatum -> FundRedeemer -> ScriptContext -> Bool
mkFundValidator dat red ctx =
    case red of
        Approve  -> validateApprove dat ctx
        Release  -> validateRelease dat ctx
        Refund   -> validateRefund dat ctx

--------------------------------------------------------------------------------
-- APPROVE (just increment approval count)
--------------------------------------------------------------------------------
{-# INLINABLE validateApprove #-}
validateApprove :: FundDatum -> ScriptContext -> Bool
validateApprove FundDatum{officials, requiredApprovals, deadline, approvalsCount} ctx =
    traceIfFalse "Deadline passed" beforeDeadline &&
    traceIfFalse "Not enough valid approvals for final approval" (approvalsCount + 1 < requiredApprovals) &&
    traceIfFalse "Not an official" isOfficial &&
    traceIfFalse "Funds must stay in script" fundsStayInScript
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        Interval.to deadline `Interval.contains` txInfoValidRange info

    signers :: [PubKeyHash]
    signers = txInfoSignatories info

    -- Check if signer is an official
    isOfficial :: Bool
    isOfficial = any (`elem` officials) signers

    -- For approval, funds must stay in script with updated datum
    fundsStayInScript :: Bool
    fundsStayInScript =
        let ownValHash = ownHash ctx
            outputs = getContinuingOutputs ctx
        in case outputs of
            [out] -> 
                let outValHash = case txOutAddress out of
                        Address (ScriptCredential vh) _ -> vh == ownValHash
                        _ -> False
                in outValHash && txOutValue out == scriptInputValue ctx
            _ -> False

--------------------------------------------------------------------------------
-- RELEASE (send funds to recipient)
--------------------------------------------------------------------------------
{-# INLINABLE validateRelease #-}
validateRelease :: FundDatum -> ScriptContext -> Bool
validateRelease FundDatum{officials, requiredApprovals, deadline, recipient, approvalsCount} ctx =
    traceIfFalse "Deadline passed" beforeDeadline &&
    traceIfFalse "Not enough approvals" (approvalsCount >= requiredApprovals) &&
    traceIfFalse "Recipient not paid" recipientPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        Interval.to deadline `Interval.contains` txInfoValidRange info

    recipientPaid :: Bool
    recipientPaid =
        let paid = valuePaidTo info recipient
        in Value.geq paid (scriptInputValue ctx)

--------------------------------------------------------------------------------
-- REFUND (back to depositor)
--------------------------------------------------------------------------------
{-# INLINABLE validateRefund #-}
validateRefund :: FundDatum -> ScriptContext -> Bool
validateRefund FundDatum{deadline, depositor, approvalsCount, requiredApprovals} ctx =
    traceIfFalse "Deadline not reached" afterDeadline &&
    traceIfFalse "Depositor did not sign" depositorSigned &&
    traceIfFalse "Funds not refunded" refunded &&
    traceIfFalse "Cannot refund if enough approvals" (approvalsCount < requiredApprovals)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    afterDeadline :: Bool
    afterDeadline =
        Interval.from deadline `Interval.contains` txInfoValidRange info

    depositorSigned :: Bool
    depositorSigned = txSignedBy info depositor

    refunded :: Bool
    refunded =
        let paid = valuePaidTo info depositor
        in Value.geq paid (scriptInputValue ctx)

--------------------------------------------------------------------------------
-- SCRIPT INPUT VALUE
--------------------------------------------------------------------------------
{-# INLINABLE scriptInputValue #-}
scriptInputValue :: ScriptContext -> Value
scriptInputValue ctx =
    case findOwnInput ctx of
        Just i  -> txOutValue (txInInfoResolved i)
        Nothing -> traceError "Script input missing"

--------------------------------------------------------------------------------
-- WRAPPER
--------------------------------------------------------------------------------
{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r c =
    if mkFundValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapper ||])

--------------------------------------------------------------------------------
-- WRITE SCRIPT
--------------------------------------------------------------------------------
writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let script = unValidatorScript val
        bs     = serialise script
        sh     = SBS.toShort (LBS.toStrict bs)
        scr    = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope file Nothing scr
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote script to: " <> file)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Compiled Public Fund Release (Anti-Corruption) contract!"
    writeValidator "./assets/public_fund.plutus" validator