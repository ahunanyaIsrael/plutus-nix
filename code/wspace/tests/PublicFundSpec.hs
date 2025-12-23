-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

-- module Main where

-- import Prelude (IO, putStrLn, print, show, FilePath, ($), (++) )
-- import qualified Data.ByteString.Char8 as BS

-- import Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS

-- import PlutusTx (toBuiltinData)
-- import qualified PlutusTx
-- import PlutusTx.Builtins (toBuiltin)

-- import Plutus.V2.Ledger.Api
--     ( Validator
--     , PubKeyHash(PubKeyHash)
--     , POSIXTime(POSIXTime)
--     )

-- -- import the writeValidator function and the on-chain values
-- import PublicFund
--     ( validator
--     , writeValidator
--     , FundDatum(..)
--     , FundRedeemer(..)
--     )

-- -- helper: save a datum to CBOR (serialised BuiltinData)
-- saveDatum :: FilePath -> FundDatum -> IO ()
-- saveDatum filepath dat = do
--     let d   = PlutusTx.toData dat   -- produces `Data`
--         lbs = serialise d           -- Data has a Serialise instance
--     LBS.writeFile filepath lbs
--     putStrLn $ "Saved datum to: " ++ filepath

-- main :: IO ()
-- main = do
--     putStrLn "----- PublicFund test harness -----"

--     -- create sample pubkeyhashes (use hex-like strings; real tests should use
--     -- actual key bytes you want)
--     let p1 = PubKeyHash $ toBuiltin (BS.pack "pubkeyhash1")
--         p2 = PubKeyHash $ toBuiltin (BS.pack "pubkeyhash2")
--         p3 = PubKeyHash $ toBuiltin (BS.pack "pubkeyhash3")

--     -- sample FundDatum
--     let sampleDatum = FundDatum
--             { officials         = [p1, p2, p3]
--             , requiredApprovals = 2
--             , deadline          = POSIXTime 1000000
--             , depositor         = p1
--             , recipient         = p2
--             , approvalsCount    = 1
--             }

--     -- sample redeemer values (you can show them, they are on-chain enums)
--     let rApprove = Approve
--         rRelease = Release
--         rRefund  = Refund

--     -- write the compiled validator to disk (same as your existing helper)
--     writeValidator "./assets/public_fund.plutus" validator

--     putStrLn "Wrote validator to ./assets/public_fund.plutus"
--     putStrLn "Sample datum (show):"
--     print sampleDatum

--     -- save the datum as CBOR (serialised BuiltinData)
--     saveDatum "./assets/sample_fund_datum.cbor" sampleDatum

--     putStrLn "Sample redeemers:"
--     putStrLn $ " - " ++ show rApprove
--     putStrLn $ " - " ++ show rRelease
--     putStrLn $ " - " ++ show rRefund

--     putStrLn "Test harness finished."

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Main where

-- Core libs
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Data.ByteString.Char8 as BS8

-- Cardano API to write .plutus file
import           Cardano.Api (writeFileTextEnvelope, displayError)
import           Cardano.Api.Shelley
    ( PlutusScript (..)
    , PlutusScriptV2
    )

-- Plutus on-chain libs
import           PlutusTx
import           PlutusTx.Prelude hiding ((<>), Show, find, foldr)
import qualified Prelude as P (IO, FilePath, print, putStrLn, Show, show, ($), (<>), Maybe(..), Eq(..), (++))
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Builtins (toBuiltin)

import           Plutus.V2.Ledger.Api
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
    , TxOutRef(..)
    , TxId(..)
    , TokenName(..)
    , TxInInfo(..)
    , POSIXTimeRange
    )
import           Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    , txInInfoOutRef
    , txInInfoResolved
    , ownHash
    , findOwnInput
    , scriptContextTxInfo
    )
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value    as Value
import           Plutus.V1.Ledger.Address ( Address(..))
import           Plutus.V1.Ledger.Credential( Credential(..))
import           Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import           Plutus.V1.Ledger.Time   (POSIXTime(..))
-- Test framework imports
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.ExpectedFailure

--------------------------------------------------------------------------------
-- DATUM & REDEEMER
--------------------------------------------------------------------------------
data FundDatum = FundDatum
    { officials         :: [PubKeyHash]
    , requiredApprovals :: Integer
    , deadline          :: POSIXTime
    , depositor         :: PubKeyHash
    , recipient         :: PubKeyHash
    , approvalsCount    :: Integer
    }
    deriving (P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''FundDatum

data FundRedeemer
    = Approve
    | Release
    | Refund
    deriving (P.Show, P.Eq)

PlutusTx.makeIsDataIndexed ''FundRedeemer
    [ ('Approve, 0)
    , ('Release, 1)
    , ('Refund,  2)
    ]

--------------------------------------------------------------------------------
-- VALIDATOR (Simplified for unit tests)
--------------------------------------------------------------------------------
{-# INLINABLE mkFundValidator #-}
mkFundValidator :: FundDatum -> FundRedeemer -> ScriptContext -> Bool
mkFundValidator dat red ctx =
    case red of
        Approve  -> validateApprove dat ctx
        Release  -> validateRelease dat ctx
        Refund   -> validateRefund dat ctx

{-# INLINABLE validateApprove #-}
validateApprove :: FundDatum -> ScriptContext -> Bool
validateApprove FundDatum{officials, requiredApprovals, deadline, approvalsCount} ctx =
    beforeDeadline &&
    (approvalsCount + 1 < requiredApprovals) &&
    isOfficial
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        Interval.to deadline `Interval.contains` txInfoValidRange info

    signers :: [PubKeyHash]
    signers = txInfoSignatories info

    isOfficial :: Bool
    isOfficial = any (`elem` officials) signers

{-# INLINABLE validateRelease #-}
validateRelease :: FundDatum -> ScriptContext -> Bool
validateRelease FundDatum{requiredApprovals, deadline, approvalsCount} ctx =
    beforeDeadline &&
    (approvalsCount >= requiredApprovals)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        Interval.to deadline `Interval.contains` txInfoValidRange info

{-# INLINABLE validateRefund #-}
validateRefund :: FundDatum -> ScriptContext -> Bool
validateRefund FundDatum{deadline, depositor, approvalsCount, requiredApprovals} ctx =
    afterDeadline &&
    depositorSigned &&
    (approvalsCount < requiredApprovals)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    afterDeadline :: Bool
    afterDeadline =
        Interval.from (deadline + 1) `Interval.contains` txInfoValidRange info

    depositorSigned :: Bool
    depositorSigned = txSignedBy info depositor

--------------------------------------------------------------------------------
-- WRAPPER & COMPILED VALIDATOR
--------------------------------------------------------------------------------
{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r c =
    if mkFundValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else PlutusTx.Prelude.error ()

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapper ||])

--------------------------------------------------------------------------------
-- MOCKS & HELPERS FOR TESTING
--------------------------------------------------------------------------------
mockPKH1 :: PubKeyHash
mockPKH1 = PubKeyHash $ toBuiltin $ BS8.pack "official1"

mockPKH2 :: PubKeyHash
mockPKH2 = PubKeyHash $ toBuiltin $ BS8.pack "official2"

mockPKH3 :: PubKeyHash
mockPKH3 = PubKeyHash $ toBuiltin $ BS8.pack "official3"

mockDepositor :: PubKeyHash
mockDepositor = PubKeyHash $ toBuiltin $ BS8.pack "depositor"

mockRecipient :: PubKeyHash
mockRecipient = PubKeyHash $ toBuiltin $ BS8.pack "recipient"

mockNonOfficial :: PubKeyHash
mockNonOfficial = PubKeyHash $ toBuiltin $ BS8.pack "nonofficial"

mockDatum :: FundDatum
mockDatum = FundDatum
    { officials = [mockPKH1, mockPKH2, mockPKH3]
    , requiredApprovals = 2
    , deadline = 100
    , depositor = mockDepositor
    , recipient = mockRecipient
    , approvalsCount = 0
    }

createMockContext :: [PubKeyHash] -> POSIXTimeRange -> ScriptContext
createMockContext signers timeRange =
    ScriptContext
        { scriptContextTxInfo = TxInfo
            { txInfoInputs = []
            , txInfoOutputs = []
            , txInfoFee = mempty
            , txInfoMint = mempty
            , txInfoDCert = []
            , txInfoWdrl = AssocMap.empty
            , txInfoValidRange = timeRange
            , txInfoSignatories = signers
            , txInfoData = AssocMap.empty
            , txInfoId = TxId $ toBuiltin $ BS8.pack "txid0"
            , txInfoReferenceInputs = []
            , txInfoRedeemers = AssocMap.empty
            }
        , scriptContextPurpose = Spending (TxOutRef (TxId $ toBuiltin $ BS8.pack "txid0") 0)
        }

--------------------------------------------------------------------------------
-- TEST SUITE (TASTY)
--------------------------------------------------------------------------------
testValidatorCompilation :: TestTree
testValidatorCompilation = testCase "Validator compiles successfully" $ do
    P.putStrLn "Testing validator compilation..."
    let _ = validator
    assertBool "Validator should compile" True

testSerialization :: TestTree
testSerialization = testGroup "Serialization Tests"
  [ testCase "Datum roundtrip via BuiltinData" $ do
      let bd = toBuiltinData mockDatum
          recovered = unsafeFromBuiltinData bd :: FundDatum
      assertBool "Datum roundtrip should succeed" (recovered P.== mockDatum)

  , testCase "Redeemer roundtrip via BuiltinData" $ do
      let a = toBuiltinData (Approve :: FundRedeemer)
          r = toBuiltinData (Release :: FundRedeemer)
          f = toBuiltinData (Refund :: FundRedeemer)

          ra = unsafeFromBuiltinData a :: FundRedeemer
          rr = unsafeFromBuiltinData r :: FundRedeemer
          rf = unsafeFromBuiltinData f :: FundRedeemer

      assertBool "Approve redeemer should roundtrip" (ra P.== Approve)
      assertBool "Release redeemer should roundtrip" (rr P.== Release)
      assertBool "Refund redeemer should roundtrip" (rf P.== Refund)
  ]

testLogicValidation :: TestTree
testLogicValidation = testGroup "Validator Logic Tests"
    [ testCase "Official can approve before deadline" $ do
        let ctx = createMockContext [mockPKH1] (Interval.interval 0 90)
        let result = mkFundValidator mockDatum Approve ctx
        assertBool "Official should be able to approve before deadline" result

    , testCase "Non-official cannot approve" $ do
        let ctx = createMockContext [mockNonOfficial] (Interval.interval 0 90)
        let result = mkFundValidator mockDatum Approve ctx
        assertBool "Non-official should NOT be able to approve" (not result)

    , testCase "Cannot approve after deadline" $ do
        let ctx = createMockContext [mockPKH1] (Interval.interval 110 200)
        let result = mkFundValidator mockDatum Approve ctx
        assertBool "Cannot approve after deadline" (not result)

    , testCase "Cannot release without enough approvals" $ do
        let ctx = createMockContext [] (Interval.interval 0 90)
        let result = mkFundValidator mockDatum Release ctx
        assertBool "Cannot release without enough approvals" (not result)

    , testCase "Can release with enough approvals" $ do
        let datumWithApprovals = mockDatum { approvalsCount = 2 }
        let ctx = createMockContext [] (Interval.interval 0 90)
        let result = mkFundValidator datumWithApprovals Release ctx
        assertBool "Can release with enough approvals" result

    , testCase "Cannot release after deadline" $ do
        let datumWithApprovals = mockDatum { approvalsCount = 2 }
        let ctx = createMockContext [] (Interval.interval 110 200)
        let result = mkFundValidator datumWithApprovals Release ctx
        assertBool "Cannot release after deadline" (not result)

    , testCase "Cannot refund before deadline" $ do
        let ctx = createMockContext [mockDepositor] (Interval.interval 0 90)
        let result = mkFundValidator mockDatum Refund ctx
        assertBool "Cannot refund before deadline" (not result)

    , testCase "Depositor can refund after deadline" $ do
        let datumWithNoApprovals = mockDatum { approvalsCount = 0 }
        let ctx = createMockContext [mockDepositor] (Interval.from 101)
        let result = mkFundValidator datumWithNoApprovals Refund ctx
        assertBool "Depositor should be able to refund after deadline" result

    , testCase "Non-depositor cannot refund" $ do
        let ctx = createMockContext [mockPKH1] (Interval.from 101)
        let result = mkFundValidator mockDatum Refund ctx
        assertBool "Non-depositor should NOT be able to refund" (not result)

    , testCase "Cannot refund if enough approvals" $ do
        let datumWithApprovals = mockDatum { approvalsCount = 2 }
        let ctx = createMockContext [mockDepositor] (Interval.from 101)
        let result = mkFundValidator datumWithApprovals Refund ctx
        assertBool "Cannot refund if enough approvals" (not result)
    ]

testPlutusScriptGeneration :: TestTree
testPlutusScriptGeneration = testCase "Generate Plutus script file" $ do
    P.putStrLn "Generating Plutus script..."
    let script = unValidatorScript validator
        bs     = serialise script
        sh     = SBS.toShort (LBS.toStrict bs)
        scr    = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2

    result <- writeFileTextEnvelope "./assets/public_fund_final.plutus" Nothing scr
    case result of
        Left err -> assertFailure $ "Failed to write script: " ++ P.show err
        Right () -> P.putStrLn "✓ Generated public_fund_final.plutus"

--------------------------------------------------------------------------------
-- ARTIFACT WRITING HELPERS (writes validator + sample datum)
--------------------------------------------------------------------------------
writeValidator :: P.FilePath -> Validator -> P.IO ()
writeValidator filepath val = do
    let script = unValidatorScript val
        bs     = serialise script
        sh     = SBS.toShort (LBS.toStrict bs)
        scr    = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope filepath Nothing scr
    case result of
      Left err -> P.putStrLn $ "Failed to write validator: " ++ P.show err
      Right () -> P.putStrLn $ "Wrote validator to: " ++ filepath

saveDatum :: P.FilePath -> FundDatum -> P.IO ()
saveDatum filepath dat = do
    let d   = PlutusTx.toData dat
        lbs = serialise d
    LBS.writeFile filepath lbs
    P.putStrLn $ "Saved datum to: " ++ filepath

--------------------------------------------------------------------------------
-- MAIN: write artifacts, then run tests
--------------------------------------------------------------------------------
main :: P.IO ()
main = do
    P.putStrLn "----- PublicFund test harness (artifacts + tests) -----"

    -- sample pubkeyhashes for writing a sample datum
    let p1 = PubKeyHash $ toBuiltin (BS8.pack "pubkeyhash1")
        p2 = PubKeyHash $ toBuiltin (BS8.pack "pubkeyhash2")
        p3 = PubKeyHash $ toBuiltin (BS8.pack "pubkeyhash3")

    let sampleDatum = FundDatum
            { officials         = [p1, p2, p3]
            , requiredApprovals = 2
            , deadline          = POSIXTime 1000000
            , depositor         = p1
            , recipient         = p2
            , approvalsCount    = 1
            }

    -- write validator and sample datum
    writeValidator "./assets/public_fund.plutus" validator
    saveDatum "./assets/sample_fund_datum.cbor" sampleDatum

    P.putStrLn "Now running tests..."
    P.putStrLn ""

    defaultMain $ testGroup "All Tests"
        [ testValidatorCompilation
        , testSerialization
        , testLogicValidation
        , testPlutusScriptGeneration
        ]
