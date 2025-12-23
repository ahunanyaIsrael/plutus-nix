{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Emulator where

import           Control.Lens
import           Control.Monad          (void)
import           Data.Default           (Default (..))
import           Ledger
import           Ledger.Ada             as Ada
import           Ledger.TimeSlot        (slotToBeginPOSIXTime)
import           Plutus.Contract
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.Prelude
import           Prelude                (IO, Show (..), String)
import           Wallet.Emulator.Wallet

import           OffChain
import           Types

testFunds :: Value
testFunds = Ada.lovelaceValueOf 100_000_000  -- 100 ADA

-- Test parameters
officials :: [Wallet]
officials = [w1, w2, w3]  -- 3 officials

requiredApprovals :: Integer
requiredApprovals = 2

deadline :: POSIXTime
deadline = slotToBeginPOSIXTime def 100

-- Helper to get PKH
toPubKeyHash :: Wallet -> PubKeyHash
toPubKeyHash = mockWalletPaymentPubKeyHash

-- SCENARIO 1: Successful release
successfulReleaseTrace :: EmulatorTrace ()
successfulReleaseTrace = do
    let depositor = w4
        recipient = w5
        params = FundParams
            { fpOfficials = map toPubKeyHash officials
            , fpRequiredApprovals = requiredApprovals
            , fpDeadline = deadline
            , fpRecipient = toPubKeyHash recipient
            , fpAmount = testFunds
            }
    
    -- Deposit funds
    hdl <- activateContractWallet depositor endpoints
    callEndpoint @"deposit" hdl params
    void $ waitNSlots 5
    
    -- Get UTxO reference
    utxo <- utxoAt (validatorAddress)
    let [(oref, _)] = Map.toList utxo
    
    -- First approval (w1)
    hdl1 <- activateContractWallet w1 endpoints
    callEndpoint @"approve" hdl1 $ ApproveParams oref
    void $ waitNSlots 5
    
    -- Second approval (w2) - reaches threshold
    hdl2 <- activateContractWallet w2 endpoints
    callEndpoint @"approve" hdl2 $ ApproveParams oref
    void $ waitNSlots 5
    
    -- Release funds (anyone can call release)
    callEndpoint @"release" hdl $ ReleaseParams oref
    void $ waitNSlots 5
    
    -- Check recipient received funds
    recipientFunds <- fundsAt recipient
    assert "Recipient should receive funds" (recipientFunds == testFunds)

-- SCENARIO 2: Refund after deadline with insufficient approvals
refundTrace :: EmulatorTrace ()
refundTrace = do
    let depositor = w4
        recipient = w5
        params = FundParams
            { fpOfficials = map toPubKeyHash officials
            , fpRequiredApprovals = requiredApprovals
            , fpDeadline = deadline
            , fpRecipient = toPubKeyHash recipient
            , fpAmount = testFunds
            }
    
    -- Deposit funds
    hdl <- activateContractWallet depositor endpoints
    callEndpoint @"deposit" hdl params
    void $ waitNSlots 5
    
    -- Get UTxO reference
    utxo <- utxoAt validatorAddress
    let [(oref, _)] = Map.toList utxo
    
    -- Only one approval (insufficient)
    hdl1 <- activateContractWallet w1 endpoints
    callEndpoint @"approve" hdl1 $ ApproveParams oref
    void $ waitNSlots 5
    
    -- Wait until after deadline
    void $ waitUntilSlot 110
    
    -- Refund
    callEndpoint @"refund" hdl $ RefundParams oref
    void $ waitNSlots 5
    
    -- Check depositor got refund
    depositorFunds <- fundsAt depositor
    assert "Depositor should get refund" (depositorFunds == testFunds)

-- SCENARIO 3: Failed approval by non-official
nonOfficialTrace :: EmulatorTrace ()
nonOfficialTrace = do
    let depositor = w4
        recipient = w5
        nonOfficial = w6  -- Not in officials list
        params = FundParams
            { fpOfficials = map toPubKeyHash officials
            , fpRequiredApprovals = requiredApprovals
            , fpDeadline = deadline
            , fpRecipient = toPubKeyHash recipient
            , fpAmount = testFunds
            }
    
    -- Deposit funds
    hdl <- activateContractWallet depositor endpoints
    callEndpoint @"deposit" hdl params
    void $ waitNSlots 5
    
    -- Get UTxO reference
    utxo <- utxoAt validatorAddress
    let [(oref, _)] = Map.toList utxo
    
    -- Non-official tries to approve (should fail)
    hdlNonOfficial <- activateContractWallet nonOfficial endpoints
    callEndpoint @"approve" hdlNonOfficial $ ApproveParams oref
    
    -- Wait and check funds still in contract
    void $ waitNSlots 10
    contractFunds <- fundsAt validatorAddress
    assert "Funds should remain in contract" (contractFunds == testFunds)

-- SCENARIO 4: Early release attempt (should fail)
earlyReleaseTrace :: EmulatorTrace ()
earlyReleaseTrace = do
    let depositor = w4
        recipient = w5
        params = FundParams
            { fpOfficials = map toPubKeyHash officials
            , fpRequiredApprovals = requiredApprovals
            , fpDeadline = deadline
            , fpRecipient = toPubKeyHash recipient
            , fpAmount = testFunds
            }
    
    -- Deposit funds
    hdl <- activateContractWallet depositor endpoints
    callEndpoint @"deposit" hdl params
    void $ waitNSlots 5
    
    -- Get UTxO reference
    utxo <- utxoAt validatorAddress
    let [(oref, _)] = Map.toList utxo
    
    -- Try to release without enough approvals (should fail)
    callEndpoint @"release" hdl $ ReleaseParams oref
    
    -- Wait and check funds still in contract
    void $ waitNSlots 10
    contractFunds <- fundsAt validatorAddress
    assert "Funds should remain in contract" (contractFunds == testFunds)

-- Run all tests
runTests :: IO ()
runTests = do
    putStrLn "Running Public Fund Release Tests..."
    putStrLn "\n1. Testing successful release..."
    Emulator.runEmulatorTraceIO successfulReleaseTrace
    
    putStrLn "\n2. Testing refund after deadline..."
    Emulator.runEmulatorTraceIO refundTrace
    
    putStrLn "\n3. Testing non-official approval attempt..."
    Emulator.runEmulatorTraceIO nonOfficialTrace
    
    putStrLn "\n4. Testing early release attempt..."
    Emulator.runEmulatorTraceIO earlyReleaseTrace
    
    putStrLn "\nAll tests completed!"