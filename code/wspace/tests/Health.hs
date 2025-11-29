{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Health where

import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Interval (contains, from)  -- <-- fixed
import           PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import qualified Prelude                   as P
import           GHC.Generics              (Generic)

-- | Consent: patient grants access to a provider
data Consent = Consent
    { patientPKH  :: PubKeyHash
    , providerPKH :: PubKeyHash
    , scopes      :: [BuiltinByteString]
    , expiry      :: POSIXTime
    }
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''Consent
PlutusTx.makeLift ''Consent

-- | Access log: provider writes access events
data AccessLog = AccessLog
    { logHash :: BuiltinByteString
    , logTs   :: POSIXTime
    }
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''AccessLog
PlutusTx.makeLift ''AccessLog

-- | Claim states
data ClaimState = File | Approve | Reject
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''ClaimState
PlutusTx.makeLift ''ClaimState

-- | Claim: insurer payout request
data Claim = Claim
    { policyId     :: BuiltinByteString
    , claimant     :: PubKeyHash
    , amount       :: Integer
    , evidenceHash :: BuiltinByteString
    , state        :: ClaimState
    }
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''Claim
PlutusTx.makeLift ''Claim

-- | Validate minting/revoking consent
{-# INLINABLE validateConsent #-}
validateConsent :: Consent -> ScriptContext -> Bool
validateConsent c ctx =
    -- Only patient can mint/revoke consent
    txSignedBy (scriptContextTxInfo ctx) (patientPKH c) &&
    -- Consent must not be expired
    contains (from $ expiry c) (txInfoValidRange $ scriptContextTxInfo ctx)

-- | Validate access log appending
{-# INLINABLE validateAccessLog #-}
validateAccessLog :: AccessLog -> Consent -> ScriptContext -> Bool
validateAccessLog log consent ctx =
    -- Only provider in consent scope can append access
    txSignedBy (scriptContextTxInfo ctx) (providerPKH consent)

-- | Validate claim state
{-# INLINABLE validateClaim #-}
validateClaim :: Claim -> ScriptContext -> Bool
validateClaim claim ctx =
    case state claim of
        File    -> True  -- anyone can file a claim
        Approve -> txSignedBy (scriptContextTxInfo ctx) (claimant claim)
        Reject  -> True  -- insurer logic can be added here

-- | Example minting policy for consent
{-# INLINABLE consentPolicy #-}
consentPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
consentPolicy patient _ ctx = txSignedBy (scriptContextTxInfo ctx) patient

-- | Main function
main :: P.IO ()
main = P.putStrLn "Health Consent / Claim contract compiled successfully!"
