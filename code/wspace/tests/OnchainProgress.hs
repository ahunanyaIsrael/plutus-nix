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

-- OnchainProgressCredential.hs
-- A simple Plutus smart contract that mints "Onchain Progress Credentials"
-- as native tokens (NFT-like). The minting policy restricts minting to a single
-- issuer (a PubKeyHash). Off-chain contract exposes endpoints to issue and
-- revoke credentials. The token name encodes a credential id and progress.
--
-- NOTE: This file targets the Plutus Playground / plutus-apps style contract
-- structure. You may need to adapt imports/stack/cabal for your Plutus SDK.

module OnchainProgressCredential where

import           Prelude (String, Show (..), print)

import           Control.Monad        hiding (fmap)
import qualified Data.Map             as Map
import           Data.Aeson           (ToJSON, FromJSON)
import           GHC.Generics         (Generic)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Plutus.Contract      as Contract
import           PlutusTx             (BuiltinData, compile, applyCode, liftCode)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (mint, singleton)
import           Ledger.Contexts      as V
import           Ledger.Constraints   as Constraints
import           Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

--------------------------------------------------
-- On-chain: Minting policy
--------------------------------------------------

-- | Parameters for the minting policy: the issuer (PubKeyHash) who is allowed to mint/revoke
newtype OPCParams = OPCParams
    { opIssuer :: PubKeyHash }
    deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''OPCParams

-- The redeemer chooses action: Mint or Burn
data OPCAction = MintCredential | BurnCredential
PlutusTx.unstableMakeIsData ''OPCAction
PlutusTx.makeLift ''OPCAction

{-# INLINABLE mkPolicy #-}
mkPolicy :: OPCParams -> OPCAction -> ScriptContext -> Bool
mkPolicy params action ctx =
    case action of
        MintCredential -> traceIfFalse "issuer signature missing" signedByIssuer
                         && traceIfFalse "must mint exactly one token" checkMintAmount
        BurnCredential -> traceIfFalse "issuer signature missing" signedByIssuer
                         && traceIfFalse "must burn exactly one token" checkBurnAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByIssuer :: Bool
    signedByIssuer = txSignedBy info $ opIssuer params

    ownSymbol :: CurrencySymbol
    ownSymbol = ownCurrencySymbol ctx

    -- ensure minted value for this policy contains exactly 1 token of any TokenName
    checkMintAmount :: Bool
    checkMintAmount = case flattenValue (txInfoMint info) of
        [ (cs, tn, amt) ] -> cs == ownSymbol && amt == 1
        _                 -> False

    checkBurnAmount :: Bool
    checkBurnAmount = case flattenValue (txInfoMint info) of
        [ (cs, tn, amt) ] -> cs == ownSymbol && amt == -1
        _                 -> False

policy :: OPCParams -> Scripts.MintingPolicy
policy params = mkMintingPolicyScript $$(PlutusTx.compile [|| \p -> Scripts.wrapMintingPolicy (mkPolicy p) ||]) `PlutusTx.applyCode` PlutusTx.liftCode params

curSymbol :: OPCParams -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

--------------------------------------------------
-- Off-chain: Contract endpoints
--------------------------------------------------

-- Data to issue a credential
data IssueParams = IssueParams
    { ipRecipient   :: PaymentPubKeyHash
    , ipCredentialId :: !Text      -- credential id (e.g. "course-xyz-001")
    , ipProgress    :: !Integer   -- progress percentage 0..100
    , ipMetadataURI :: !(Maybe Text) -- optional URI (IPFS/HTTP) for metadata
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Data to revoke a credential (burn)
newtype RevokeParams = RevokeParams
    { rpCredentialName :: Text }
    deriving (Show, Generic, ToJSON, FromJSON)

-- Schema with two endpoints
type OPCSchema =
        Endpoint "issue" IssueParams
    .\/ Endpoint "revoke" RevokeParams

-- Helper to build a token name encoding credential id and progress
{-# INLINABLE mkTokenName #-}
mkTokenName :: Text -> Integer -> TokenName
mkTokenName credId progress = TokenName $ encodeUtf8 $ credId <> "|p=" <> (T.pack $ show progress)
  where
    encodeUtf8 = Data.Text.Encoding.encodeUtf8

-- Issue endpoint implementation
issue :: OPCParams -> IssueParams -> Contract w s Text ()
issue params ip = do
    let issuerPkh = opIssuer params
        tn = mkTokenName (ipCredentialId ip) (ipProgress ip)
        val = singleton (curSymbol params) tn 1
        lookups = Constraints.mintingPolicy (policy params)
        tx = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData MintCredential) val
             <> Constraints.mustPayToPubKey (ipRecipient ip) val
             <> Constraints.mustBeSignedBy issuerPkh
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ "Issued credential " ++ show tn ++ " to " ++ show (ipRecipient ip)

-- Revoke endpoint implementation (burn)
revoke :: OPCParams -> RevokeParams -> Contract w s Text ()
revoke params rp = do
    let issuerPkh = opIssuer params
        tn = TokenName $ Data.Text.Encoding.encodeUtf8 $ rpCredentialName rp
        val = singleton (curSymbol params) tn (-1)
        lookups = Constraints.mintingPolicy (policy params)
        tx = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData BurnCredential) val
             <> Constraints.mustBeSignedBy issuerPkh
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ "Revoked credential " ++ show (rpCredentialName rp)

-- Endpoints
endpoints :: OPCParams -> Contract () OPCSchema Text ()
endpoints params = forever
    $ handleError logError
    $ awaitPromise
    $ issue' `select` revoke'
  where
    issue' = endpoint @"issue" $ issue params
    revoke' = endpoint @"revoke" $ revoke params

--------------------------------------------------
-- Utility / Example instantiation
--------------------------------------------------

-- Example: construct OPCParams from a PubKeyHash
-- In practice you'll supply the real issuer PubKeyHash when running the contract

{-
Example usage (PAB / local testing):

1. Start the PAB with this contract compiled and available.
2. Provide the issuer's PubKeyHash as parameter to the policy.
3. Call the `issue` endpoint with:
   {
     "ipRecipient": "<recipient payment key hash>",
     "ipCredentialId": "blockchain-course-001",
     "ipProgress": 60,
     "ipMetadataURI": "ipfs://..." (optional)
   }
4. The contract will mint 1 token with TokenName like "blockchain-course-001|p=60"
   and send it to the recipient's pubkey address.

Notes & considerations:
- Token metadata (CIP-25) should be published off-chain (IPFS) and referenced by metadata URI.
  Cardano on-chain native tokens don't carry rich JSON metadata by themselves.
- To make tokens soulbound (non-transferable), consider minting tokens to a script address
  with a validator that only allows spending (transfer) back to the issuer, or use an
  on-chain validation pattern where transfers are rejected unless certain conditions are met.
- This example restricts minting/burning to the issuer (a single PubKeyHash). You can
  extend the policy to allow delegated issuers or time-limited minting windows.
- Always test thoroughly on a testnet and with plutus-apps test framework before mainnet.
-}

