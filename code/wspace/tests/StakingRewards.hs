{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- StakingRewards.hs
-- A simple Plutus staking rewards contract (tutorial-style)
-- WARNING: This is an instructional example and not production audited code.

module StakingRewards where

import           Prelude                    (Show (..), String)
import           Control.Monad              (void)
import           Data.Aeson                 (ToJSON, FromJSON)
import           GHC.Generics               (Generic)
import qualified Data.Map                   as Map
import qualified Data.Text                  as T

import           Plutus.Contract            as Contract
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (singleton)
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Ledger.Constraints         as Constraints
import           Ledger.Ada                 as Ada
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrency)
import           Text.Printf                (printf)

-- | Datum stored at the script output representing a stake.
data StakeDatum = StakeDatum
    { sdStaker   :: PubKeyHash
    , sdAmount   :: Integer      -- amount of staking tokens (lovelace or token units)
    , sdStart    :: Slot
    } deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''StakeDatum
PlutusTx.makeLift ''StakeDatum

-- | Redeemers: only two actions (for clarity)
data StakeRedeemer = RedeemWithdraw
    deriving Show

PlutusTx.unstableMakeIsData ''StakeRedeemer
PlutusTx.makeLift ''StakeRedeemer

-- | A very small on-chain validator: ensures only the original staker can withdraw the stake UTXO.
-- It also ensures the transaction pays back at least the principal to the staker. Reward payments
-- are expected to be handled by the off-chain code (e.g. paid from a reward wallet or pool) so
-- this validator doesn't attempt to compute reward amounts.
{-# INLINABLE mkValidator #-}
mkValidator :: StakeDatum -> StakeRedeemer -> ScriptContext -> Bool
mkValidator dat _ ctx =
    traceIfFalse "not signed by staker" signedByStaker &&
    traceIfFalse "principal not returned" principalReturned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByStaker :: Bool
    signedByStaker = txSignedBy info $ sdStaker dat

    principalReturned :: Bool
    principalReturned =
        let paid = valuePaidTo info (sdStaker dat)
            expected = lovelaceValueOf (sdAmount dat)
        in  traceIfFalse "insufficient principal returned" (paid `geq` expected)

-- Boilerplate to make a typed validator
data Staking
instance Scripts.ValidatorTypes Staking where
    type instance RedeemerType Staking = StakeRedeemer
    type instance DatumType Staking = StakeDatum

typedValidator :: Scripts.TypedValidator Staking
typedValidator = Scripts.mkTypedValidator @Staking
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @StakeDatum @StakeRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- | Off-chain contract schema exposing simple endpoints.
--  * "stake" takes an amount of lovelace to lock in the script under the caller's PKH
--  * "unstake" consumes the script UTXO for the caller and expects the off-chain code to
--              fund and include reward payments such that the script checks pass

type StakingSchema = Endpoint "stake" Integer
                 .\/ Endpoint "unstake" ()

-- | Stake endpoint: creates a script output with the StakeDatum
stake :: AsContractError e => Integer -> Contract w s e ()
stake amt = do
    pkh <- Contract.ownPubKeyHash
    currentSlot <- Contract.currentSlot
    let dat = StakeDatum
                { sdStaker = pkh
                , sdAmount = amt
                , sdStart = currentSlot
                }
        tx = Constraints.mustPayToTheScript dat (lovelaceValueOf amt)
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "staked %d lovelace at slot %s" amt (show currentSlot)

-- | Find the script UTXO for our wallet (we keep this simple and take the first matching UTXO)
findMyStakeUTXO :: PubKeyHash -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, StakeDatum))
findMyStakeUTXO pkh = do
    utxos <- utxosAt scrAddress
    -- attempt to find a UTXO with datum matching our pkh
    let convertOut (oref, o) = case _ciTxOutDatum o of
            Left _ -> Nothing
            Right (Datum d) -> case PlutusTx.fromBuiltinData d of
                Nothing -> Nothing
                Just sd -> Just (oref, o, sd)
        matches = Map.foldrWithKey (\k v acc -> case convertOut (k,v) of
                                Just t@(_,_,sd) -> if sdStaker sd == pkh then t:acc else acc
                                Nothing -> acc) [] utxos
    return $ case matches of
        (x:_) -> Just x
        _     -> Nothing

-- | Unstake endpoint: calculates rewards (off-chain), constructs transaction that consumes
-- the script UTXO and pays principal+reward to the staker. The reward funds must come from
-- the wallet running this off-chain code (e.g., the reward pool owner).
unstake :: forall w s e. AsContractError e => Contract w s e ()
unstake = do
    pkh <- Contract.ownPubKeyHash
    m <- findMyStakeUTXO pkh
    case m of
        Nothing -> Contract.logError @String "No stake found for this wallet"
        Just (oref, o, sd) -> do
            now <- Contract.currentSlot
            let principal = sdAmount sd
                duration = (getSlot now) - (getSlot $ sdStart sd)
                -- For tutorial purposes: rewardRate is 1 lovelace per 1000 lovelace per slot.
                -- This means reward = principal * duration / 1000
                reward = (principal * duration) `divide` 1000
                totalToPay = principal + reward

            Contract.logInfo @String $ printf "computed reward %d over %d slots (principal %d)" reward duration principal

            -- Constraints: consume the script output and pay principal+reward to pkh
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, toTxOut o)])
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData RedeemWithdraw)
                          <> Constraints.mustPayToPubKey pkh (lovelaceValueOf totalToPay)

            -- We also require that the wallet running this endpoint supplies the reward amount.
            ledgerTx <- submitTxConstraintsWith @Staking lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "unstaked and paid total %d (including reward %d)" totalToPay reward
  where
    -- utility to convert ChainIndexTxOut to TxOut for lookup map
    toTxOut :: ChainIndexTxOut -> TxOut
    toTxOut cio = case cio of
        Datumed{_ciTxOutAddress = a, _ciTxOutValue = v, _ciTxOutDatum = _ } -> TxOut a v Nothing
        _ -> case cio of
            ScriptChainIndexTxOut{} -> TxOut scrAddress (lovelaceValueOf 0) Nothing

-- | Endpoints
endpoints :: Contract () StakingSchema Text ()
endpoints = awaitPromise (stake' `select` unstake') >> endpoints
  where
    stake' = endpoint "stake" $ \amt -> do
        stake amt
    unstake' = endpoint "unstake" $ const unstake

-- | Helper: integer division that avoids divide-by-zero
{-# INLINABLE divide #-}
divide :: Integer -> Integer -> Integer
divide x y = if y == 0 then 0 else x `PlutusTx.Prelude.divide` y

-- Boilerplate for Playground (commented for real deployment)
mkSchemaDefinitions ''StakingSchema

-- NOTE and SECURITY CONSIDERATIONS
-- 1) This example delegates reward computation and funding to off-chain code. The on-chain
--    validator only checks the staker signature and that at least the principal is returned.
--    A malicious off-chain operator could attempt to pay less reward; so in production you must
--    either enforce reward logic on-chain (using a state machine or on-chain config UTXO), or
--    use an oracle/trusted mechanism to assert reward amounts.
-- 2) Token-denominated staking: here we treat "amount" as lovelace. To stake custom tokens,
--    adapt `lovelaceValueOf` to `singleton` or create a Value parameter.
-- 3) This contract is intentionally simple for learning. For production you will want:
--       - a proper state machine or central config UTXO for reward rates
--       - ability to pause/unpause, emergency withdraws, admin controls
--       - unit tests/simulations using emulator
--
-- If you'd like, I can:
--  * convert this to use a state machine with an on-chain reward rate
--  * adapt it for staking a specific native token (AssetClass)
--  * write emulator tests and a README describing deployment and usage
--
