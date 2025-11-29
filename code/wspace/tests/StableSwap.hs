{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data PoolDatum = PoolDatum
    { pdBalances :: [Integer]       -- balances of each coin in pool
    , pdAmp      :: Integer         -- amplification factor
    , pdFeeBps   :: Integer         -- fee in basis points
    , pdAdmin    :: PubKeyHash      -- admin reference
    }
PlutusTx.unstableMakeIsData ''PoolDatum

data PoolAction = Swap Integer Integer Integer   -- fromIdx, toIdx, amountIn
                | AddLiq [Integer]               -- amounts per coin
                | RemoveOne Integer Integer      -- coinIdx, amountOut
                | RemoveProportional Integer     -- amountOut of LP
PlutusTx.unstableMakeIsData ''PoolAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE getBalance #-}
getBalance :: [Integer] -> Integer -> Integer
getBalance balances idx =
    if idx < length balances then balances !! idx else traceError "invalid index"

{-# INLINABLE sumList #-}
sumList :: [Integer] -> Integer
sumList = foldl (+) 0

{-# INLINABLE prodList #-}
prodList :: [Integer] -> Integer
prodList = foldl (*) 1

{-# INLINABLE intPow #-}
-- Integer exponentiation for on-chain use
intPow :: Integer -> Integer -> Integer
intPow base exp
    | exp == 0  = 1
    | exp == 1  = base
    | otherwise = base * intPow base (exp - 1)

{-# INLINABLE invariant #-}
-- StableSwap invariant (simplified for N coins)
invariant :: [Integer] -> Integer -> Integer
invariant balances amp =
    let n = length balances
        s = sumList balances
        p = prodList balances
    in amp * (intPow n n) * s + p

{-# INLINABLE feeAmount #-}
feeAmount :: Integer -> Integer -> Integer
feeAmount amt feeBps = (amt * feeBps) `divide` 10000

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkValidator pd action ctx =
    case action of
      Swap i j amtIn ->
        traceIfFalse "invalid indices" (i < length (pdBalances pd) && j < length (pdBalances pd) && i /= j) &&
        traceIfFalse "fee exceeds limits" feeOk &&
        traceIfFalse "invariant violated" invariantOk
      AddLiq amounts ->
        traceIfFalse "balances mismatch" (length amounts == length (pdBalances pd)) &&
        traceIfFalse "fee exceeded" feeOk
      RemoveOne idx amtOut ->
        traceIfFalse "invalid index" (idx < length (pdBalances pd)) &&
        traceIfFalse "fee exceeded" feeOk
      RemoveProportional amtOut ->
        traceIfFalse "amtOut > sum balances" (amtOut <= sumList (pdBalances pd)) &&
        traceIfFalse "fee exceeded" feeOk
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    feeOk :: Bool
    feeOk = pdFeeBps pd <= 1000 -- max 10% fee allowed

    invariantOk :: Bool
    invariantOk =
        let oldInv = invariant (pdBalances pd) (pdAmp pd)
            newBalances = pdBalances pd -- placeholder, in practice compute new balances after swap
            newInv = invariant newBalances (pdAmp pd)
        in abs (oldInv - newInv) <= 10 -- tolerance

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @PoolDatum d
        red = unsafeFromBuiltinData @PoolAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes   = Serialise.serialise val
        short   = SBS.toShort (LBS.toStrict bytes)
        builtin = Builtins.toBuiltin (SBS.fromShort short)
    in ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr = C.makeShelleyAddressInEra
                        network
                        (C.PaymentCredentialByScript scriptHash)
                        C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "stable_swap.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- StableSwap AMM Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------"
    putStrLn "StableSwap AMM validator generated successfully."




