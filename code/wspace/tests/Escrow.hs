{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, FilePath, putStrLn, print, (<>))
import qualified Prelude as P

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import Cardano.Api
    ( writeFileTextEnvelope
    , displayError
    )
import Cardano.Api.Shelley
    ( PlutusScript(..)
    , PlutusScriptV2
    )

-- Plutus
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V1.Ledger.Interval (Interval, contains, to, from)
import Plutus.V1.Ledger.Api
    ( PubKeyHash
    , POSIXTime
    , CurrencySymbol
    , TokenName
    )
import Plutus.V2.Ledger.Api
    ( Validator
    , BuiltinData
    , mkValidatorScript
    , unValidatorScript
    , ScriptContext
    , TxInfo
    , txInfoValidRange
    , adaSymbol
    , adaToken
    , txOutValue
    )
import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    , txInInfoOutRef             
    , txInInfoResolved   
    , findOwnInput  
    , scriptContextTxInfo    
    )
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

------------------------------------------------------------------------
-- Datum + Redeemer
------------------------------------------------------------------------

data EscrowDatum = EscrowDatum
    { edBuyer    :: PubKeyHash
    , edSeller   :: PubKeyHash
    , edAmount   :: Integer
    , edDeadline :: POSIXTime
    , edCurrency :: CurrencySymbol
    , edToken    :: TokenName
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowAction = PaySeller | RefundSeller
PlutusTx.unstableMakeIsData ''EscrowAction

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsNFT #-}
scriptInputContainsNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found!!"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      PaySeller ->
           traceIfFalse "script input missing NFT!!" (scriptInputContainsNFT ctx (edCurrency dat) (edToken dat)) &&
           traceIfFalse "buyer signature missing!!"   (txSignedBy info (edBuyer dat)) &&
           traceIfFalse "seller not paid"           sellerPaid &&
           traceIfFalse "buyer not receive NFT"     buyerGetsNFT

      RefundSeller ->
           traceIfFalse "script input missing NFT" (scriptInputContainsNFT ctx (edCurrency dat) (edToken dat)) &&
           traceIfFalse "seller signature missing" (txSignedBy info (edSeller dat)) &&
           traceIfFalse "too early for refund"     afterDeadline &&
           traceIfFalse "seller did not receive NFT" sellerGetsNFT

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: Interval POSIXTime
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline =
        contains (from (edDeadline dat + 1)) txRange

    sellerPaid :: Bool
    sellerPaid =
      let v = valuePaidTo info (edSeller dat)
      in valueOf v adaSymbol adaToken >= edAmount dat

    buyerGetsNFT :: Bool
    buyerGetsNFT =
      let v = valuePaidTo info (edBuyer dat)
      in valueOf v (edCurrency dat) (edToken dat) >= 1

    sellerGetsNFT :: Bool
    sellerGetsNFT =
      let v = valuePaidTo info (edSeller dat)
      in valueOf v (edCurrency dat) (edToken dat) >= 1

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------


{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData d
        red = unsafeFromBuiltinData r
        ctx = unsafeFromBuiltinData c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])


--------------------------------------------------------------------------------
-- WRITE .plutus FILE (Correct modern version)
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let script = unValidatorScript val
        bs     = serialise script
        sh     = SBS.toShort (LBS.toStrict bs)
        plutus = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2

    result <- writeFileTextEnvelope file Nothing plutus
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote script to: " <> file)

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------


main :: IO ()
main = do
    putStrLn "Compiled Escrow NFT validator!"
    writeValidator "./assets/escrow-nft.plutus" validator

