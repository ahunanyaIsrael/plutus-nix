{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import PlutusTx
import PlutusTx.Prelude (traceIfFalse, BuiltinString, Bool(..), ($), Integer, (&&))
import Plutus.V2.Ledger.Api 
    ( Validator
    , mkValidatorScript
    , PubKeyHash
    , POSIXTime
    , ScriptContext
    , TxInfo
    , txInfoValidRange
    )      
import Plutus.V2.Ledger.Contexts ( txSignedBy)   
import Prelude (Show, IO, putStrLn)
import Plutus.V1.Ledger.Interval (contains, to, from)
import PlutusTx.Builtins (toBuiltin)

-- Every doner gets NFTs/benefits based on their level of donation

--  Compaign Donation for specified beneficiaries
data CampaignDatum = CampaignDatum {
    beneficiary :: PubKeyHash,
    goal        :: Integer,
    deadline    :: POSIXTime
} deriving (Show)

PlutusTx.unstableMakeIsData ''CampaignDatum -- converts data to bytes format (0 and 1s) that can be stored and transmitted -this because cardano doesnt understand haskell data structure it only understands binary data 

data CampaignActions = Donate | Withdraw | Refund 
    deriving (Show)

PlutusTx.unstableMakeIsData ''CampaignActions -- converts data to bytes format (0 and 1s)

mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
mkCampaignValidator dat red ctx = 
    case red of
        Donate   -> traceIfFalse (toBuiltin "Too late to donate") canDonate -- if canDonate is not true msg else run canDonate
        Withdraw -> traceIfFalse (toBuiltin "Not allowed to withdraw") canWithdraw
        Refund   -> traceIfFalse "Refund fail" refundCheck

        where 
            info :: TxInfo -- TxInfo is a detailed record that contains all relevant information about the transaction
            info = scriptContextTxInfo ctx --Retrieves scriptContextTxInfo from scriptt context (ctx) which returns TxInfo 

            canDonate :: Bool  -- - is the transaction time with the deadline
            canDonate = to (deadline dat) `contains` (txInfoValidRange info) -- info provides  txInfoValidRange

            canWithdraw :: Bool
            canWithdraw = 
                txSignedBy info (beneficiary dat) && --Check if the transaction was signed by the beneficiary specified in the datum.
                from (deadline dat) `contains` txInfoValidRange info

            refundCheck :: Bool
            refundCheck = True 

wrap :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
wrap = mkCampaignValidator

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrap ||])



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

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}

-- module Main where

-- import Types
-- import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode)
-- import PlutusTx.Builtins.Class (stringToBuiltinString)
-- import Plutus.V2.Ledger.Contexts (txSignedBy, ScriptContext)
-- import Plutus.V2.Ledger.Api (TxInfo, scriptContextTxInfo, Validator, mkValidatorScript,BuiltinData)
-- import Plutus.V1.Ledger.Interval (contains, to, from)
-- import PlutusTx.Prelude (traceIfFalse, Bool(..), (&&))
-- import Prelude (Show, IO, putStrLn)



-- {-# INLINABLE mkCampaignValidator #-}
-- mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- mkCampaignValidator dat red ctx =
--     case red of
--         Donate   -> traceIfFalse (stringToBuiltinString "Too late to donate") canDonate
--         Withdraw -> traceIfFalse (stringToBuiltinString "Not allowed to withdraw") canWithdraw
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     canDonate :: Bool
--     canDonate = to (deadline dat) `contains` txInfoValidRange info

--     canWithdraw :: Bool
--     canWithdraw =
--       txSignedBy info (beneficiary dat) &&
--       from (deadline dat) `contains` txInfoValidRange info

-- {-# INLINABLE wrap #-}
-- wrap :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
-- wrap = mkCampaignValidator

-- compiledCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- compiledCode = $$(PlutusTx.compile [||
--     \d r c -> wrap
--         (PlutusTx.unsafeFromBuiltinData d)
--         (PlutusTx.unsafeFromBuiltinData r)
--         (PlutusTx.unsafeFromBuiltinData c)
--     ||])

-- validator :: Validator
-- validator = mkValidatorScript compiledCode

-- main :: IO ()
-- main = putStrLn "Campaign validator compiled successfully!"
