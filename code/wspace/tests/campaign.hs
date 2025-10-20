import Plutus.Prelude
import Plutus-.V2.Ledger.API
import PlutusTx
import Prelude (Show)
import Plutus.V2.Ledger.Contexts

data CampaignDatum = CampaignDatum {
    beneficiary :: PubkeyHash,
    goal        :: Integer,
    deadline    :: POSIXTime
} deriving (show)

PlutusTx.unstableMakeIsData ''CampaignDatum -- converts data to bytes format (0 and 1s) that can be stored and transmitted -this because cardano doesnt understand haskell data structure it only understands binary data 

data CampaignActions = Donate | Withdraw
    deriving (Show)

PlutusTx.unstableMakeIsData ''CampaignActions -- converts data to bytes format (0 and 1s)

mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
mkCampaignValidator dat red ctx = 
    case red of
        Donate   -> traceIfFalse "To late to donate" canDonate -- if canDonate is not true msg else run canDonate
        Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw

        where 
            info :: TxInfo -- TxInfo is a detailed record that contains all relevant information about the transaction
            info = scriptContextTxInfo ctx --Retrieves scriptContextTxInfo from scriptt context (ctx) which returns TxInfo 

            canDonate :: Bool  -- - is the transaction time with the deadline
            canDonate = to (deadline dat) `contains` (txInfoValidRange info) -- info provides  txInfoValidRange

            canWithdraw :: Bool
            canWithdraw = 
                txSignedBy info (beneficiary dat) && --Check if the transaction was signed by the beneficiary specified in the datum.
                from (deadline dat) `contains` txInfoValidRange info -


-- {-# INLINABLE traceIfFalse #-}
-- traceIfFalse :: BuiltinString -> Bool -> Bool
-- traceIfFalse msg condition =
--     if condition
--         then True
--         else trace msg False

-- data ScriptContext = ScriptContext
--     { scriptContextTxInfo :: TxInfo
--     , scriptContextPurpose :: ScriptPurpose
--     }

-- data TxInfo = TxInfo
--     { txInfoInputs      :: [TxInInfo]
--     , txInfoOutputs     :: [TxOut]
--     , txInfoFee         :: Value
--     , txInfoMint        :: Value
--     , txInfoDCert       :: [DCert]
--     , txInfoWdrl        :: Map StakingCredential Integer
--     , txInfoValidRange  :: POSIXTimeRange
--     , txInfoSignatories :: [PubKeyHash]
--     , txInfoData        :: [(DatumHash, Datum)]
--     , txInfoId          :: TxId
--     }

-- to =>  Creates an interval from -∞ to an upper bound
-- contain => Checks if one interval is within another
-- contains :: Interval a -> Interval a -> Bool

{--
txSignedBy :: TxInfo -> PubKeyHash -> Bool
checks if a transaction is signed by a given key


--}