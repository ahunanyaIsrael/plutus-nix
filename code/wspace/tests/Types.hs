{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where
import Prelude (Show)
import PlutusTx
import PlutusTx.Prelude (Bool(..), ($), (&&), Integer)
import Plutus.V2.Ledger.Api (PubKeyHash, POSIXTime)

-- Campaign Datum
data CampaignDatum = CampaignDatum
  { beneficiary :: PubKeyHash
  , goal        :: Integer
  , deadline    :: POSIXTime
  } deriving (Show)

PlutusTx.unstableMakeIsData ''CampaignDatum

-- Campaign Actions
data CampaignActions = Donate | Withdraw
  deriving Show

PlutusTx.unstableMakeIsData ''CampaignActions
