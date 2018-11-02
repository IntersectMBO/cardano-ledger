{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Chain.Genesis.AvvmBalances
       ( GenesisAvvmBalances (..)
       ) where

import           Cardano.Prelude

import           Data.Aeson
    (FromJSON (..), ToJSON (..))

import           Cardano.Chain.Common
    (Coin)
import           Cardano.Crypto.Signing.Redeem
    (RedeemPublicKey)

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
  { getGenesisAvvmBalances :: Map RedeemPublicKey Coin
  } deriving (Show, Eq)

deriving instance ToJSON GenesisAvvmBalances
deriving instance FromJSON GenesisAvvmBalances
