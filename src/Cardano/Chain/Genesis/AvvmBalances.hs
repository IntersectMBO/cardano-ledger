{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.AvvmBalances
       ( GenesisAvvmBalances (..)
       ) where

import           Cardano.Prelude

import qualified Data.Aeson as Aeson
import           Text.JSON.Canonical
    (FromJSON (..), ToJSON (..))

import           Cardano.Chain.Common
    (Coin)
import           Cardano.Crypto.Signing.Redeem
    (RedeemPublicKey)


-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
  { getGenesisAvvmBalances :: Map RedeemPublicKey Coin
  } deriving (Show, Eq)

instance Monad m => ToJSON m GenesisAvvmBalances where
    toJSON = toJSON . getGenesisAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisAvvmBalances where
    fromJSON = fmap GenesisAvvmBalances . fromJSON

deriving instance Aeson.ToJSON GenesisAvvmBalances
deriving instance Aeson.FromJSON GenesisAvvmBalances
