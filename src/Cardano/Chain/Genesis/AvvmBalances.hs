{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.AvvmBalances
  ( GenesisAvvmBalances(..)
  )
where

import Cardano.Prelude

import qualified Data.Aeson as Aeson
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Chain.Common (Lovelace)
import Cardano.Crypto.Signing.Redeem (RedeemVerificationKey)


-- | Predefined balances of AVVM (Ada Voucher Vending Machine) entries.
-- People who purchased Ada at a pre-sale were issued a certificate during
-- the pre-sale period. These certificates allow customers to redeem ADA.
newtype GenesisAvvmBalances = GenesisAvvmBalances
  { unGenesisAvvmBalances :: Map RedeemVerificationKey Lovelace
  } deriving (Show, Eq, Semigroup)

instance Monad m => ToJSON m GenesisAvvmBalances where
    toJSON = toJSON . unGenesisAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisAvvmBalances where
    fromJSON = fmap GenesisAvvmBalances . fromJSON

deriving instance Aeson.ToJSON GenesisAvvmBalances
deriving instance Aeson.FromJSON GenesisAvvmBalances
