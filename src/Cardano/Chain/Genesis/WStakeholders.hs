{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.WStakeholders
       ( GenesisWStakeholders (..)
       ) where

import           Cardano.Prelude

import           Control.Monad.Except
    (MonadError)
import qualified Data.Aeson as Aeson
    (FromJSON, ToJSON)
import           Formatting
    (bprint)
import           Formatting.Buildable
    (Buildable (..))
import           Text.JSON.Canonical
    (FromJSON (..), ToJSON (..))

import           Cardano.Chain.Common
    (StakeholderId)


-- | Wrapper around weighted stakeholders map to be used in genesis core data
--
--   Each 'Word16' is a weight. I.e. if stakeholder A has weight "1" and
--   stakeholder B has weight "3", during the bootstrap era all stake in the
--   system will be divided between A and B in proportion of 1:3.
newtype GenesisWStakeholders = GenesisWStakeholders
  { getGenesisWStakeholders :: Map StakeholderId Word16
  } deriving (Show, Eq, Semigroup, Monoid)

instance Buildable GenesisWStakeholders where
  build (GenesisWStakeholders m) =
    bprint ("GenesisWStakeholders: " . mapJson) m

instance Monad m => ToJSON m GenesisWStakeholders where
  toJSON (GenesisWStakeholders stks) = toJSON stks

instance MonadError SchemaError m => FromJSON m GenesisWStakeholders where
  fromJSON = fmap GenesisWStakeholders . fromJSON

deriving instance Aeson.ToJSON GenesisWStakeholders

deriving instance Aeson.FromJSON GenesisWStakeholders
