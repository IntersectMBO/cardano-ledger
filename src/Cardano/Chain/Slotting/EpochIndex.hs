{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Slotting.EpochIndex
       ( EpochIndex (..)
       , isBootstrapEra
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Data.Ix (Ix)
import           Formatting (bprint, int)
import           Formatting.Buildable (Buildable (..))
import           Servant.API (FromHttpApiData)
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..))

import           Cardano.Binary.Class (Bi (..))


-- | Index of epoch.
newtype EpochIndex = EpochIndex
  { getEpochIndex :: Word64
  } deriving ( Show
             , Eq
             , Ord
             , Num
             , Enum
             , Ix
             , Integral
             , Real
             , Generic
             , Bounded
             , Typeable
             , NFData
             )

instance Buildable EpochIndex where
  build = bprint ("#" . int)

instance Bi EpochIndex where
  encode (EpochIndex epoch) = encode epoch
  decode = EpochIndex <$> decode

deriving instance FromHttpApiData EpochIndex

-- Note that it will be encoded as string, because 'EpochIndex' doesn't
-- necessary fit into JS number.
instance Monad m => ToJSON m EpochIndex where
  toJSON = toJSON . getEpochIndex

deriving instance Aeson.FromJSON EpochIndex

deriving instance Aeson.ToJSON EpochIndex

instance MonadError SchemaError m => FromJSON m EpochIndex where
  fromJSON = fmap EpochIndex . fromJSON

-- | Bootstrap era is ongoing until stakes are unlocked. The reward era starts
--   from the epoch specified as the epoch that unlocks stakes:
--
--   @
--                       [unlock stake epoch]
--                               /
--   Epoch: ...  E-3  E-2  E-1   E+0  E+1  E+2  E+3  ...
--          ------------------ | -----------------------
--               Bootstrap era   Reward era
--   @
isBootstrapEra
  :: EpochIndex
  -- ^ Unlock stake epoch
  -> EpochIndex
  -- ^ Epoch in question (for which we determine whether it belongs to the
  --   bootstrap era)
  -> Bool
isBootstrapEra unlockStakeEpoch epoch = epoch < unlockStakeEpoch
