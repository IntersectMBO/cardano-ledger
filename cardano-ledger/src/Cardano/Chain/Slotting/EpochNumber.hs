{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Slotting.EpochNumber
  ( EpochNumber(..)
  , isBootstrapEra
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import Data.Data (Data)
import Data.Ix (Ix)
import Formatting (bprint, int)
import Formatting.Buildable (Buildable(..))
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary (FromCBOR(..), ToCBOR(..))


-- | Index of epoch.
newtype EpochNumber = EpochNumber
  { getEpochNumber :: Word64
  } deriving ( Show
             , Data
             , Eq
             , Ord
             , Num
             , Enum
             , Ix
             , Integral
             , Real
             , Generic
             , Bounded
             , NFData
             )

instance Buildable EpochNumber where
  build = bprint ("#" . int)

instance ToCBOR EpochNumber where
  toCBOR (EpochNumber epoch) = toCBOR epoch

instance FromCBOR EpochNumber where
  fromCBOR = EpochNumber <$> fromCBOR

-- Note that it will be encoded as string, because 'EpochNumber' doesn't
-- necessary fit into JS number.
instance Monad m => ToJSON m EpochNumber where
  toJSON = toJSON . getEpochNumber

instance MonadError SchemaError m => FromJSON m EpochNumber where
  fromJSON = fmap EpochNumber . fromJSON

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
  :: EpochNumber
  -- ^ Unlock stake epoch
  -> EpochNumber
  -- ^ Epoch in question (for which we determine whether it belongs to the
  --   bootstrap era)
  -> Bool
isBootstrapEra unlockStakeEpoch epoch = epoch < unlockStakeEpoch
