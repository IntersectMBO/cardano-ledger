{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.KeyHashes
  ( GenesisKeyHashes(..)
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Formatting (bprint)
import Formatting.Buildable (Buildable(..))
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Chain.Common (KeyHash)


-- | The set of genesis keys, who are able to produce blocks and submit votes
--   and proposals in the Byron era
newtype GenesisKeyHashes = GenesisKeyHashes
  { unGenesisKeyHashes :: Set KeyHash
  } deriving (Show, Eq, Semigroup, Monoid)

instance Buildable GenesisKeyHashes where
  build (GenesisKeyHashes m) =
    bprint ("GenesisKeyHashes: " . listJson) (Set.toList m)

instance Monad m => ToJSON m GenesisKeyHashes where
  toJSON (GenesisKeyHashes stks) =
    toJSON . M.fromList $ zip (Set.toList stks) (repeat (1 :: Word16))

instance MonadError SchemaError m => FromJSON m GenesisKeyHashes where
  fromJSON =
    fmap (GenesisKeyHashes . M.keysSet) . fromJSON @m @(Map KeyHash Word16)
