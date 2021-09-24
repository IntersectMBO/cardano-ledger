{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missed-specialisations #-}

module Cardano.Chain.Genesis.KeyHashes
  ( GenesisKeyHashes (..),
  )
where

import Cardano.Binary
import Cardano.Chain.Common (KeyHash)
import Cardano.Prelude
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Formatting (bprint)
import Formatting.Buildable (Buildable (..))
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | The set of genesis keys, who are able to produce blocks and submit votes
--   and proposals in the Byron era
newtype GenesisKeyHashes = GenesisKeyHashes
  { unGenesisKeyHashes :: Set KeyHash
  }
  deriving (Show, Eq, Semigroup, Monoid, NoThunks)

instance Buildable GenesisKeyHashes where
  build (GenesisKeyHashes m) =
    bprint ("GenesisKeyHashes: " . listJson) (Set.toList m)

instance Monad m => ToJSON m GenesisKeyHashes where
  toJSON (GenesisKeyHashes stks) =
    toJSON . M.fromList $ zip (Set.toList stks) (repeat (1 :: Word16))

instance MonadError SchemaError m => FromJSON m GenesisKeyHashes where
  fromJSON =
    fmap (GenesisKeyHashes . M.keysSet) . fromJSON @m @(Map KeyHash Word16)

instance ToCBOR GenesisKeyHashes where
  toCBOR (GenesisKeyHashes gkh) = encodeListLen 1 <> toCBOR @(Set KeyHash) gkh

instance FromCBOR GenesisKeyHashes where
  fromCBOR = do
    enforceSize "GenesisKeyHashes" 1
    GenesisKeyHashes <$> fromCBOR @(Set KeyHash)
