{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.HKD (
  HKD,
  HKDNoUpdate,
  HKDFunctor (..),
  NoUpdate (..),
)
where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- ====================================================================

-- | Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data NoUpdate a = NoUpdate
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (NoUpdate a)

instance NFData (NoUpdate a)

type family HKDNoUpdate f a where
  HKDNoUpdate Identity a = a
  HKDNoUpdate StrictMaybe a = NoUpdate a
  HKDNoUpdate Maybe a = NoUpdate a
  HKDNoUpdate f a = f a

class HKDFunctor f where
  hkdMap :: proxy f -> (a -> b) -> HKD f a -> HKD f b
  toNoUpdate :: HKD f a -> HKDNoUpdate f a
  fromNoUpdate :: HKDNoUpdate f a -> HKD f a

instance HKDFunctor Identity where
  hkdMap _ f = f
  toNoUpdate = id
  fromNoUpdate = id

instance HKDFunctor Maybe where
  hkdMap _ = fmap
  toNoUpdate _ = NoUpdate
  fromNoUpdate _ = Nothing

instance HKDFunctor StrictMaybe where
  hkdMap _ = fmap
  toNoUpdate _ = NoUpdate
  fromNoUpdate _ = SNothing
