{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.HKD
  ( HKD,
    HKDFunctor (..),
  )
where

import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe (..))

-- ====================================================================

-- | Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

class HKDFunctor f where
  hkdMap :: proxy f -> (a -> b) -> HKD f a -> HKD f b

instance HKDFunctor Identity where
  hkdMap _ f a = f a

instance HKDFunctor Maybe where
  hkdMap _ f = fmap f

instance HKDFunctor StrictMaybe where
  hkdMap _ f = fmap f
