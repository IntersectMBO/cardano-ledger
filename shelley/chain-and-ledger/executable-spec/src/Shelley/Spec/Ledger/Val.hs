{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output.
module Shelley.Spec.Ledger.Val where

import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))
import Data.Group (Abelian)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Coin (Coin (..))

data Comparison = Gt | Lt | Gteq | Lteq | Neq | Equal

class
  ( Abelian t,
    Eq t,
    -- Do we really need these?
    Show t,
    Typeable t,
    NFData t,
    NoUnexpectedThunks t
  ) =>
  Val t
  where
  -- | TODO This needs documenting. what is it?
  scalev :: Integer -> t -> t

  -- | Compare two values. Note that we only have a partial ordering; two values
  -- may not necessarily be comparible, in which case `False` will be returned
  -- for all comparisons.
  compare :: Comparison -> t -> t -> Bool

  -- | Is the argument zero?
  isZero :: t -> Bool
  isZero t = t == mempty

  -- | Get the ADA present in the value (since ADA is our "blessed" currency)
  coin :: t -> Coin

  -- | Create a value containing only this amount of ADA
  inject :: Coin -> t

  size :: t -> Integer -- compute size of Val instance
  -- TODO add PACK/UNPACK stuff to this class

instance Val Coin where
  scalev n (Coin x) = Coin $ n * x
  compare Gt x y = x > y
  compare Lt x y = x < y
  compare Gteq x y = x >= y
  compare Lteq x y = x <= y
  compare Neq x y = not (x == y)
  compare Equal x y = x == y
  coin = id
  inject = id
  size _ = 1
