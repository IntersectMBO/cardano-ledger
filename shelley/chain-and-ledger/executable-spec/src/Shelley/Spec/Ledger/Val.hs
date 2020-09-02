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
--
-- This module is intended for qualified import:
-- > import qualified Shelley.Spec.Ledger.Val as Val
module Shelley.Spec.Ledger.Val
  ( Val (..),
    (~~),

    -- * Re-exports
    Data.Group.invert,
    (Data.PartialOrd.<=),
    (Data.PartialOrd.>=),
    (Data.PartialOrd.==),
    (Data.PartialOrd./=),
    (Data.PartialOrd.>),
    (Data.PartialOrd.<),
    Data.PartialOrd.compare,
  )
where

import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))
import Data.Group (Abelian, Group (invert))
import Data.PartialOrd hiding ((==))
import qualified Data.PartialOrd
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Coin (Coin (..))

class
  ( Abelian t,
    Eq t,
    PartialOrd t,
    -- Do we really need these?
    Show t,
    Typeable t,
    NFData t,
    NoUnexpectedThunks t
  ) =>
  Val t
  where
  -- | Multiply the value by a scalar
  scale :: Integral i => i -> t -> t

  -- | Is the argument zero?
  isZero :: t -> Bool
  isZero t = t == mempty

  -- | Get the ADA present in the value (since ADA is our "blessed" currency)
  coin :: t -> Coin

  -- | Create a value containing only this amount of ADA
  inject :: Coin -> t

  size :: t -> Integer -- compute size of Val instance
  -- TODO add PACK/UNPACK stuff to this class

-- | Group subtraction. When we move to groups-0.5 we can export this from
-- there.
(~~) :: Group g => g -> g -> g
a ~~ b = a <> invert b

instance Val Coin where
  scale n (Coin x) = Coin $ (fromIntegral n) * x
  coin = id
  inject = id
  size _ = 1
