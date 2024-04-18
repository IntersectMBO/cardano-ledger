{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Constrained.TypeErrors
  ( Reduces
  , module GHC.TypeLits
  , module Data.Kind
  ) where

import Data.Kind
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- NOTE: This trick is taken from this blog post:
-- https://blog.csongor.co.uk/report-stuck-families/
data Dummy
type family Reduces (err :: Constraint) rep :: Constraint where
  Reduces _ Dummy = ((), ())
  Reduces _ _ = ()
