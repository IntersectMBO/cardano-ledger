{-# LANGUAGE PatternSynonyms #-}

-- | This module provides an API for extending the library with new function
-- symbols.
module Constrained.API.Extend (
  -- * Abstract syntax
  SpecificationD (..),
  pattern TypeSpec,
  PredD (..),
  TermD (..),
  BinderD (..),

  -- * Implementing new functions
  appTerm,
  Semantics (..),
  Syntax (..),

  -- ** The `Logic` instance
  Logic (..),
  HOLE (..),
  pattern Unary,
  pattern (:<:),
  pattern (:>:),

  -- ** Built-in 'TypeSpec's
  PairSpec (..),
  MapSpec (..),
  SetSpec (..),
  NumSpec (..),
  TreeSpec (..),

  -- * Generics
  (:::),
  SOP,
  algebra,
  inject,

  -- * Building new `NumSpec`-based instances
  emptyNumSpec,
  cardinalNumSpec,
  combineNumSpec,
  genFromNumSpec,
  shrinkWithNumSpec,
  conformsToNumSpec,
  toPredsNumSpec,
  MaybeBounded (..),

  -- * Re-export of `Constrained.API`
  module Constrained.API,
) where

import Constrained.API
import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.FunctionSymbol
import Constrained.Generic
import Constrained.NumOrd
import Constrained.Spec.Map
import Constrained.Spec.Set
import Constrained.Spec.Tree
import Constrained.TheKnot
