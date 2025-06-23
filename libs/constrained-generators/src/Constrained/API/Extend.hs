{-# LANGUAGE PatternSynonyms #-}

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

  -- * Re-export of `Constrained.API`
  module Constrained.API,
) where

import Constrained.API
import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.FunctionSymbol
import Constrained.NumOrd
import Constrained.Spec.Map
import Constrained.Spec.Set
import Constrained.Spec.SumProd
import Constrained.Spec.Tree
