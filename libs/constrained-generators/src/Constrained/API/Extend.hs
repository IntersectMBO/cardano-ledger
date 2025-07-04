{-# LANGUAGE PatternSynonyms #-}

module Constrained.API.Extend (
  module Constrained.API,
  appTerm,
  PredD (..),
  TermD (..),
  HOLE (..),
  SpecificationD (..),
  Logic (..),
  Semantics (..),
  Syntax (..),
  Foldy (..),
  NumSpec (..),
  MaybeBounded (..),
  NonEmpty ((:|)),
  pattern TypeSpec,
  Fun (..),
  HasSpec (..),
  HasSimpleRep (..),
  OrdLike (..),
  SetW (..),
  SetSpec (..),
  pattern Elem,
  pattern ToGeneric,
  pattern FromGeneric,
  pattern Unary,
  pattern (:<:),
  pattern (:>:),
  PairSpec (..),
  MapSpec (..),
  Prod (..),
) where

import Constrained.API
import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.FunctionSymbol
import Constrained.Generic
import Constrained.NumOrd
import Constrained.Spec.Map
import Constrained.Spec.Set
import Constrained.Spec.SumProd
import Constrained.TheKnot
