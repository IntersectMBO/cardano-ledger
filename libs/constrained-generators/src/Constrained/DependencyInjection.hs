{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Constrained.DependencyInjection where

import Data.Kind

-- In this module we introduce the `Dependencies` class which is intended to
-- collect type classes and type families that are necessary in the abstract
-- syntax of terms, predicates, and specifications but which we don't want to
-- define in the same place. This is typically because the type classes have
-- large default instances that mean the type classes themselves need a lot
-- of code before we can define them. By making these classes abstract in the
-- GADTs we avoid the code-base blowing up with a lot of interdependencies.

-- The `Dependencies` class will eventually only be instantiated once by an
-- uninhabited type `data Deps`.

-- NOTE TO SELF: it may be necessary / nice to introduce some functions here
-- too if it makes life easier.
class Dependencies d where
  type HasSpecD d :: Type -> Constraint
  type TypeSpecD d :: Type -> Type
  type LogicD d :: ([Type] -> Type -> Type) -> Constraint
  type ForallableD d :: Type -> Type -> Constraint
  type HasGenHintD d :: Type -> Constraint
  type HintD d :: Type -> Type
