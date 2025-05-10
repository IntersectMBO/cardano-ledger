{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.DependencyInjection where

import Data.Kind

-- NOTE TO SELF: it may be necessary / nice to introduce some functions here
-- too if it makes life easier.
class Dependencies d where
  type HasSpecD d :: Type -> Constraint
  type TypeSpecD d :: Type -> Type
  type LogicD d :: ([Type] -> Type -> Type) -> Constraint
  type ForallableD d :: Type -> Type -> Constraint
  type HasGenHintD d :: Type -> Constraint
  type HintD d :: Type -> Type
