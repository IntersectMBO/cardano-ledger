{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Spec (
  module Constrained.Spec,
  module X,
) where

import Constrained.Base as X
import Constrained.Instances ()
import Constrained.Spec.Generics as X
import Constrained.Spec.Maps as X
import Constrained.Spec.Pairs as X
import Constrained.Univ

type DefaultFns = '[ListFn, SumFn, MapFn, SetFn, FunFn, GenericsFn, PairFn, IntFn, BoolFn, EqFn, OrdFn, SizeFn]
type Fn = Fix (OneofL DefaultFns)
