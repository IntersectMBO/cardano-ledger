{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Constrained.TypeErrors (
  AssertComputes,
  module X,
) where

import Data.Kind
#if MIN_VERSION_base(4,17,0)
import GHC.TypeError as X
#else
import GHC.TypeLits as X
#endif

-- NOTE: this module implementes this very neat little trick for observing when type
-- families are stuck https://blog.csongor.co.uk/report-stuck-families/ which allows
-- us to report much better type errors when our generics tricks fail.
--
-- The idea of this type family is that if `ty` evaluates to a type (other than Dummy which
-- we haven't exported) then `Computes ty (TE err)` will evaluate to `()` without
-- getting stuck and without expanding `TE` to `TypeError err`.
--
-- If, on the other hand, GHC gets stuck evaluating `ty` it will (hopefully) try to normalize
-- everything and (hopefully) end up with `Computes (TypeError err) ty` which in turn will cause
-- it to throw `err` as a type error.
--
-- Now, the important thing here is that you can't do `Computes _ _ = ()` because that doesn't
-- force the evaluation of `ty` and consequently doesn't end up with GHC wanting to report
-- that `Computes tyThatDoesntCompute (TE err)` fails and consequently normalizing `TE err`
-- and finally arriving at `TypeError err`.
type family Computes (ty :: Type) (err :: Constraint) :: Constraint where
  Computes Dummy _ =
    TypeError
      (Text "This shouldn't be reachable because " :<>: ShowType Dummy :<>: Text " shouldn't be exported!")
  Computes _ _ = ()

-- This is intentionally hidden in here to avoid any funny business
data Dummy

-- NOTE: this indirection is necessary only for older versions of GHC where this is
-- "the right" amount of strictness - apparently it's not necessary on newer versions of GHC!
-- The alternative would be to just do `AssertComputes ty em = Computes ty (TypeError em)` below
-- and that works fine on e.g. ghc 9.12 (and other recent versions) but fails on 8.10.7 because
-- GHC is "too eager" to throw the type error.
type family TE em where
  TE em = TypeError em

type AssertComputes ty em = Computes ty (TE em)
