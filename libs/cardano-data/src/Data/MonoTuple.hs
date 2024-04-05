{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Supports creating tuples (where all components have the same type) as Fixed length lists.
module Data.MonoTuple (
  TupleN,
) where

import GHC.TypeNats (Nat)

-- =========================================

type family TupleN (n :: Nat) a where
  TupleN 0 a = ()
  TupleN 1 a = a
  TupleN 2 a = (a, a)
  TupleN 3 a = (a, a, a)
  TupleN 4 a = (a, a, a, a)
  TupleN 5 a = (a, a, a, a, a)
  TupleN 6 a = (a, a, a, a, a, a)
  TupleN 7 a = (a, a, a, a, a, a, a)
  TupleN 8 a = (a, a, a, a, a, a, a, a)
  TupleN 9 a = (a, a, a, a, a, a, a, a, a)
  TupleN 10 a = (a, a, a, a, a, a, a, a, a, a)
