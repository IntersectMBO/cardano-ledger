{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module is for compatibility with older GHC. Functions and types below were
-- introduced in ghc-9.2
module Cardano.Ledger.Binary.Decoding.NatsCompat where

import Data.Typeable (type (:~:) (..))
import GHC.TypeLits (CmpNat, KnownNat, natVal)
import Unsafe.Coerce (unsafeCoerce)

data OrderingI a b where
  LTI :: CmpNat a b ~ 'LT => OrderingI a b
  EQI :: CmpNat a a ~ 'EQ => OrderingI a a
  GTI :: CmpNat a b ~ 'GT => OrderingI a b

cmpNat ::
  forall a b proxy1 proxy2.
  (KnownNat a, KnownNat b) =>
  proxy1 a ->
  proxy2 b ->
  OrderingI a b
cmpNat x y = case compare (natVal x) (natVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpNat a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpNat a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpNat a b :~: 'GT) of
    Refl -> GTI
