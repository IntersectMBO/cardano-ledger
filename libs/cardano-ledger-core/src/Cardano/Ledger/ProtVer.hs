{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.ProtVer 
  ( AtMostEra,
    AtLeastEra,
    ExactEra,
    ProtVerAtMost,
    ProtVerAtLeast,
    ProtVerInBounds,
    atLeastEra,
    atMostEra,
    notSupportedInThisEra,
    notSupportedInThisEraL
  ) where

import Data.Kind (Constraint, Type)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Lens.Micro
import Cardano.Ledger.Era.Class (Era(..))

-----------------------------
-- Protocol version bounds --
-----------------------------

type family ProtVerIsInBounds (check :: Symbol) era (v :: Nat) (b :: Bool) :: Constraint where
  ProtVerIsInBounds check era v 'True = ()
  ProtVerIsInBounds check era v 'False =
    TypeError
      ( 'ShowType era
          ':<>: 'Text " protocol version bounds are: ["
          ':<>: 'ShowType (ProtVerLow era)
          ':<>: 'Text ", "
          ':<>: 'ShowType (ProtVerHigh era)
          ':<>: 'Text "], but required is "
          ':<>: 'Text check
          ':<>: 'Text " "
          ':<>: 'ShowType v
      )

-- | Requirement for the era's highest protocol version to be higher or equal to
-- the supplied value
type family ProtVerAtLeast era (l :: Nat) :: Constraint where
  ProtVerAtLeast era l = ProtVerIsInBounds "at least" era l (l <=? ProtVerHigh era)

-- | Requirement for the era's lowest protocol version to be lower or equal to
-- the supplied value
type family ProtVerAtMost era (h :: Nat) :: Constraint where
  ProtVerAtMost era h = ProtVerIsInBounds "at most" era h (ProtVerLow era <=? h)

-- | Restrict a lower and upper bounds of the protocol version for the particular era
type ProtVerInBounds era l h = (ProtVerAtLeast era l, ProtVerAtMost era h)

-- | Restrict an era to the specific era through the protocol version. This is
-- equivalent to @(inEra (Crypto era) ~ era)@
type ExactEra (inEra :: Type -> Type) era =
  ProtVerInBounds era (ProtVerLow (inEra (EraCrypto era))) (ProtVerHigh (inEra (EraCrypto era)))

-- | Restrict the @era@ to equal to @eraName@ or come after it
type AtLeastEra (eraName :: Type -> Type) era =
  ProtVerAtLeast era (ProtVerLow (eraName (EraCrypto era)))

-- | Restrict the @era@ to equal to @eraName@ or come before it.
type AtMostEra (eraName :: Type -> Type) era =
  ProtVerAtMost era (ProtVerHigh (eraName (EraCrypto era)))

-- | Enforce era to be at least the specified era at the type level. In other words
-- compiler will produce type error when applied to eras prior to the specified era.
-- This function should be used in order to avoid redundant constraints warning.
--
-- For example these will type check
--
-- >>> atLeastEra @BabbageEra @(ConwayEra StandardCrypto)
-- >>> atLeastEra @BabbageEra @(BabbageEra StandardCrypto)
--
-- However this will result in a type error
--
-- >>> atLeastEra @BabbageEra @(AlonzoEra StandardCrypto)
atLeastEra :: AtLeastEra eraName era => ()
atLeastEra = ()

-- | Enforce era to be at most the specified era at the type level. In other words
-- compiler will produce type error when applied to eras prior to the specified era.
-- This function should be used in order to avoid redundant constraints warning.
--
-- For example these will type check
--
-- >>> atMostEra @BabbageEra @(ShelleyEra StandardCrypto)
-- >>> atMostEra @AlonzoEra @(MaryEra StandardCrypto)
--
-- However this will result in a type error
--
-- >>> atMostEra @BabbageEra @(ConwayEra StandardCrypto)
atMostEra :: AtMostEra eraName era => ()
atMostEra = ()

notSupportedInThisEra :: HasCallStack => a
notSupportedInThisEra = error "Impossible: Function is not supported in this era"

-- Without using `lens` we hit a ghc bug, which results in a redundant constraint warning
notSupportedInThisEraL :: HasCallStack => Lens' a b
notSupportedInThisEraL = lens notSupportedInThisEra notSupportedInThisEra
