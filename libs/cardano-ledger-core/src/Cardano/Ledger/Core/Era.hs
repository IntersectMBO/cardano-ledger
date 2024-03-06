{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Core.Era (
  -- * Era
  Era (..),
  ByronEra,

  -- ** Rules
  EraRule,
  EraRuleFailure,
  EraRuleEvent,
  VoidEraRule,
  absurdEraRule,
  InjectRuleFailure (..),
  InjectRuleEvent (..),

  -- ** Protocol Version
  AtMostEra,
  AtLeastEra,
  ExactEra,
  ProtVerAtMost,
  ProtVerAtLeast,
  ProtVerInBounds,
  atLeastEra,
  atMostEra,
  notSupportedInThisEra,
  notSupportedInThisEraL,
  eraProtVerLow,
  eraProtVerHigh,
  toEraCBOR,
  fromEraCBOR,
  fromEraShareCBOR,
  eraDecoder,
)
where

import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto
import Control.DeepSeq (NFData (..))
import Control.State.Transition.Extended (PredicateFailure, STS (..))
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Lens.Micro

--------------------------------------------------------------------------------
-- Era
--------------------------------------------------------------------------------

class
  ( Crypto (EraCrypto era)
  , Typeable era
  , KnownNat (ProtVerLow era)
  , KnownNat (ProtVerHigh era)
  , ProtVerLow era <= ProtVerHigh era
  , MinVersion <= ProtVerLow era
  , MinVersion <= ProtVerHigh era
  , -- We need to make sure that there is never a case that MaxVersion equals to the highest
    -- protocol version for the era, otherwise we can never upgrade to the next version:
    CmpNat (ProtVerLow era) MaxVersion ~ 'LT
  , CmpNat (ProtVerHigh era) MaxVersion ~ 'LT
  , -- These two are redundant and can be removed once support for GHC-8.10 is dropped:
    ProtVerLow era <= MaxVersion
  , ProtVerHigh era <= MaxVersion
  ) =>
  Era era
  where
  type EraCrypto era :: Type

  -- | Map an era to its predecessor.
  --
  -- For example:
  --
  -- > type instance PreviousEra (AllegraEra c) = ShelleyEra c
  type PreviousEra era = (r :: Type) | r -> era

  -- | Lowest major protocol version for this era
  type ProtVerLow era :: Nat

  -- | Highest major protocol version for this era. By default se to `ProtVerLow`
  type ProtVerHigh era :: Nat

  type ProtVerHigh era = ProtVerLow era

  -- | Textual name of the current era.
  --
  -- Designed to be used with @TypeApplications@:
  --
  -- >>> eraName @(ByronEra StandardCrypto)
  -- Byron
  eraName :: String

-- | This is the era that preceded Shelley era. It cannot have any other class instances,
-- except for `Era` type class.
data ByronEra c

-- | This is a non-existent era and is defined for satisfying the `PreviousEra` type family injectivity
data VoidEra c

instance Crypto c => Era (ByronEra c) where
  type EraCrypto (ByronEra c) = c
  type PreviousEra (ByronEra c) = VoidEra c
  type ProtVerLow (ByronEra c) = 0
  type ProtVerHigh (ByronEra c) = 1

  eraName = "Byron"

-----------------------------
-- Rules --------------------
-----------------------------

-- | Era STS map
type family EraRule (rule :: Symbol) era = (r :: Type) | r -> rule

-- | `EraRuleFailure` type family is needed for injectivity, which STS' `PredicateFailure`
-- does not provide for us unfortunately.
type family EraRuleFailure (rule :: Symbol) era = (r :: Type) | r -> rule era

type family EraRuleEvent (rule :: Symbol) era = (r :: Type) | r -> rule era

-- | This is a type with no inhabitans for the rules. It is used to indicate that a rule
-- does not have a predicate failure as well as marking rules that have been disabled when
-- comparing to prior eras.
data VoidEraRule (rule :: Symbol) era
  deriving (Show, Eq, Ord)

instance NFData (VoidEraRule (rule :: Symbol) era) where
  rnf = absurdEraRule

instance (KnownSymbol rule, Era era) => ToCBOR (VoidEraRule (rule :: Symbol) era) where
  toCBOR = absurdEraRule

instance (KnownSymbol rule, Era era) => EncCBOR (VoidEraRule (rule :: Symbol) era)

instance (KnownSymbol rule, Era era) => FromCBOR (VoidEraRule (rule :: Symbol) era) where
  fromCBOR = cborError DecoderErrorVoid

instance (KnownSymbol rule, Era era) => DecCBOR (VoidEraRule (rule :: Symbol) era)

absurdEraRule :: VoidEraRule rule era -> a
absurdEraRule a = case a of {}

-- Rules that should never have a predicate failures
type instance EraRuleFailure "EPOCH" era = VoidEraRule "EPOCH" era
type instance EraRuleFailure "NEWEPOCH" era = VoidEraRule "NEWEPOCH" era
type instance EraRuleFailure "MIR" era = VoidEraRule "MIR" era
type instance EraRuleFailure "NEWPP" era = VoidEraRule "NEWPP" era
type instance EraRuleFailure "SNAP" era = VoidEraRule "SNAP" era
type instance EraRuleFailure "TICK" era = VoidEraRule "TICK" era
type instance EraRuleFailure "TICKF" era = VoidEraRule "TICKF" era
type instance EraRuleFailure "UPEC" era = VoidEraRule "UPEC" era
type instance EraRuleFailure "RUPD" era = VoidEraRule "RUPD" era
type instance EraRuleFailure "POOLREAP" era = VoidEraRule "POOLREAP" era

class
  EraRuleFailure rule era ~ PredicateFailure (EraRule rule era) =>
  InjectRuleFailure (rule :: Symbol) t era
  where
  injectFailure :: t era -> EraRuleFailure rule era
  default injectFailure :: t era ~ EraRuleFailure rule era => t era -> EraRuleFailure rule era
  injectFailure = id

class
  EraRuleEvent rule era ~ Event (EraRule rule era) =>
  InjectRuleEvent (rule :: Symbol) t era
  where
  injectEvent :: t era -> EraRuleEvent rule era
  default injectEvent :: t era ~ EraRuleEvent rule era => t era -> EraRuleEvent rule era
  injectEvent = id

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

-- | Get the value level `Version` of the lowest major protocol version for the supplied @era@.
eraProtVerLow :: forall era. Era era => Version
eraProtVerLow = natVersion @(ProtVerLow era)

-- | Get the value level `Version` of the highest major protocol version for the supplied @era@.
eraProtVerHigh :: forall era. Era era => Version
eraProtVerHigh = natVersion @(ProtVerHigh era)

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

-- | Convert a type that implements `EncCBOR` to plain `Plain.Encoding` using the lowest
-- protocol version for the supplied @era@
toEraCBOR :: forall era t. (Era era, EncCBOR t) => t -> Plain.Encoding
toEraCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR
{-# INLINE toEraCBOR #-}

-- | Convert a type that implements `DecCBOR` to plain `Plain.Decoder` using the lowest
-- protocol version for the supplied @era@
fromEraCBOR :: forall era t s. (Era era, DecCBOR t) => Plain.Decoder s t
fromEraCBOR = eraDecoder @era decCBOR
{-# INLINE fromEraCBOR #-}

-- | Convert a type that implements `DecShareCBOR` to plain `Plain.Decoder` using the lowest
-- protocol version for the supplied @era@
fromEraShareCBOR :: forall era t s. (Era era, DecShareCBOR t) => Plain.Decoder s t
fromEraShareCBOR = eraDecoder @era decNoShareCBOR
{-# INLINE fromEraShareCBOR #-}

-- | Convert a versioned `Decoder` to plain a `Plain.Decoder` using the lowest protocol
-- version for the supplied @era@
eraDecoder :: forall era t s. Era era => Decoder s t -> Plain.Decoder s t
eraDecoder = toPlainDecoder (eraProtVerLow @era)
{-# INLINE eraDecoder #-}
