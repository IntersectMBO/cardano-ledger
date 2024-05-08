{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Core.Binary.RoundTrip (
  RuleListEra (..),
  EraRuleProof (..),

  -- * Spec
  roundTripEraSpec,
  roundTripAnnEraSpec,
  roundTripEraTypeSpec,
  roundTripAnnEraTypeSpec,
  roundTripShareEraSpec,
  roundTripShareEraTypeSpec,

  -- * Expectation
  roundTripEraExpectation,
  roundTripEraTypeExpectation,
  roundTripAnnEraExpectation,
  roundTripAnnEraTypeExpectation,
  roundTripShareEraExpectation,
  roundTripShareEraTypeExpectation,
  roundTripCoreEraTypesSpec,
  roundTripAllPredicateFailures,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.CertState
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Keys.Bootstrap
import Cardano.Ledger.UTxO
import Control.State.Transition.Extended (STS (..))
import Data.Typeable
import GHC.TypeLits (Symbol)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

-- | QuickCheck property spec that uses `roundTripEraExpectation`
roundTripEraSpec ::
  forall era t.
  (Era era, Show t, Eq t, EncCBOR t, DecCBOR t, Arbitrary t, HasCallStack) =>
  Spec
roundTripEraSpec =
  prop (show (typeRep $ Proxy @t)) $ roundTripEraExpectation @era @t

-- | Roundtrip CBOR testing for types and type families that implement
-- EncCBOR/DecCBOR. Requires TypeApplication of an @@era@
roundTripEraExpectation ::
  forall era t.
  (Era era, Show t, Eq t, EncCBOR t, DecCBOR t, HasCallStack) =>
  t ->
  Expectation
roundTripEraExpectation =
  roundTripCborRangeExpectation (eraProtVerLow @era) (eraProtVerHigh @era)

-- | QuickCheck property spec that uses `roundTripAnnEraExpectation`
roundTripAnnEraSpec ::
  forall era t.
  (Era era, Show t, Eq t, ToCBOR t, DecCBOR (Annotator t), Arbitrary t, HasCallStack) =>
  Spec
roundTripAnnEraSpec =
  prop (show (typeRep $ Proxy @t)) $ roundTripAnnEraExpectation @era @t

-- | Similar to `roundTripEraExpectation`, but for Annotator decoders. Note the
-- constraint `ToCBOR` vs `EncCBOR`, this is due to the requirement for memoized types
-- to be already fully encoded.
roundTripAnnEraExpectation ::
  forall era t.
  (Era era, Show t, Eq t, ToCBOR t, DecCBOR (Annotator t), HasCallStack) =>
  t ->
  Expectation
roundTripAnnEraExpectation =
  roundTripAnnRangeExpectation (eraProtVerLow @era) (eraProtVerHigh @era)

-- | QuickCheck property spec that uses `roundTripEraTypeExpectation`
roundTripEraTypeSpec ::
  forall era t.
  ( Era era
  , Show (t era)
  , Eq (t era)
  , EncCBOR (t era)
  , DecCBOR (t era)
  , Arbitrary (t era)
  , HasCallStack
  ) =>
  Spec
roundTripEraTypeSpec =
  prop (show (typeRep $ Proxy @(t era))) $ roundTripEraTypeExpectation @era @t

-- | Roundtrip CBOR testing for types that implement EncCBOR/DecCBOR. Unlike
-- `roundTripEraExpectation`, this function can't be used with type families, but the
-- types of this function are unambiguous.
roundTripEraTypeExpectation ::
  forall era t.
  (Era era, Show (t era), Eq (t era), EncCBOR (t era), DecCBOR (t era), HasCallStack) =>
  t era ->
  Expectation
roundTripEraTypeExpectation = roundTripEraExpectation @era @(t era)

-- | QuickCheck property spec that uses `roundTripAnnEraTypeExpectation`
roundTripAnnEraTypeSpec ::
  forall era t.
  ( Era era
  , Show (t era)
  , Eq (t era)
  , ToCBOR (t era)
  , DecCBOR (Annotator (t era))
  , Arbitrary (t era)
  , HasCallStack
  ) =>
  Spec
roundTripAnnEraTypeSpec =
  prop (show (typeRep $ Proxy @(t era))) $ roundTripAnnEraTypeExpectation @era @t

-- | Same as `roundTripAnnEraExpectation`, but is not suitable for type families.
roundTripAnnEraTypeExpectation ::
  forall era t.
  ( Era era
  , Show (t era)
  , Eq (t era)
  , ToCBOR (t era)
  , DecCBOR (Annotator (t era))
  , HasCallStack
  ) =>
  t era ->
  Expectation
roundTripAnnEraTypeExpectation = roundTripAnnEraExpectation @era @(t era)

-- | QuickCheck property spec that uses `roundTripShareEraExpectation`
roundTripShareEraSpec ::
  forall era t.
  (Era era, Show t, Eq t, EncCBOR t, DecShareCBOR t, Arbitrary t, HasCallStack) =>
  Spec
roundTripShareEraSpec =
  prop (show (typeRep $ Proxy @t)) $ roundTripShareEraExpectation @era @t

-- | Roundtrip CBOR testing for types and type families that implement
-- EncCBOR/DecShareCBOR. Requires TypeApplication of an @@era@
roundTripShareEraExpectation ::
  forall era t.
  (Era era, Show t, Eq t, EncCBOR t, DecShareCBOR t, HasCallStack) =>
  t ->
  Expectation
roundTripShareEraExpectation =
  roundTripRangeExpectation
    (mkTrip encCBOR decNoShareCBOR)
    (eraProtVerLow @era)
    (eraProtVerHigh @era)

-- | QuickCheck property spec that uses `roundTripShareEraTypeExpectation`
roundTripShareEraTypeSpec ::
  forall era t.
  ( Era era
  , Show (t era)
  , Eq (t era)
  , EncCBOR (t era)
  , DecShareCBOR (t era)
  , Arbitrary (t era)
  , HasCallStack
  ) =>
  Spec
roundTripShareEraTypeSpec =
  prop (show (typeRep $ Proxy @(t era))) $ roundTripShareEraTypeExpectation @era @t

-- | Roundtrip CBOR testing for types that implement EncCBOR/DecShareCBOR. Unlike
-- `roundTripShareEraExpectation`, this function can't be used with type families, but the
-- types of this function are unambiguous.
roundTripShareEraTypeExpectation ::
  forall era t.
  (Era era, Show (t era), Eq (t era), EncCBOR (t era), DecShareCBOR (t era), HasCallStack) =>
  t era ->
  Expectation
roundTripShareEraTypeExpectation = roundTripShareEraExpectation @era @(t era)

-- | CBOR RoundTrip spec for all the core types and type families that are parametrized on era.
roundTripCoreEraTypesSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxOut era)
  , Arbitrary (TxCert era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (Value era)
  , Arbitrary (CompactForm (Value era))
  , Arbitrary (Script era)
  , Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , HasCallStack
  ) =>
  Spec
roundTripCoreEraTypesSpec = do
  describe "Core Type Families" $ do
    roundTripEraSpec @era @(Value era)
    roundTripEraSpec @era @(CompactForm (Value era))
    roundTripEraSpec @era @(TxOut era)
    roundTripEraSpec @era @(TxCert era)
    roundTripEraSpec @era @(PParams era)
    roundTripEraSpec @era @(PParamsUpdate era)
    roundTripAnnEraSpec @era @(BootstrapWitness (EraCrypto era))
    roundTripAnnEraSpec @era @(Script era)
    roundTripAnnEraSpec @era @(TxAuxData era)
    roundTripAnnEraSpec @era @(TxWits era)
    roundTripAnnEraSpec @era @(TxBody era)
    roundTripAnnEraSpec @era @(Tx era)
  describe "Core State Types" $ do
    roundTripShareEraSpec @era @(SnapShots (EraCrypto era))
    roundTripShareEraTypeSpec @era @DState
    roundTripShareEraTypeSpec @era @PState
    roundTripShareEraTypeSpec @era @CommitteeState
    roundTripShareEraTypeSpec @era @VState
    roundTripShareEraTypeSpec @era @CertState
    roundTripShareEraTypeSpec @era @UTxO

data EraRuleProof era (rs :: [Symbol]) where
  EraRuleProofEmpty :: EraRuleProof era '[]
  EraRuleProofHead ::
    ( Show (EraRuleFailure r era)
    , Eq (EraRuleFailure r era)
    , EncCBOR (EraRuleFailure r era)
    , DecCBOR (EraRuleFailure r era)
    , Arbitrary (EraRuleFailure r era)
    , EraRuleFailure r era ~ PredicateFailure (EraRule r era)
    ) =>
    Proxy r ->
    EraRuleProof era xs ->
    EraRuleProof era (r ': xs)

class UnliftRules era (rs :: [Symbol]) where
  unliftEraRuleProofs :: EraRuleProof era rs

instance UnliftRules era '[] where
  unliftEraRuleProofs = EraRuleProofEmpty

instance
  ( Show (EraRuleFailure r era)
  , Eq (EraRuleFailure r era)
  , EncCBOR (EraRuleFailure r era)
  , DecCBOR (EraRuleFailure r era)
  , Arbitrary (EraRuleFailure r era)
  , EraRuleFailure r era ~ PredicateFailure (EraRule r era)
  , UnliftRules era rs
  ) =>
  UnliftRules era (r ': rs)
  where
  unliftEraRuleProofs = EraRuleProofHead Proxy unliftEraRuleProofs

class
  UnliftRules era (EraRules era) =>
  RuleListEra era
  where
  type EraRules era :: [Symbol]

roundTripAllPredicateFailures ::
  forall era.
  ( RuleListEra era
  , Era era
  , HasCallStack
  ) =>
  Spec
roundTripAllPredicateFailures =
  describe "Predicate Failures" . go $ unliftEraRuleProofs @era @(EraRules era)
  where
    go :: EraRuleProof era rs' -> Spec
    go EraRuleProofEmpty = pure ()
    go (EraRuleProofHead (Proxy :: Proxy r) x) = do
      roundTripEraSpec @era @(EraRuleFailure r era)
      go x
