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
  roundTripEraTypeSpec,
  roundTripShareEraSpec,
  roundTripShareEraTypeSpec,

  -- * Expectation
  roundTripEraExpectation,
  roundTripEraTypeExpectation,
  roundTripShareEraExpectation,
  roundTripShareEraTypeExpectation,
  roundTripCoreEraTypesSpec,
  roundTripAllPredicateFailures,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.State
import Control.State.Transition.Extended (STS (..))
import Data.Typeable
import GHC.TypeLits (Symbol)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

-- | QuickCheck property spec that uses `roundTripEraExpectation`
roundTripEraSpec ::
  forall era t.
  (Era era, EncCBOR t, Arbitrary t, HasCallStack) =>
  Spec
roundTripEraSpec =
  prop (show (typeRep $ Proxy @t)) . forAllBlind arbitrary $ roundTripEraExpectation @era @t

-- | Roundtrip CBOR testing for types and type families that implement
-- EncCBOR/DecCBOR. Requires TypeApplication of an @@era@
roundTripEraExpectation ::
  forall era t.
  (Era era, EncCBOR t, HasCallStack) =>
  t ->
  Expectation
roundTripEraExpectation =
  roundTripCborRangeExpectation (eraProtVerLow @era) (eraProtVerHigh @era)

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
  , EraCertState era
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
  , Arbitrary (CertState era)
  , Arbitrary (Accounts era)
  , DecCBOR (Script era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxBody era)
  , DecCBOR (Tx era)
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
    roundTripEraSpec @era @(Script era)
    roundTripEraSpec @era @(Annotator (Script era))
    roundTripEraSpec @era @(TxAuxData era)
    roundTripEraSpec @era @(Annotator (TxAuxData era))
    roundTripEraSpec @era @(TxWits era)
    roundTripEraSpec @era @(Annotator (TxWits era))
    roundTripEraSpec @era @(TxBody era)
    roundTripEraSpec @era @(Annotator (TxBody era))
    roundTripEraSpec @era @(Tx era)
    roundTripEraSpec @era @(Annotator (Tx era))
    prop ("MemPack/CBOR Roundtrip " <> show (typeRep $ Proxy @(TxOut era))) $
      roundTripRangeExpectation @(TxOut era)
        (mkTrip encodeMemPack decNoShareCBOR)
        (eraProtVerLow @era)
        (eraProtVerHigh @era)
    roundTripShareEraSpec @era @(CertState era)
  describe "Core State Types" $ do
    roundTripShareEraSpec @era @SnapShots
    roundTripShareEraTypeSpec @era @DState
    roundTripShareEraTypeSpec @era @PState
    roundTripShareEraTypeSpec @era @CommitteeState
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
