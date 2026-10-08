{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Binary.RoundTrip (
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
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import Data.Typeable
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
  (Era era, Typeable t, Show t, Eq t, EncCBOR t, DecShareCBOR t, Arbitrary t, HasCallStack) =>
  Spec
roundTripShareEraSpec =
  prop (show (typeRep $ Proxy @t)) $ roundTripShareEraExpectation @era @t

-- | Roundtrip CBOR testing for types and type families that implement
-- EncCBOR/DecShareCBOR. Requires TypeApplication of an @@era@
roundTripShareEraExpectation ::
  forall era t.
  (Era era, Typeable t, Show t, Eq t, EncCBOR t, DecShareCBOR t, HasCallStack) =>
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
  , Typeable t
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
  (Era era, Typeable t, Show (t era), Eq (t era), EncCBOR (t era), DecShareCBOR (t era), HasCallStack) =>
  t era ->
  Expectation
roundTripShareEraTypeExpectation = roundTripShareEraExpectation @era @(t era)

-- | CBOR RoundTrip spec for all the core types and type families that are parametrized on era.
roundTripCoreEraTypesSpec ::
  forall era.
  ( EraTx era
  , EraCertState era
  , Arbitrary (Tx TopTx era)
  , Arbitrary (TxBody TopTx era)
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
  , DecCBOR (TxBody TopTx era)
  , DecCBOR (Tx TopTx era)
  , Typeable (CertState era)
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
    roundTripAnnEraSpec @era @(Script era)
    roundTripEraSpec @era @(Script era)
    roundTripAnnEraSpec @era @(TxAuxData era)
    roundTripEraSpec @era @(TxAuxData era)
    roundTripAnnEraSpec @era @(TxWits era)
    roundTripEraSpec @era @(TxWits era)
    roundTripAnnEraSpec @era @(TxBody TopTx era)
    roundTripEraSpec @era @(TxBody TopTx era)
    roundTripAnnEraSpec @era @(Tx TopTx era)
    roundTripEraSpec @era @(Tx TopTx era)
    prop ("MemPack/CBOR Roundtrip " <> show (typeRep $ Proxy @(TxOut era))) $
      roundTripRangeExpectation @(TxOut era)
        (mkTrip encodeMemPack decNoShareCBOR)
        (eraProtVerLow @era)
        (eraProtVerHigh @era)
    roundTripShareEraSpec @era @(CertState era)
    roundTripShareEraSpec @era @(SnapShots era)
    roundTripEraSpec @era @IndividualPoolStake
  describe "Core State Types" $ do
    roundTripShareEraTypeSpec @era @DState
    roundTripShareEraTypeSpec @era @PState
    roundTripShareEraTypeSpec @era @CommitteeState
    roundTripShareEraTypeSpec @era @UTxO
  describe "PState encoding" $ do
    let version = eraProtVerLow @era
        -- The 4 elements that a 'PState' consisted of before 'psBlsKeyHashes' was added
        legacyFields :: PState era -> [Encoding]
        legacyFields ps =
          [ encCBOR (psVRFKeyHashes ps)
          , encCBOR (psStakePools ps)
          , encCBOR (psFutureStakePoolParams ps)
          , encCBOR (psRetiring ps)
          ]
        indefiniteLengthEncoding :: PState era -> Encoding
        indefiniteLengthEncoding ps =
          mconcat $
            [encodeListLenIndef]
              <> legacyFields ps
              <> [encCBOR (psBlsKeyHashes ps) | not (null (psBlsKeyHashes ps))]
              <> [encodeBreak]
    -- Eras before Dijkstra never have BLS key hashes, so their encoding must not change
    prop "without BLS key hashes is the legacy 4-element list" $
      forAll (arbitrary @(PState era)) $ \ps ->
        serialize' version (ps {psBlsKeyHashes = mempty})
          `shouldBe` serialize' version (mconcat (encodeListLen 4 : legacyFields ps))
    prop "with BLS key hashes is a 5-element list" $
      forAll (arbitrary @(PState era)) $ \ps blsKeyHash blsKeyHashCount ->
        let blsKeyHashes = Map.singleton blsKeyHash blsKeyHashCount
         in serialize' version (ps {psBlsKeyHashes = blsKeyHashes})
              `shouldBe` serialize' version (mconcat (encodeListLen 5 : legacyFields ps <> [encCBOR blsKeyHashes]))
    prop "decodes an indefinite-length list, with or without BLS key hashes" $
      forAll (arbitrary @(PState era)) $ \ps ->
        forM_ [ps, ps {psBlsKeyHashes = mempty}] $
          roundTripRangeExpectation
            (mkTrip indefiniteLengthEncoding decNoShareCBOR)
            (eraProtVerLow @era)
            (eraProtVerHigh @era)
