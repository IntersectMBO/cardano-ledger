{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Binary.RoundTrip (
  roundTripShelleyCommonSpec,
  roundTripShelleyCommonSpecWith,
  roundTripStateEraTypesSpec,
  roundTripStateEraTypesSpecWith,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Metadata (Metadatum (I))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.State
import qualified Data.Text as T
import Data.Typeable (Proxy (..), typeRep)
import Test.Cardano.Base.Bytes (genByteString)
import Test.Cardano.Ledger.Binary.RoundTrip (
  embedTrip,
  embedTripRangeFailureExpectation,
  mkTrip,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Arbitrary (
  genEpochStateWith,
  genNewEpochStateWith,
  genUTxOStateWith,
 )
import Test.Cardano.Ledger.Shelley.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.Era (ShelleyEraTest)

roundTripShelleyCommonSpec :: forall era. ShelleyEraTest era => Spec
roundTripShelleyCommonSpec = roundTripShelleyCommonSpecWith @era arbitrary arbitrary

-- | Like `roundTripShelleyCommonSpec`, but with the given generators for the `GovState`. See
-- `roundTripStateEraTypesSpecWith`.
roundTripShelleyCommonSpecWith ::
  forall era. ShelleyEraTest era => Gen (GovState era) -> Gen (GovState era) -> Spec
roundTripShelleyCommonSpecWith genGovState genNestedGovState = do
  roundTripCoreEraTypesSpec @era
  roundTripStateEraTypesSpecWith @era genGovState genNestedGovState
  describe "Metadatum size limits" $
    forEachEraVersion @era metadatumSizeLimitSpec
  describe "Metadatum int range" $
    metadatumIntRangeSpec (eraProtVerLow @era) (eraProtVerHigh @era)

roundTripStateEraTypesSpec ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , EncCBOR (StashedAVVMAddresses era)
  , DecCBOR (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (PParams era)
  , Arbitrary (GovState era)
  , Arbitrary (CertState era)
  , Arbitrary (InstantStake era)
  ) =>
  Spec
roundTripStateEraTypesSpec = roundTripStateEraTypesSpecWith @era arbitrary arbitrary

-- | Like `roundTripStateEraTypesSpec`, but with the given generators for the `GovState`:
--
-- * the first for the roundtrip of the `GovState` itself,
-- * the second for the `GovState` nested in other types such as `UTxOState`, `EpochState` and `NewEpochState`.
--
-- The `GovState` is decoded the same way when it is nested, and the first generator already
-- covers its decoding, so the second one only needs to give it a realistic structure. That lets
-- eras whose `GovState` is expensive to roundtrip, like Conway with the cost models in its
-- `PParams`, to pass a cheaper generator, instead of decoding the same expensive
-- parts again in every state type. The other fields of these state types still come from their
-- `Arbitrary` instances.
roundTripStateEraTypesSpecWith ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , EncCBOR (StashedAVVMAddresses era)
  , DecCBOR (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (PParams era)
  , Arbitrary (GovState era)
  , Arbitrary (CertState era)
  , Arbitrary (InstantStake era)
  ) =>
  Gen (GovState era) ->
  Gen (GovState era) ->
  Spec
roundTripStateEraTypesSpecWith genGovState genNestedGovState = do
  describe "State Types Families" $ do
    prop (show $ typeRep $ Proxy @(GovState era)) $
      forAllShrink genGovState shrink $
        roundTripShareEraExpectation @era
  describe "State Types" $ do
    prop (show $ typeRep $ Proxy @(UTxOState era)) $
      forAllShrink (genUTxOStateWith genNestedGovState) shrink $
        roundTripShareEraTypeExpectation @era @UTxOState
    prop (show $ typeRep $ Proxy @(EpochState era)) $
      forAllShrink (genEpochStateWith genNestedGovState) shrink $
        roundTripEraTypeExpectation @era @EpochState
    prop (show $ typeRep $ Proxy @(NewEpochState era)) $
      forAllShrink (genNewEpochStateWith genNestedGovState) shrink $
        roundTripEraTypeExpectation @era @NewEpochState

metadatumSizeLimitSpec :: Version -> Spec
metadatumSizeLimitSpec v = do
  let
    genAsciiText n = T.pack <$> vectorOf n (choose ('a', 'z'))
    dec :: ToCBOR a => a -> Either DecoderError Metadatum
    dec = decodeFull v . toLazyByteString . toCBOR
  prop "Accepts bytes up to 64 bytes" $
    forAll (choose (0, 64) >>= genByteString) $ \bs ->
      expectRightDeepExpr_ $ dec bs
  prop "Accepts text up to 64 bytes" $
    forAll (choose (0, 64) >>= genAsciiText) $ \txt ->
      expectRightDeepExpr_ $ dec txt
  if v > natVersion @2
    then do
      prop "Rejects bytes exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genByteString) $ \bs ->
          void . expectLeftExpr $ dec bs
      prop "Rejects text exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genAsciiText) $ \txt ->
          void . expectLeftExpr $ dec txt
    else do
      prop "Accepts bytes exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genByteString) $ \bs ->
          expectRightDeepExpr_ $ dec bs
      prop "Accepts text exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genAsciiText) $ \txt ->
          expectRightDeepExpr_ $ dec txt

metadatumIntRangeSpec :: Version -> Version -> Spec
metadatumIntRangeSpec fromVersion toVersion = do
  let
    -- CBOR int range: -2^64 .. 2^64-1
    maxInt :: Integer
    maxInt = 2 ^ (64 :: Int) - 1
    minInt :: Integer
    minInt = -(2 ^ (64 :: Int))
    intToMetadatum = mkTrip (encCBOR @Integer) (decCBOR @Metadatum)
    expectAccepted n =
      forM_ [fromVersion .. toVersion] $ \v ->
        case embedTrip v v intToMetadatum n of
          Left err -> expectationFailure $ "Failed to deserialize: " ++ show err
          Right m -> m `shouldBe` I n
    expectRejected = embedTripRangeFailureExpectation intToMetadatum fromVersion toVersion
    -- Test the binary decoder directly, bypassing FlatTerm.
    -- This is needed because cborg's FlatTerm has an off-by-one bug in
    -- tokenTypeOf that misclassifies -(2^64) as TypeInteger instead of
    -- TypeNInt64 (https://github.com/well-typed/cborg/issues/377)
    --
    -- This bug only affect the tests, not the actual decoder, because the
    -- actual decoder does not go through FlatTerm
    expectBinaryAccepted n =
      forM_ [fromVersion .. toVersion] $ \v ->
        decodeFull @Metadatum v (serialize v n) `shouldBe` Right (I n)
    expectBinaryRejected n =
      forM_ [fromVersion .. toVersion] $ \v ->
        case decodeFull @Metadatum v (serialize v n) of
          Left _ -> pure ()
          Right _ -> expectationFailure $ "Should not have deserialized: " ++ show n
  it "Accepts max int (2^64-1)" $
    expectAccepted maxInt
  it "Accepts max int (2^64-1) (binary)" $
    expectBinaryAccepted maxInt
  -- TODO: enable this once the cborg FlatTerm bug is fixed
  xit "Accepts min int (-(2^64))" $
    expectAccepted minInt
  it "Accepts min int (-(2^64)) (binary)" $
    expectBinaryAccepted minInt
  it "Rejects positive big integer (2^64)" $
    expectRejected (maxInt + 1)
  it "Rejects positive big integer (2^64) (binary)" $
    expectBinaryRejected (maxInt + 1)
  it "Rejects negative big integer (-(2^64+1))" $
    expectRejected (minInt - 1)
  it "Rejects negative big integer (-(2^64+1)) (binary)" $
    expectBinaryRejected (minInt - 1)
  prop "Accepts any int in CBOR range" $
    forAll (choose (minInt, maxInt)) $ \n ->
      property $ expectBinaryAccepted n
