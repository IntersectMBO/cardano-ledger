{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Binary.RoundTrip (
  roundTripShelleyCommonSpec,
  roundTripStateEraTypesSpec,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Metadata (Metadatum)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.State
import qualified Data.Text as T
import Test.Cardano.Base.Bytes (genByteString)
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborRangeExpectation,
  roundTripCborRangeFailureExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.Era (ShelleyEraTest)

roundTripShelleyCommonSpec ::
  forall era.
  ( ShelleyEraTest era
  , RuleListEra era
  ) =>
  Spec
roundTripShelleyCommonSpec = do
  roundTripCoreEraTypesSpec @era
  roundTripStateEraTypesSpec @era
  roundTripAllPredicateFailures @era
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
roundTripStateEraTypesSpec = do
  describe "State Types Families" $ do
    roundTripShareEraSpec @era @(GovState era)
  describe "State Types" $ do
    roundTripShareEraTypeSpec @era @UTxOState
    roundTripEraTypeSpec @era @EpochState
    roundTripEraTypeSpec @era @NewEpochState

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
  it "Accepts max int (2^64-1)" $
    roundTripCborRangeExpectation fromVersion toVersion maxInt
  it "Accepts min int (-(2^64))" $
    roundTripCborRangeExpectation fromVersion toVersion minInt
  it "Rejects positive big integer (2^64)" $
    roundTripCborRangeFailureExpectation fromVersion toVersion (maxInt + 1)
  it "Rejects negative big integer (-(2^64+1))" $
    roundTripCborRangeFailureExpectation fromVersion toVersion (minInt - 1)
  prop "Accepts any int in CBOR range" $
    forAll (choose (minInt, maxInt)) $ \n ->
      property $ roundTripCborRangeExpectation fromVersion toVersion n

instance RuleListEra ShelleyEra where
  type
    EraRules ShelleyEra =
      '[ "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOW"
       ]
