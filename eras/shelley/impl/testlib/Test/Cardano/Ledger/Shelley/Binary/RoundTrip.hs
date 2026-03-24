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
  metadatumSizeLimitSpec,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Metadata (Metadatum)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.State
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (E))
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
  let genBytes n = BS.pack <$> vectorOf n arbitrary
      genAsciiText n = T.pack <$> vectorOf n (choose ('a', 'z'))
      dec :: ToCBOR a => a -> Either DecoderError Metadatum
      dec = decodeFull v . toLazyByteString . toCBOR . E
  prop "Accepts bytes up to 64 bytes" $
    forAll (choose (0, 64) >>= genBytes) $ \bs ->
      dec bs `shouldSatisfy` isRight
  prop "Accepts text up to 64 bytes" $
    forAll (choose (0, 64) >>= genAsciiText) $ \txt ->
      dec txt `shouldSatisfy` isRight
  if v > natVersion @2
    then do
      prop "Rejects bytes exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genBytes) $ \bs ->
          dec bs `shouldSatisfy` isLeft
      prop "Rejects text exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genAsciiText) $ \txt ->
          dec txt `shouldSatisfy` isLeft
    else do
      prop "Accepts bytes exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genBytes) $ \bs ->
          dec bs `shouldSatisfy` isRight
      prop "Accepts text exceeding 64 bytes" $
        forAll (choose (65, 1000) >>= genAsciiText) $ \txt ->
          dec txt `shouldSatisfy` isRight

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
