{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Address (CompactAddr)
import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Binary (decoderEquivalenceExpectation)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Binary.RoundTrip (roundTripConwayCommonSpec)
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.Binary as Binary (decoderEquivalenceCoreEraTypesSpec, txSizeSpec)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra, roundTripEraSpec)

spec ::
  forall era.
  ( ConwayEraImp era
  , RuleListEra era
  ) =>
  Spec
spec = do
  describe "RoundTrip" $ do
    roundTripCborSpec @GovActionId
    roundTripCborSpec @(GovPurposeId 'PParamUpdatePurpose)
    roundTripCborSpec @(GovPurposeId 'HardForkPurpose)
    roundTripCborSpec @(GovPurposeId 'CommitteePurpose)
    roundTripCborSpec @(GovPurposeId 'ConstitutionPurpose)
    roundTripCborSpec @Vote
    roundTripCborSpec @Voter
    roundTripConwayCommonSpec @era
    -- ConwayGenesis only makes sense in Conway era
    roundTripEraSpec @era @ConwayGenesis
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @era
    decoderEquivalenceLenientSpec @(TxDats era)
    decoderEquivalenceLenientSpec @(Redeemers era)
  Binary.txSizeSpec @era

  describe "MultiAsset constraints" $ do
    describe "TxOut" $ do
      prop "Completely empty MultiAsset fails deserialisation since Dijkstra" $
        -- Cannot use standard serialization because MaryValue encCBOR optimizes away
        -- completely empty MultiAssets via 'if Map.null m then encCBOR c', preventing decoder testing
        forAll arbitrary $ \(addr :: CompactAddr) ->
          testMultiAssetRejection @era @(TxOut era) (natVersion @12) "Dijkstra" $ \version ->
            buildTxOutCborWithMultiAsset addr (MultiAsset Map.empty) version

      prop "Empty nested asset maps fails deserialisation since Conway" $
        -- Cannot use standard serialization because MaryValue encCBOR optimizes away
        -- empty nested maps via pruneZeroMultiAsset, preventing decoder validation testing
        forAll arbitrary $ \(addr :: CompactAddr) ->
          forAll arbitrary $ \policyId ->
            testMultiAssetRejection @era @(TxOut era) (natVersion @9) "Conway" $ \version ->
              buildTxOutCborWithMultiAsset addr (MultiAsset $ Map.singleton policyId Map.empty) version

    describe "Mint field in Tx" $ do
      prop "Completely empty mint MultiAsset fails deserialisation since Conway" $
        -- Cannot use standard serialization because Conway TxBody encoder omits empty
        -- mint fields via 'Omit (== mempty) (Key 9 (To ctbrMint))', preventing decoder testing
        testMultiAssetRejection @era @(TxBody TopTx era) (natVersion @9) "Conway" $ \version ->
          buildTxBodyCborWithMint @era (MultiAsset Map.empty) version

      prop "Empty nested asset maps in mint MultiAsset fails deserialisation since Conway" $
        forAll arbitrary $ \policyId ->
          testMultiAssetRejection @era @(TxBody TopTx era) (natVersion @9) "Conway" $ \version ->
            buildTxBodyCborWithMint @era (MultiAsset $ Map.singleton policyId Map.empty) version
  where
    -- The expectation used in this spec allows for the deserialization to fail, in which case
    -- it only checks that it fails for both decoders.
    -- This is necessary because for some arbitrarily generated values, the deserialization fails
    -- starting with Conway (for example: empty TxDats or Redeemers)
    decoderEquivalenceLenientSpec ::
      forall t. (Arbitrary t, Eq t, EncCBOR t, DecCBOR t, DecCBOR (Annotator t), Show t) => Spec
    decoderEquivalenceLenientSpec =
      prop (show (typeRep $ Proxy @t)) $ property $ \(x :: t) ->
        forM_ [eraProtVerLow @ConwayEra .. eraProtVerHigh @ConwayEra] $ \v ->
          decoderEquivalenceExpectation @t v (serialize v x)

-- | Test empty MultiAsset rejection across protocol versions
testMultiAssetRejection ::
  forall era a.
  ( ConwayEraImp era
  , DecCBOR a
  , Show a
  ) =>
  Version -> -- Protocol version when rejection starts
  String -> -- Era name for error messages
  (Version -> BSL.ByteString) -> -- CBOR constructor function
  Expectation
testMultiAssetRejection rejectionVersion era mkCborBytes =
  forM_ [eraProtVerLow @era .. eraProtVerHigh @era] $ \version -> do
    let cborBytes = mkCborBytes version
    case decodeFull @a version cborBytes of
      Left err ->
        unless (version >= rejectionVersion) $
          expectationFailure $
            "Empty MultiAsset should succeed pre-" ++ era ++ " (version " ++ show version ++ ") | " ++ show err
      Right val ->
        when (version >= rejectionVersion) $
          expectationFailure $
            "Empty MultiAsset should fail since " ++ era ++ " (version " ++ show version ++ ") | " ++ show val

-- | Build TxOut CBOR with explicit MultiAsset inclusion
-- Bypasses MaryValue encoder that may optimize away empty MultiAssets
buildTxOutCborWithMultiAsset ::
  CompactAddr ->
  MultiAsset ->
  Version ->
  BSL.ByteString
buildTxOutCborWithMultiAsset addr multiAsset version =
  let coin = Coin 100
      maryValueMap = (coin, multiAsset)
      txOutMap =
        Map.fromList
          [ (0, encCBOR addr)
          , (1, encCBOR (encCBOR maryValueMap))
          ] ::
          Map.Map Int Encoding
   in serialize version (encCBOR txOutMap)

-- | Build TxBody CBOR with explicit mint field inclusion
-- Bypasses Conway TxBody encoder that omits empty mint fields
buildTxBodyCborWithMint ::
  forall era.
  ConwayEraImp era =>
  MultiAsset ->
  Version ->
  BSL.ByteString
buildTxBodyCborWithMint mintMultiAsset version =
  let emptyInputs = Set.empty :: Set.Set TxIn
      emptyOutputs = StrictSeq.empty :: StrictSeq (TxOut era)
      zeroFee = Coin 0
      txBodyMap =
        Map.fromList
          [ (0, encCBOR emptyInputs)
          , (1, encCBOR emptyOutputs)
          , (2, encCBOR zeroFee)
          , (9, encCBOR mintMultiAsset)
          ] ::
          Map.Map Int Encoding
   in serialize version (encCBOR txBodyMap)
