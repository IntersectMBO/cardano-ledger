{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Core.Binary (
  decoderEquivalenceSpec,
  decoderEquivalenceEraSpec,
  txSizeSpec,
  decoderEquivalenceCoreEraTypesSpec,
  Mem,
  fullAnnCddlSpec,
  fullAnnGenCddlSpec,
  fullCddlSpec,
  fullGenCddlSpec,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR, EncCBOR, Version)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (Mem)
import qualified Data.Text as T
import Lens.Micro
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.Cuddle (
  HuddleEnv,
  huddleAntiCborSpec,
  huddleDecoderEquivalenceSpec,
  huddleRoundTripAnnCborSpec,
  huddleRoundTripCborSpec,
  huddleRoundTripGenValidate,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.Annotator

txSizeSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx TopTx era)
  , SafeToHash (TxWits era)
  ) =>
  Spec
txSizeSpec =
  describe "Transaction size" $ do
    prop "should match the size of the cbor encoding" $ \(tx :: Tx TopTx era) -> do
      let txSize = sizeTxForFeeCalculation tx
      txSize `shouldBe` tx ^. sizeTxF

-- | Like 'fullAnnCddlSpec' but with a custom generator.
fullAnnGenCddlSpec ::
  forall a.
  ( Eq a
  , Show a
  , EncCBOR a
  , DecCBOR a
  , DecCBOR (Annotator a)
  , HasCallStack
  ) =>
  Gen a ->
  Version ->
  T.Text ->
  SpecWith HuddleEnv
fullAnnGenCddlSpec gen version ruleName = do
  fullGenCddlSpec @a gen version ruleName
  huddleRoundTripAnnCborSpec @a version ruleName
  huddleDecoderEquivalenceSpec @a version ruleName

-- | Full CDDL codec spec for types with both plain and Annotator decoders.
fullAnnCddlSpec ::
  forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , EncCBOR a
  , DecCBOR a
  , DecCBOR (Annotator a)
  , HasCallStack
  ) =>
  Version ->
  T.Text ->
  SpecWith HuddleEnv
fullAnnCddlSpec = fullAnnGenCddlSpec @a (arbitrary @a)

-- | Like 'fullCddlSpec' but with a custom generator.
fullGenCddlSpec ::
  forall a.
  ( Eq a
  , Show a
  , EncCBOR a
  , DecCBOR a
  , HasCallStack
  ) =>
  Gen a ->
  Version ->
  T.Text ->
  SpecWith HuddleEnv
fullGenCddlSpec gen version ruleName = do
  huddleRoundTripCborSpec @a version ruleName
  huddleRoundTripGenValidate @a gen version ruleName
  huddleAntiCborSpec @a version ruleName

-- | CDDL codec spec for types with only plain decoders (no Annotator).
fullCddlSpec ::
  forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , EncCBOR a
  , DecCBOR a
  , HasCallStack
  ) =>
  Version ->
  T.Text ->
  SpecWith HuddleEnv
fullCddlSpec = fullGenCddlSpec @a (arbitrary @a)
