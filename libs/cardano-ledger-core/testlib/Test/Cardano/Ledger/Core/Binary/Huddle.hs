{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Binary.Huddle (
  huddleCodecSpecFull,
  huddleCodecSpecNoAnn,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR, EncCBOR, ToCBOR, Version)
import Cardano.Ledger.Core (Era)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Binary.Cuddle (
  HuddleEnv,
  huddleAntiCborSpec,
  huddleDecoderEquivalenceSpec,
  huddleRoundTripAnnCborSpec,
  huddleRoundTripCborSpec,
  huddleRoundTripGenValidate,
 )
import Test.Cardano.Ledger.Core.Binary.RoundTrip (
  roundTripAnnEraExpectation,
  roundTripEraExpectation,
 )
import Test.Hspec (SpecWith, it)
import Test.QuickCheck (Gen, forAll, property)

-- | Full codec spec for types with both plain and Annotator decoders.
-- Runs all 7 test dimensions:
--
-- 1. CDDL -> decode (Annotator) -> encode -> bytes match
-- 2. CDDL -> decode (plain) -> encode -> bytes match
-- 3. Haskell -> encode -> CDDL validate
-- 4. CDDL -> corrupt -> decode rejects
-- 5. Annotator vs plain decoder produce same result
-- 6. Haskell -> encode -> decode (plain) -> Haskell match
-- 7. Haskell -> encode -> decode (Annotator) -> Haskell match
huddleCodecSpecFull ::
  forall era a.
  ( Era era
  , Eq a
  , Show a
  , EncCBOR a
  , DecCBOR a
  , DecCBOR (Annotator a)
  , ToCBOR a
  , HasCallStack
  ) =>
  Gen a ->
  Version ->
  T.Text ->
  SpecWith HuddleEnv
huddleCodecSpecFull gen version ruleName = do
  huddleCodecSpecNoAnn @era @a gen version ruleName
  huddleRoundTripAnnCborSpec @a version ruleName
  huddleDecoderEquivalenceSpec @a version ruleName
  it (T.unpack ruleName <> ": Haskell roundtrip (Annotator)") $ \_ ->
    property $ forAll gen $ roundTripAnnEraExpectation @era @a

-- | Codec spec for types with only plain decoders (no Annotator).
-- Runs 4 test dimensions:
--
-- 1. CDDL -> decode (plain) -> encode -> bytes match
-- 2. Haskell -> encode -> CDDL validate
-- 3. CDDL -> corrupt -> decode rejects
-- 4. Haskell -> encode -> decode (plain) -> Haskell match
huddleCodecSpecNoAnn ::
  forall era a.
  ( Era era
  , Eq a
  , Show a
  , EncCBOR a
  , DecCBOR a
  , HasCallStack
  ) =>
  Gen a ->
  Version ->
  T.Text ->
  SpecWith HuddleEnv
huddleCodecSpecNoAnn gen version ruleName = do
  huddleRoundTripCborSpec @a version ruleName
  huddleRoundTripGenValidate @a gen version ruleName
  huddleAntiCborSpec @a version ruleName
  it (T.unpack ruleName <> ": Haskell roundtrip (plain)") $ \_ ->
    property $ forAll gen $ roundTripEraExpectation @era @a
