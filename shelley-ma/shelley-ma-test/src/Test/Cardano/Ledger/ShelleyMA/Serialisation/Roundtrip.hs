{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Binary (Annotator (..), FromCBOR, ToCBOR)
import Cardano.Ledger.Core (SerialisableData)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import qualified Cardano.Ledger.ShelleyMA.Metadata as MA
import Control.State.Transition
import qualified Data.ByteString.Lazy as Lazy (null)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Sequence.Strict (StrictSeq, fromList)
import Shelley.Spec.Ledger.API (ApplyTxError, UTXOW)
import qualified Shelley.Spec.Ledger.Metadata as Shelley (Metadata (..))
import Test.Cardano.Ledger.EraBuffet
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders
  ( roundTrip,
    roundTripAnn,
  )
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (counterexample, forAll, (===))
import Test.Shelley.Spec.Ledger.Generator.Metadata ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, arbitrary, choose, testProperty, vectorOf)

-- ======================================================================
-- Witnesses to each Era

data EraIndex index where
  Mary :: EraIndex (MaryEra StandardCrypto)
  Shelley :: EraIndex (ShelleyEra StandardCrypto)
  Allegra :: EraIndex (AllegraEra StandardCrypto)

instance Show (EraIndex e) where
  show Mary = "Mary Era"
  show Shelley = "Shelley Era"
  show Allegra = "Allegra Era"

-- ============================================================
-- EraIndex parameterized generators for each type family

genTxBody :: EraIndex e -> Gen (TxBody e)
genTxBody Shelley = arbitrary
genTxBody Mary = arbitrary
genTxBody Allegra = arbitrary

genScript :: EraIndex e -> Gen (Script e)
genScript Shelley = arbitrary
genScript Mary = arbitrary
genScript Allegra = arbitrary

genValue :: EraIndex e -> Gen (Value e)
genValue Shelley = arbitrary
genValue Mary = arbitrary
genValue Allegra = arbitrary

genMeta :: EraIndex e -> Gen (Metadata e)
genMeta Mary = do
  m <- arbitrary
  s <- genScriptSeq Mary
  pure (MA.Metadata m s)
genMeta Allegra = do
  m <- arbitrary
  s <- genScriptSeq Allegra
  pure (MA.Metadata m s)
genMeta Shelley = Shelley.Metadata <$> arbitrary

genApplyTxError :: EraIndex e -> Gen (ApplyTxError e)
genApplyTxError Shelley = arbitrary
genApplyTxError Mary = arbitrary
genApplyTxError Allegra = arbitrary

-- ==========================================================
-- Parameterized helper function for generating MA style Metadata

genScriptSeq :: EraIndex e -> Gen (StrictSeq (Script e))
genScriptSeq index = do
  n <- choose (0, 6)
  l <- vectorOf n (genScript index)
  pure (fromList l)

-- ==========================================================
-- EraIndex parameterized property tests

propertyAnn ::
  forall e t.
  (Eq t, Show t, ToCBOR t, FromCBOR (Annotator t)) =>
  String ->
  EraIndex e ->
  (EraIndex e -> Gen t) ->
  TestTree
propertyAnn name i gen = testProperty ("roundtripAnn " ++ name) $ do
  x <- gen i
  case roundTripAnn x of
    Right (left, _)
      | not (Lazy.null left) ->
        error ("unconsumed trailing bytes: " ++ show left)
    Right (_, y) ->
      if x == y
        then pure True
        else error ("Unequal\n   " ++ show x ++ "\n   " ++ show y)
    Left s -> error (show (s, x))

property ::
  forall e t.
  (Eq t, Show t, ToCBOR t, FromCBOR t) =>
  String ->
  EraIndex e ->
  (EraIndex e -> Gen t) ->
  TestTree
property name i gen = testProperty ("roundtrip " ++ name) $
  forAll (gen i) $ \x -> case roundTrip x of
    Right (remaining, y) | BSL.null remaining -> x === y
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    Left stuff ->
      counterexample
        ("Failed to decode: " <> show stuff)
        False

allprops ::
  ( ShelleyBased e,
    SerialisableData (ApplyTxError e),
    Eq (PredicateFailure (UTXOW e)),
    Show (PredicateFailure (UTXOW e))
  ) =>
  EraIndex e ->
  TestTree
allprops index =
  testGroup
    (show index)
    [ propertyAnn "TxBody" index genTxBody,
      propertyAnn "Metadata" index genMeta,
      property "Value" index genValue,
      propertyAnn "Script" index genScript,
      property "ApplyTxError" index genApplyTxError
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [allprops Shelley, allprops Allegra, allprops Mary]
