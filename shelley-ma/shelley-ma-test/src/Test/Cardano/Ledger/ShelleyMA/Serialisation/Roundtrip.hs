{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip
  where

import Data.String(fromString)
import Data.Sequence.Strict (StrictSeq,fromList)
import qualified Data.ByteString.Lazy as Lazy(null)
import Cardano.Binary( Annotator (..), FromCBOR, ToCBOR )
import qualified Cardano.Ledger.ShelleyMA.Metadata as MA
import qualified Cardano.Ledger.Mary.Value as Mary(Value(..),AssetName(..),PolicyID(..),valueFromList)
import Shelley.Spec.Ledger.Scripts(ScriptHash(..))
import Shelley.Spec.Ledger.MetaData(MetaData(..))
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators() -- import Arbitrary instances
import Test.Shelley.Spec.Ledger.Generator.MetaData() -- import Arbitrary instances
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators (genHash)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders(roundTrip, roundTripAnn)
import Test.Cardano.Ledger.EraBuffet
import Test.Tasty.QuickCheck (Gen,arbitrary, choose, vectorOf, testProperty)
import Test.Tasty(TestTree, testGroup)

-- ======================================================================
-- Witnesses to each Era

data EraIndex index where
  Mary :: EraIndex (MaryEra StandardCrypto)
  Shelley :: EraIndex (ShelleyEra StandardCrypto)
  Allegra :: EraIndex (AllegraEra StandardCrypto)
  -- Add new Era's here, like this:
  -- Alonzo :: EraIndex (AlonzoEra StandardCrypto)

instance Show (EraIndex e) where
  show Mary = "Mary Era"
  show Shelley = "Shelley Era"
  show Allegra = "Allegra Era"
  -- Show Alonzo = "Alonzo Era"

-- ============================================================
-- EraIndex parameterized generators for each type family

genTxBody :: EraIndex e -> Gen(TxBody e)
genTxBody Shelley = arbitrary
genTxBody Mary = arbitrary
genTxBody Allegra = arbitrary

genScript :: EraIndex e -> Gen(Script e)
genScript Shelley = arbitrary
genScript Mary = arbitrary
genScript Allegra = arbitrary

genValue :: EraIndex e -> Gen(Value e)
genValue Shelley = arbitrary
genValue Mary = genMaryValue Mary
genValue Allegra = arbitrary

genMeta :: EraIndex e -> Gen(Metadata e)
genMeta Mary = do
  m <- arbitrary
  s <- genScriptSeq Mary
  pure (MA.Metadata m s)
genMeta Allegra = do
  m <- arbitrary
  s <- genScriptSeq Allegra
  pure (MA.Metadata m s)
genMeta Shelley = do
  m <- arbitrary
  pure (MetaData m)

-- ==========================================================
-- Parameterized helper functions for generating Mary style Values

genAssetName :: Gen Mary.AssetName
genAssetName = (Mary.AssetName . fromString) <$> arbitrary

genPolicyID :: EraIndex e -> Gen (Mary.PolicyID e)
genPolicyID index = Mary.PolicyID <$> genScriptHash index

genScriptHash :: EraIndex e -> Gen(ScriptHash e)
genScriptHash Shelley = ScriptHash <$> genHash
genScriptHash Mary = ScriptHash <$> genHash
genScriptHash Allegra = ScriptHash <$> genHash

genMaryValue :: EraIndex era -> Gen(Mary.Value era)
genMaryValue index = do
   ada <- arbitrary
   size <- choose (0,10)
   triples <- vectorOf size (do { p <- genPolicyID index; n <- genAssetName; i <- choose (-3,50); pure(p,n,i)})
   pure $ Mary.valueFromList ada triples

-- ==========================================================
-- Parameterized helper function for generating MA style Metadata

genScriptSeq :: EraIndex e -> Gen(StrictSeq (Script e))
genScriptSeq index = do
  n <- choose (0,6)
  l <- vectorOf n (genScript index)
  pure (fromList l)

-- ==========================================================
-- EraIndex parameterized property tests

propertyAnn :: forall e t. (Eq t, Show t, ToCBOR t, FromCBOR(Annotator t)) =>
  String -> EraIndex e -> (EraIndex e -> Gen t) -> TestTree
propertyAnn name i gen = testProperty ("roundtripAnn "++name) $ do
  x <- gen i
  case roundTripAnn x of
    Right(left,_) | not(Lazy.null left) -> error("unconsumed trailing bytes: "++show left)
    Right(_,y) -> if (x==y) then pure True else error("Unequal\n   "++show x++"\n   "++show y)
    Left s -> error (show s)

property :: forall e t. (Eq t, Show t, ToCBOR t, FromCBOR t) =>
  String -> EraIndex e -> (EraIndex e -> Gen t) -> TestTree
property name i gen = testProperty ("roundtrip "++name) $ do
  x <- gen i
  case roundTrip x of
    Right(left,_) | not(Lazy.null left) -> error("unconsumed trailing bytes: "++show left)
    Right(_,y) -> if (x==y) then pure True else error("Unequal\n   "++show x++"\n   "++show y)
    Left s -> error (show s)


allprops ::
  ( ToCBOR (TxBody e),
    ToCBOR (Metadata e),
    ToCBOR (Value e),
    ToCBOR (Script e),
    Eq (TxBody e),
    Eq (Metadata e),
    Eq (Value e),
    Eq (Script e),
    Show (TxBody e),
    Show (Metadata e),
    Show (Value e),
    Show (Script e),
    FromCBOR (Value e),
    FromCBOR (Annotator (TxBody e)),
    FromCBOR (Annotator (Metadata e)),
    FromCBOR (Annotator (Script e))
  ) => EraIndex e -> TestTree
allprops index = testGroup (show index)
        [ propertyAnn "TxBody" index genTxBody
        , propertyAnn "Metadata" index genMeta
        , property "Value" index genValue
        , propertyAnn "Script" index genScript
        ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup "All Era Roundtrip Tests"
    [ allprops Shelley, allprops Allegra, allprops Mary ]