{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Arbitrary (
  sizedTimelock,
  maxTimelockDepth,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  Timelock (..),
  ValidityInterval (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )

import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (AllegraTxBody))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (ShelleyTxAuxData (ShelleyTxAuxData))
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq, fromList)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.Arbitrary (genMetadata', sizedNativeScriptGens)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  choose,
  genericShrink,
  oneof,
  scale,
  vectorOf,
 )

maxTimelockDepth :: Int
maxTimelockDepth = 3

sizedTimelock ::
  AllegraEraScript era =>
  Int ->
  Gen (NativeScript era)
sizedTimelock 0 = RequireSignature <$> arbitrary
sizedTimelock n =
  oneof $
    sizedNativeScriptGens n
      <> [ RequireTimeStart <$> arbitrary
         , RequireTimeExpire <$> arbitrary
         ]

-- TODO Generate metadata with script preimages
instance
  forall era.
  ( AllegraEraScript era
  , NativeScript era ~ Timelock era
  , Arbitrary (Script era)
  , Era era
  ) =>
  Arbitrary (AllegraTxAuxData era)
  where
  -- Why do we use the \case instead of a do statement? like this:
  --
  -- @
  -- arbitrary = do
  --   ShelleyTxAuxData m <- genMetadata'
  --   AllegraTxAuxData m <$> genScriptSeq
  -- @
  --
  -- The above leads to an error about a failable
  -- pattern, despite the pattern being COMPLETE, resulting
  -- in an unsatisfied `MonadFail` constraint.
  arbitrary =
    genMetadata' @era >>= \case
      ShelleyTxAuxData m -> AllegraTxAuxData m <$> (genScriptSeq @era)

genScriptSeq :: Arbitrary (NativeScript era) => Gen (StrictSeq (NativeScript era))
genScriptSeq = do
  n <- choose (0, 3)
  l <- vectorOf n arbitrary
  pure (fromList l)

instance
  ( Era era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (EraRuleFailure "PPUP" era)
  ) =>
  Arbitrary (AllegraUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( EraTxOut era
  , EraTxCert era
  , Arbitrary (TxOut era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (TxCert era)
  ) =>
  Arbitrary (AllegraTxBody era)
  where
  arbitrary =
    AllegraTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary

instance
  ( AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  Arbitrary (Timelock era)
  where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink
