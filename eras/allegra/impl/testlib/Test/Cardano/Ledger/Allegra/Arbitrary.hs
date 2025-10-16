{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Arbitrary (
  sizedTimelock,
  maxTimelockDepth,
) where

import Cardano.Ledger.Allegra (AllegraEra, Tx (..))
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  Timelock (..),
  ValidityInterval (..),
#if __GLASGOW_HASKELL__ >= 914
  data RequireTimeExpire,
  data RequireTimeStart,
#else
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
#endif
 )
import Cardano.Ledger.Allegra.Transition
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
#if __GLASGOW_HASKELL__ >= 914
import Cardano.Ledger.Allegra.TxBody (data AllegraTxBody)
#else
import Cardano.Ledger.Allegra.TxBody (pattern AllegraTxBody)
#endif
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (ShelleyTxAuxData (ShelleyTxAuxData))
import Cardano.Ledger.Shelley.Scripts (
#if __GLASGOW_HASKELL__ >= 914
  data RequireSignature,
#else
  pattern RequireSignature,
#endif
 )
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
  , Arbitrary (NativeScript era)
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  , Arbitrary (Script era)
#endif
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

instance Arbitrary (TxBody AllegraEra) where
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

deriving newtype instance Arbitrary (TransitionConfig AllegraEra)

deriving newtype instance Arbitrary (Tx AllegraEra)
