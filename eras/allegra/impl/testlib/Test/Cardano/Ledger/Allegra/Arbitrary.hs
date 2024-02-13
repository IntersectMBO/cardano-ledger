{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Arbitrary (
  sizedTimelock,
  maxTimelockDepth,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Allegra.Scripts (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (AllegraTxBody))
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (KeyHash (KeyHash), ShelleyTxAuxData (ShelleyTxAuxData))
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq, fromList)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Shelley.Arbitrary (genMetadata')
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, choose, genericShrink, listOf, oneof, resize, scale, vectorOf)

maxTimelockDepth :: Int
maxTimelockDepth = 3

maxTimelockListLens :: Int
maxTimelockListLens = 5

sizedTimelock ::
  Era era =>
  Int ->
  Gen (Timelock era)
sizedTimelock 0 = RequireSignature . KeyHash . mkDummyHash <$> (arbitrary :: Gen Int)
sizedTimelock n =
  oneof
    [ RequireSignature . KeyHash . mkDummyHash <$> (arbitrary :: Gen Int)
    , RequireAllOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n - 1)))
            )
    , RequireAnyOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n - 1)))
            )
    , do
        subs <- resize maxTimelockListLens (listOf (sizedTimelock (n - 1)))
        let i = length subs
        RequireMOf <$> choose (0, i) <*> pure (fromList subs)
    , RequireTimeStart <$> arbitrary
    , RequireTimeExpire <$> arbitrary
    ]

-- TODO Generate metadata with script preimages
instance
  forall era.
  ( EncCBOR (Script era)
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

genScriptSeq :: Era era => Gen (StrictSeq (Timelock era))
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

instance Era era => Arbitrary (Timelock era) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink
