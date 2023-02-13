{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (
  sizedTimelock,
  maxTimelockDepth,
  genMintValues,
)
where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Allegra.Scripts (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.Allegra.Scripts as MA (Timelock (..))
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..))
import Cardano.Ledger.Binary (Annotator, FromCBOR, ToCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Cardano.Ledger.Shelley.API (KeyHash (KeyHash), ShelleyTxAuxData (ShelleyTxAuxData))
import Cardano.Ledger.Shelley.LedgerState (PPUPPredFailure)
import Data.Int (Int64)
import Data.Sequence.Strict (StrictSeq, fromList)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Mary.ValueSpec ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.TxAuxData (genMetadata')
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck (
  Arbitrary,
  arbitrary,
  choose,
  genericShrink,
  listOf,
  oneof,
  resize,
  scale,
  shrink,
  vectorOf,
 )
import Test.Tasty.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  AllegraEra Generators
  Generators used for roundtrip tests, generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

maxTimelockDepth :: Int
maxTimelockDepth = 3

maxTimelockListLens :: Int
maxTimelockListLens = 5

sizedTimelock ::
  Era era =>
  Int ->
  Gen (Timelock era)
sizedTimelock 0 = MA.RequireSignature . KeyHash . mkDummyHash <$> (arbitrary :: Gen Int)
sizedTimelock n =
  oneof
    [ MA.RequireSignature . KeyHash . mkDummyHash <$> (arbitrary :: Gen Int)
    , MA.RequireAllOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n - 1)))
            )
    , MA.RequireAnyOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n - 1)))
            )
    , do
        subs <- resize maxTimelockListLens (listOf (sizedTimelock (n - 1)))
        let i = length subs
        MA.RequireMOf <$> choose (0, i) <*> pure (fromList subs)
    , RequireTimeStart <$> arbitrary
    , RequireTimeExpire <$> arbitrary
    ]

-- TODO Generate metadata with script preimages
instance
  forall era c.
  ( Era era
  , c ~ EraCrypto era
  , Mock c
  , FromCBOR (Annotator (Timelock era))
  , ToCBOR (Script era)
  , Arbitrary (Script era)
  ) =>
  Arbitrary (AllegraTxAuxData era)
  where
  -- Why do we use the \case instead of a do statement? like this:
  --
  -- @
  -- arbitrary = do
  --   Metadata m <- genMetadata'
  --   MAAuxiliaryData m <$> genScriptSeq
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
  , Mock (EraCrypto era)
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PPUPPredFailure era)
  ) =>
  Arbitrary (AllegraUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (TxOut era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (AllegraTxBody era)
  where
  arbitrary =
    AllegraTxBody
      <$> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary

{-------------------------------------------------------------------------------
  MaryEra Generators
-------------------------------------------------------------------------------}

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (TxOut era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (MaryTxBody era)
  where
  arbitrary =
    MaryTxBody
      <$> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) genMintValues

-- | When generating values for the mint field, we do two things:
--
-- - Fix the ADA value to 0
-- - Allow both positive and negative quantities
genMintValues :: forall c. Crypto c => Gen (MultiAsset c)
genMintValues = multiAssetFromListBounded @Int64 <$> arbitrary

-- | Variant on @multiAssetFromList@ that makes sure that generated values stay
-- bounded within the range of a given integral type.
multiAssetFromListBounded ::
  forall i c.
  (Bounded i, Integral i) =>
  [(PolicyID c, AssetName, i)] ->
  MultiAsset c
multiAssetFromListBounded =
  foldr
    (\(p, n, fromIntegral -> i) ans -> ConcreteValue.insertMultiAsset comb p n i ans)
    mempty
  where
    comb :: Integer -> Integer -> Integer
    comb a b =
      max
        (fromIntegral $ minBound @i)
        (min (fromIntegral $ maxBound @i) (a + b))

{-------------------------------------------------------------------------------
  AllegraEra Generators
-------------------------------------------------------------------------------}

instance Era era => Arbitrary (Timelock era) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink
