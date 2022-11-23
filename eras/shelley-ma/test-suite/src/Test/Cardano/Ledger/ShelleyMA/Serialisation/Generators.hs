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

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators
  ( sizedTimelock,
    maxTimelockDepth,
    genMintValues,
  )
where

import Cardano.Ledger.Binary (Annotator, FromCBOR, ToCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Cardano.Ledger.Shelley.API (KeyHash (KeyHash), ShelleyTxAuxData (ShelleyTxAuxData))
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AllegraTxAuxData (..))
import Cardano.Ledger.ShelleyMA.Rules (ShelleyMAUtxoPredFailure)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA (Timelock (..))
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody (..))
import Control.State.Transition (PredicateFailure)
import qualified Data.ByteString.Short as SBS
import Data.Int (Int64)
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Word (Word64)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Metadata (genMetadata')
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    choose,
    genericShrink,
    listOf,
    oneof,
    resize,
    shrink,
    vectorOf,
  )
import Test.Tasty.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  ShelleyMAEra Generators
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
    [ MA.RequireSignature . KeyHash . mkDummyHash <$> (arbitrary :: Gen Int),
      MA.RequireAllOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n - 1)))
            ),
      MA.RequireAnyOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n - 1)))
            ),
      do
        subs <- resize maxTimelockListLens (listOf (sizedTimelock (n - 1)))
        let i = length subs
        MA.RequireMOf <$> choose (0, i) <*> pure (fromList subs),
      RequireTimeStart <$> arbitrary,
      RequireTimeExpire <$> arbitrary
    ]

-- TODO Generate metadata with script preimages
instance
  forall era c.
  ( Era era,
    c ~ EraCrypto era,
    Mock c,
    FromCBOR (Annotator (Script era)),
    ToCBOR (Script era),
    Arbitrary (Script era)
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

genScriptSeq ::
  forall era. Arbitrary (Script era) => Gen (StrictSeq (Script era))
genScriptSeq = do
  n <- choose (0, 3)
  l <- vectorOf n arbitrary
  pure (fromList l)

instance
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (Value era),
    Arbitrary (TxOut era),
    Arbitrary (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Arbitrary (ShelleyMAUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (Value era)) =>
  Arbitrary (MATxBody era)
  where
  arbitrary =
    MATxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues

{-------------------------------------------------------------------------------
  MaryEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (PolicyID c) where
  arbitrary = PolicyID <$> arbitrary

instance Mock c => Arbitrary (MultiAsset c) where
  arbitrary = MultiAsset <$> arbitrary

instance Mock c => Arbitrary (MaryValue c) where
  arbitrary = MaryValue <$> (fromIntegral <$> positives) <*> (multiAssetFromListBounded <$> triples)
    where
      triples = arbitrary :: Gen [(PolicyID c, AssetName, Word64)]
      positives = arbitrary :: Gen Word64

  shrink (MaryValue ada assets) =
    concat
      [ -- Shrink the ADA value
        flip MaryValue assets <$> shrink ada,
        -- Shrink the non-ADA assets by reducing the list length
        MaryValue ada <$> shrink assets
      ]

-- | When generating values for the mint field, we do two things:
--
-- - Fix the ADA value to 0
-- - Allow both positive and negative quantities
genMintValues :: forall c. Mock c => Gen (MultiAsset c)
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

instance Arbitrary AssetName where
  arbitrary = AssetName . SBS.pack . take 32 . SBS.unpack <$> arbitrary

{-------------------------------------------------------------------------------
  AllegraEra Generators
-------------------------------------------------------------------------------}

instance Era era => Arbitrary (Timelock era) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink
