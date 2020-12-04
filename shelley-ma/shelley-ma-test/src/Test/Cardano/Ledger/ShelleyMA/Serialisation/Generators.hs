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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators
  ( sizedTimelock,
    maxTimelockDepth,
    genMintValues,
  )
where

import Cardano.Binary (toCBOR)
import Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import qualified Cardano.Ledger.Mary.Value as Mary
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.ShelleyMA (ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.Metadata as MA
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA.STS
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA (Timelock (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import Data.Coerce (coerce)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Generic.Random (genericArbitraryU)
import Shelley.Spec.Ledger.API hiding (SignedDSIGN, TxBody (..))
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
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.MetaData (genMetaData')
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  ShelleyMAEra Generators
  Generators used for roundtrip tests, generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

mkDummyHash :: forall h a. HashAlgorithm h => Int -> Hash.Hash h a
mkDummyHash = coerce . hashWithSerialiser @h toCBOR

maxTimelockDepth :: Int
maxTimelockDepth = 3

maxTimelockListLens :: Int
maxTimelockListLens = 5

sizedTimelock ::
  Era era =>
  Int ->
  Gen (Timelock era)
sizedTimelock 0 = (MA.RequireSignature . KeyHash . mkDummyHash) <$> arbitrary
sizedTimelock n =
  oneof
    [ (MA.RequireSignature . KeyHash . mkDummyHash) <$> arbitrary,
      MA.RequireAllOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n -1)))
            ),
      MA.RequireAnyOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n -1)))
            ),
      do
        subs <- resize maxTimelockListLens (listOf (sizedTimelock (n -1)))
        let i = length subs
        MA.RequireMOf <$> choose (0, i) <*> pure (fromList subs),
      RequireTimeStart <$> arbitrary,
      RequireTimeExpire <$> arbitrary
    ]

-- TODO Generate metadata with script preimages
instance
  (Mock c, Typeable ma, Arbitrary (Timelock (ShelleyMAEra ma c))) =>
  Arbitrary (MA.Metadata (ShelleyMAEra ma c))
  where
  -- Why do we use the \case instead of a do statement? like this:
  --
  -- @
  -- arbitrary = do
  --   MetaData m <- genMetaData'
  --   pure $ MA.Metadata m StrictSeq.empty
  -- @
  --
  -- The above leads to an error about a failable
  -- pattern, despite the pattern being COMPLETE, resulting
  -- in an unsatisfied `MonadFail` constraint.
  arbitrary =
    genMetaData' >>= \case
      MetaData m ->
        do ss <- genScriptSeq; pure (MA.Metadata m ss)

genScriptSeq ::
  (Arbitrary (Timelock (ShelleyMAEra ma c))) =>
  Gen (StrictSeq (Timelock (ShelleyMAEra ma c)))
genScriptSeq = do
  n <- choose (0, 3)
  l <- vectorOf n arbitrary
  pure (fromList l)

{-------------------------------------------------------------------------------
  MaryEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (MA.TxBody (MaryEra c)) where
  arbitrary =
    MA.TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues

instance Mock c => Arbitrary (Timelock (MaryEra c)) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Mock c => Arbitrary (Mary.PolicyID (MaryEra c)) where
  arbitrary = Mary.PolicyID <$> arbitrary

instance Mock c => Arbitrary (Mary.Value (MaryEra c)) where
  arbitrary = valueFromListBounded @Word64 <$> arbitrary <*> arbitrary

  shrink (Mary.Value ada assets) =
    concat
      [ -- Shrink the ADA value
        flip Mary.Value assets <$> shrink ada,
        -- Shrink the non-ADA assets by reducing the list length
        Mary.Value
          ada
          <$> shrink assets
      ]

-- | When generating values for the mint field, we do two things:
--
-- - Fix the ADA value to 0
-- - Allow both positive and negative quantities
genMintValues :: forall c. Mock c => Gen (Mary.Value (MaryEra c))
genMintValues = valueFromListBounded @Int64 0 <$> arbitrary

-- | Variant on @valueFromList@ that makes sure that generated values stay
-- bounded within the range of a given integral type.
valueFromListBounded ::
  forall i era.
  (Bounded i, Integral i) =>
  i ->
  [(Mary.PolicyID era, Mary.AssetName, i)] ->
  Mary.Value era
valueFromListBounded (fromIntegral -> ada) =
  foldr
    (\(p, n, fromIntegral -> i) ans -> ConcreteValue.insert comb p n i ans)
    (Mary.Value ada Map.empty)
  where
    comb :: Integer -> Integer -> Integer
    comb a b =
      max
        (fromIntegral $ minBound @i)
        (min (fromIntegral $ maxBound @i) (a + b))

instance Arbitrary Mary.AssetName where
  arbitrary = Mary.AssetName <$> arbitrary

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (MaryEra c)) where
  arbitrary = genericArbitraryU

{-------------------------------------------------------------------------------
  AllegraEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (MA.TxBody (AllegraEra c)) where
  arbitrary =
    MA.TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure (Coin 0)

instance Mock c => Arbitrary (Timelock (AllegraEra c)) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (AllegraEra c)) where
  arbitrary = genericArbitraryU

instance Mock c => Arbitrary (ConcreteValue.PolicyID (AllegraEra c)) where
  arbitrary = ConcreteValue.PolicyID <$> arbitrary

instance Mock c => Arbitrary (ConcreteValue.Value (AllegraEra c)) where
  arbitrary = ConcreteValue.Value <$> arbitrary <*> arbitrary
