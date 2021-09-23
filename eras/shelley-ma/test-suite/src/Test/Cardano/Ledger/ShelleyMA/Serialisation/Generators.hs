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

import Cardano.Binary (Annotator, FromCBOR, ToCBOR (toCBOR))
import Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import qualified Cardano.Ledger.Mary.Value as Mary
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.Shelley.API hiding (SignedDSIGN, TxBody (..))
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA.STS
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA (Timelock (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Word (Word64)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Metadata (genMetadata')
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
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

mkDummyHash :: forall h a. HashAlgorithm h => Int -> Hash.Hash h a
mkDummyHash = coerce . hashWithSerialiser @h toCBOR

maxTimelockDepth :: Int
maxTimelockDepth = 3

maxTimelockListLens :: Int
maxTimelockListLens = 5

sizedTimelock ::
  CC.Crypto crypto =>
  Int ->
  Gen (Timelock crypto)
sizedTimelock 0 = MA.RequireSignature . KeyHash . mkDummyHash <$> arbitrary
sizedTimelock n =
  oneof
    [ MA.RequireSignature . KeyHash . mkDummyHash <$> arbitrary,
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
  forall era c.
  ( Era era,
    c ~ Crypto era,
    Mock c,
    FromCBOR (Annotator (Core.Script era)),
    ToCBOR (Core.Script era),
    Ord (Core.Script era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (MA.AuxiliaryData era)
  where
  -- Why do we use the \case instead of a do statement? like this:
  --
  -- @
  -- arbitrary = do
  --   Metadata m <- genMetadata'
  --   MA.AuxiliaryData m <$> genScriptSeq
  -- @
  --
  -- The above leads to an error about a failable
  -- pattern, despite the pattern being COMPLETE, resulting
  -- in an unsatisfied `MonadFail` constraint.
  arbitrary =
    genMetadata' >>= \case
      Metadata m -> MA.AuxiliaryData m <$> (genScriptSeq @era)

genScriptSeq ::
  forall era. Arbitrary (Core.Script era) => Gen (StrictSeq (Core.Script era))
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

instance Mock c => Arbitrary (Mary.PolicyID c) where
  arbitrary = Mary.PolicyID <$> arbitrary

instance Mock c => Arbitrary (Mary.Value c) where
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
genMintValues :: forall c. Mock c => Gen (Mary.Value c)
genMintValues = valueFromListBounded @Int64 0 <$> arbitrary

-- | Variant on @valueFromList@ that makes sure that generated values stay
-- bounded within the range of a given integral type.
valueFromListBounded ::
  forall i crypto.
  (Bounded i, Integral i) =>
  i ->
  [(Mary.PolicyID crypto, Mary.AssetName, i)] ->
  Mary.Value crypto
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
  arbitrary = Mary.AssetName . BS.pack . take 32 . BS.unpack <$> arbitrary

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

instance Mock c => Arbitrary (Timelock c) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (AllegraEra c)) where
  arbitrary = genericArbitraryU
