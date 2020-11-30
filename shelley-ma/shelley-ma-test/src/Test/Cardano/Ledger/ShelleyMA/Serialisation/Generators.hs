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
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators
  ( sizedTimelock,
    maxTimelockDepth,
  )
where

import Cardano.Binary (toCBOR)
import Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.ShelleyMA (ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.Metadata as MA
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA.STS
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA (Timelock (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import Data.Coerce (coerce)
import Data.Sequence.Strict (fromList)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
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
      MA.RequireAllOf <$> (fromList <$> resize maxTimelockListLens (listOf (sizedTimelock (n -1)))),
      MA.RequireAnyOf <$> (fromList <$> resize maxTimelockListLens (listOf (sizedTimelock (n -1)))),
      do
        subs <- resize maxTimelockListLens (listOf (sizedTimelock (n -1)))
        let i = length subs
        MA.RequireMOf <$> choose (0, i) <*> pure (fromList subs),
      RequireTimeStart <$> arbitrary,
      RequireTimeExpire <$> arbitrary
    ]

-- TODO Generate metadata with script preimages
instance
  (Mock c, Typeable ma) =>
  Arbitrary (MA.Metadata (ShelleyMAEra ma c))
  where
  -- Why do we do this rather than:
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
        pure $ MA.Metadata m StrictSeq.empty

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
  arbitrary = Mary.Value <$> (abs <$> arbitrary) <*> (pointwiseAbs <$> arbitrary)
    where
      pointwiseAbs = fmap (fmap abs)

genMintValues :: Mock c => Gen (Mary.Value (MaryEra c))
genMintValues = Mary.Value 0 <$> arbitrary

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
