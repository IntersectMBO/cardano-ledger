{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Cardano.Binary(toCBOR)
import Cardano.Ledger.Era(Era(..))
import Cardano.Ledger.ShelleyMA.Timelocks(Timelock(..), ValidityInterval(..))
import Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Core as Abstract
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as Mary (AssetID (..), PolicyID (..), Value (..))
import Cardano.Ledger.Shelley (ShelleyBased)
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA.STS
import qualified Cardano.Ledger.ShelleyMA.Scripts as MA (Timelock (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import Data.Coerce (coerce)
import Data.Sequence.Strict (fromList)
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
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Tasty.QuickCheck (Gen)
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Test.Shelley.Spec.Ledger.Serialisation.Generators() -- imports arbitray instance for MultiSig

{-------------------------------------------------------------------------------
  ShelleyMAEra Generators
  Generators used for roundtrip tests, generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

mkDummyHash :: forall h a. HashAlgorithm h => Int -> Hash.Hash h a
mkDummyHash = coerce . hashWithSerialiser @h toCBOR

genTxBody ::
  ( ShelleyBased era,
    Arbitrary (Abstract.Value era),
    Arbitrary (TxOut era),
    Arbitrary (DCert era),
    Arbitrary (Wdrl era),
    Arbitrary (Update era)
  ) =>
  Gen (MA.TxBody era)
genTxBody =
  MA.TxBody
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

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

{-------------------------------------------------------------------------------
  MaryEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (MA.TxBody (MaryEra c)) where
  arbitrary = genTxBody

instance Mock c => Arbitrary (Timelock (MaryEra c)) where
  arbitrary = sizedTimelock maxTimelockDepth


instance Mock c => Arbitrary (Mary.PolicyID (MaryEra c)) where
  arbitrary = Mary.PolicyID <$> arbitrary

instance Mock c => Arbitrary (Mary.Value (MaryEra c)) where
  arbitrary = Mary.Value <$> arbitrary <*> arbitrary

instance Arbitrary Mary.AssetID where
  arbitrary = Mary.AssetID <$> arbitrary

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (MaryEra c)) where
  arbitrary = genericArbitraryU

{-------------------------------------------------------------------------------
  AllegraEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (MA.TxBody (AllegraEra c)) where
  arbitrary = genTxBody

instance Mock c => Arbitrary (Timelock (AllegraEra c)) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (AllegraEra c)) where
  arbitrary = genericArbitraryU

instance Mock c => Arbitrary (ConcreteValue.PolicyID  (AllegraEra c)) where
  arbitrary = ConcreteValue.PolicyID <$> arbitrary

instance Mock c => Arbitrary (ConcreteValue.Value  (AllegraEra c)) where
  arbitrary = ConcreteValue.Value <$> arbitrary <*> arbitrary
