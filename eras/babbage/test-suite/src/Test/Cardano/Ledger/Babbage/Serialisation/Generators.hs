{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Generators where

import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody, BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized, ToCBOR, mkSized)
import Cardano.Ledger.Core
import Control.State.Transition (STS (PredicateFailure))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance (Era era, ToCBOR (f era), Arbitrary (f era)) => Arbitrary (Sized (f era)) where
  arbitrary = mkSized (eraProtVerHigh @era) <$> arbitrary

instance
  ( EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Value era),
    Arbitrary (Script era)
  ) =>
  Arbitrary (BabbageTxOut era)
  where
  arbitrary =
    BabbageTxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( Mock (EraCrypto era),
    BabbageEraTxBody era,
    Arbitrary (Value era),
    Arbitrary (Script era)
  ) =>
  Arbitrary (BabbageTxBody era)
  where
  arbitrary =
    BabbageTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- ==========================
--

instance Arbitrary (BabbagePParams era) where
  arbitrary =
    BabbagePParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (BabbagePParamsUpdate era) where
  arbitrary =
    BabbagePParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Value era),
    Arbitrary (TxOut era),
    Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (BabbageUtxoPredFailure era)
  where
  arbitrary =
    oneof
      [ AlonzoInBabbageUtxoPredFailure <$> arbitrary,
        IncorrectTotalCollateralField <$> arbitrary <*> arbitrary
      ]

instance
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Arbitrary (BabbageUtxowPredFailure era)
  where
  arbitrary =
    oneof
      [ AlonzoInBabbageUtxowPredFailure <$> arbitrary,
        UtxoFailure <$> arbitrary,
        MalformedScriptWitnesses <$> arbitrary,
        MalformedReferenceScripts <$> arbitrary
      ]
