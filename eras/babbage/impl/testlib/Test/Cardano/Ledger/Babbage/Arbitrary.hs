{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Arbitrary () where

import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Language (Language (..))
import Control.State.Transition (STS (PredicateFailure))
import Data.Functor.Identity (Identity)
import Test.Cardano.Ledger.Alonzo.Arbitrary (genAlonzoScript)
import Test.QuickCheck

deriving instance Arbitrary CoinPerByte

instance Crypto c => Arbitrary (AlonzoScript (BabbageEra c)) where
  arbitrary = genAlonzoScript [PlutusV1, PlutusV2]

instance Arbitrary (BabbagePParams Identity era) where
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

instance Arbitrary (BabbagePParams StrictMaybe era) where
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
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (BabbageUtxoPredFailure era)
  where
  arbitrary =
    oneof
      [ AlonzoInBabbageUtxoPredFailure <$> arbitrary
      , IncorrectTotalCollateralField <$> arbitrary <*> arbitrary
      ]

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
  , Arbitrary (DCert era)
  ) =>
  Arbitrary (BabbageUtxowPredFailure era)
  where
  arbitrary =
    oneof
      [ AlonzoInBabbageUtxowPredFailure <$> arbitrary
      , UtxoFailure <$> arbitrary
      , MalformedScriptWitnesses <$> arbitrary
      , MalformedReferenceScripts <$> arbitrary
      ]

instance
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) =>
  Arbitrary (BabbageTxOut era)
  where
  arbitrary =
    BabbageTxOut
      <$> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( BabbageEraTxBody era
  , Arbitrary (Sized (TxOut era))
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (DCert era)
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
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
