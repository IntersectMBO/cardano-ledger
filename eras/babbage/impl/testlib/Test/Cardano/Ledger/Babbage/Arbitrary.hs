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
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Plutus.TxInfo (TxOutSource)
import Control.State.Transition (STS (PredicateFailure))
import Data.Functor.Identity (Identity)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.QuickCheck

deriving instance Arbitrary CoinPerByte

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

instance Crypto crypto => Arbitrary (TxOutSource crypto) where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (BabbageContextError era)
  where
  -- Switch to this implementation once #4110 is taken care of
  -- arbitrary = genericArbitraryU
  arbitrary =
    oneof
      [ AlonzoContextError <$> arbitrary
      , ByronTxOutInContext <$> arbitrary
      , -- , RedeemerPointerPointsToNothing <$> arbitrary -- see #4110
        InlineDatumsNotSupported <$> arbitrary
      , ReferenceScriptsNotSupported <$> arbitrary
      , ReferenceInputsNotSupported <$> arbitrary
      ]

instance
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (BabbageUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
  , Arbitrary (TxCert era)
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (BabbageUtxowPredFailure era)
  where
  arbitrary = genericArbitraryU

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
  , Arbitrary (TxCert era)
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
