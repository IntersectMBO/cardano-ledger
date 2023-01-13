{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Generators where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Binary (Sized, Term (..), ToCBOR, mkSized)
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.Val (Val)
import Control.State.Transition (STS (PredicateFailure))
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..), emptyOrNothing, toTerm, twiddleStrictMaybe)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance (Era era, ToCBOR (f era), Arbitrary (f era)) => Arbitrary (Sized (f era)) where
  arbitrary = mkSized (eraProtVerHigh @era) <$> arbitrary

instance
  ( EraTxOut era
  , Mock (EraCrypto era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
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
  ( Mock (EraCrypto era)
  , BabbageEraTxBody era
  , Arbitrary (Sized (TxOut era))
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
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

instance
  ( EraTxOut era
  , Mock (EraCrypto era)
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
  , Mock (EraCrypto era)
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
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
  ( Era era
  , ToCBOR (PParamsUpdate era)
  ) =>
  Twiddle (Update era)
  where
  twiddle v = twiddle v . toTerm v

instance Twiddle a => Twiddle (Sized a)

instance
  ( Era era
  , Val (Value era)
  , ToCBOR (Value era)
  , ToCBOR (Script era)
  ) =>
  Twiddle (BabbageTxOut era)
  where
  twiddle v = twiddle v . toTerm v

instance
  ( Era era
  , Twiddle (TxOut era)
  , BabbageEraTxBody era
  ) =>
  Twiddle (BabbageTxBody era)
  where
  twiddle v txBody = do
    inputs' <- twiddle v $ btbInputs txBody
    outputs' <- twiddle v $ btbOutputs txBody
    fee' <- twiddle v $ btbTxFee txBody
    -- Empty collateral can be represented by empty set or the
    -- value can be omitted entirely
    ttl' <- twiddleStrictMaybe v . invalidHereafter $ btbValidityInterval txBody
    cert' <- emptyOrNothing v $ btbCerts txBody
    wdrls' <- twiddle v $ btbWdrls txBody
    update' <- twiddleStrictMaybe v $ btbUpdate txBody
    auxDataHash' <- twiddleStrictMaybe v $ btbAuxDataHash txBody
    validityStart' <- twiddleStrictMaybe v . invalidBefore $ btbValidityInterval txBody
    mint' <- twiddle v $ btbMint txBody
    scriptDataHash' <- twiddleStrictMaybe v $ btbScriptIntegrityHash txBody
    collateral' <- emptyOrNothing v $ btbCollateral txBody
    requiredSigners' <- emptyOrNothing v $ btbReqSignerHashes txBody
    networkId' <- twiddleStrictMaybe v $ btbTxNetworkId txBody
    collateralReturn <- twiddleStrictMaybe v $ btbCollateralReturn txBody
    totalCollateral <- twiddleStrictMaybe v $ btbTotalCollateral txBody
    referenceInputs <- emptyOrNothing v $ btbReferenceInputs txBody
    mp <- elements [TMap, TMapI]
    let fields =
          [ (TInt 0, inputs')
          , (TInt 1, outputs')
          , (TInt 2, fee')
          ]
            <> catMaybes
              [ (TInt 3,) <$> ttl'
              , (TInt 4,) <$> cert'
              , (TInt 5,) <$> Just wdrls'
              , (TInt 6,) <$> update'
              , (TInt 7,) <$> auxDataHash'
              , (TInt 8,) <$> validityStart'
              , (TInt 9,) <$> Just mint'
              , (TInt 11,) <$> scriptDataHash'
              , (TInt 13,) <$> collateral'
              , (TInt 14,) <$> requiredSigners'
              , (TInt 15,) <$> networkId'
              , (TInt 16,) <$> collateralReturn
              , (TInt 17,) <$> totalCollateral
              , (TInt 18,) <$> referenceInputs
              ]
    fields' <- shuffle fields
    pure $ mp fields'
