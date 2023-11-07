{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Translation.TranslatableGen (
  genTx,
  genTxOut,
  utxoWithTx,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, AlonzoScript (..), ExUnits (..), Tag (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Babbage (Babbage, BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxOut (..), Datum (..))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Data.Map as Map
import Data.Maybe.Strict
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.Arbitrary (genScripts)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  TxInfoLanguage (..),
 )
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck (
  Arbitrary,
  Gen,
  arbitrary,
  elements,
  frequency,
  listOf1,
  oneof,
  scale,
  vectorOf,
 )

instance TranslatableGen Babbage where
  tgTx l = genTx @Babbage (genTxBody l)
  tgUtxo = utxoWithTx @Babbage
  mkTxInfoLanguage PlutusV1 = TxInfoLanguage SPlutusV1
  mkTxInfoLanguage PlutusV2 = TxInfoLanguage SPlutusV2
  mkTxInfoLanguage lang =
    error $ "Language " ++ show lang ++ " is not supported in " ++ eraName @Babbage

utxoWithTx ::
  forall era.
  ( EraTx era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , TxOut era ~ BabbageTxOut era
  ) =>
  Language ->
  Tx era ->
  Gen (UTxO era)
utxoWithTx l tx = do
  let allIns = tx ^. bodyTxL ^. allInputsTxBodyF
  outs <- vectorOf (length allIns) (genTxOut @era l)
  pure $ UTxO (Map.fromList $ Set.toList allIns `zip` outs)

genTx ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , Arbitrary (Script era)
  , AlonzoEraScript era
  , AlonzoScript era ~ Script era
  , AlonzoTxWits era ~ TxWits era
  ) =>
  Gen (TxBody era) ->
  Gen (AlonzoTx era)
genTx txbGen =
  AlonzoTx
    <$> txbGen
    <*> genTxWits @era
    <*> arbitrary
    <*> arbitrary

genTxOut ::
  forall era.
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) =>
  Language ->
  Gen (BabbageTxOut era)
genTxOut l = do
  addr <- genNonByronAddr @(EraCrypto era)
  value <- scale (`div` 15) arbitrary
  script <- case l of
    PlutusV1 -> pure SNothing
    _ -> arbitrary
  datum <- case l of
    PlutusV1 -> oneof [pure NoDatum, DatumHash <$> (arbitrary :: Gen (DataHash (EraCrypto era)))]
    _ -> arbitrary
  pure $ BabbageTxOut addr value datum script

genTxBody :: forall c. Crypto c => Language -> Gen (BabbageTxBody (BabbageEra c))
genTxBody l = do
  let genTxOuts = fromList <$> listOf1 (mkSized (eraProtVerLow @Babbage) <$> genTxOut @(BabbageEra c) l)
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn c))
  BabbageTxBody
    <$> genTxIns
    <*> arbitrary
    <*> ( case l of -- refinputs
            PlutusV1 -> pure Set.empty
            _ -> arbitrary
        )
    <*> genTxOuts
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

genNonByronAddr :: forall c. Crypto c => Gen (Addr c)
genNonByronAddr =
  Addr
    <$> arbitrary
    <*> arbitrary
    <*> frequency
      [ (85, StakeRefBase <$> arbitrary)
      , (15, pure StakeRefNull)
      ]

genTxWits ::
  ( Arbitrary (Script era)
  , AlonzoScript era ~ Script era
  , AlonzoEraScript era
  ) =>
  Gen (AlonzoTxWits era)
genTxWits =
  AlonzoTxWits
    <$> arbitrary
    <*> arbitrary
    <*> genScripts
    <*> arbitrary
    <*> genRedeemers

genRedeemers :: forall era. Era era => Gen (Redeemers era)
genRedeemers = do
  d <- arbitrary :: Gen (Data era)
  eu <- arbitrary :: Gen ExUnits
  -- We provide `RdrmPtr Spend 0` as the only valid reedemer, because
  -- for any other redeemer type, we would have to modify the body of the transaction
  -- in order for the translation to succeed
  Redeemers <$> elements [Map.singleton (RdmrPtr Spend 0) (d, eu), Map.empty]
