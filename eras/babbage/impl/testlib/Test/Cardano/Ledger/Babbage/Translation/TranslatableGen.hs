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
import Cardano.Ledger.Alonzo.Plutus.Context (SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Babbage (BabbageEra, Tx (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), TxBody (BabbageTxBody))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.Arbitrary (genScripts)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (TranslatableGen (..))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
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

instance TranslatableGen BabbageEra where
  tgRedeemers = genRedeemers
  tgTx l = MkBabbageTx <$> genTx @BabbageEra (genTxBody l)
  tgUtxo = utxoWithTx @BabbageEra

utxoWithTx ::
  forall era.
  ( EraTx era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , TxOut era ~ BabbageTxOut era
  ) =>
  SupportedLanguage era ->
  Tx era ->
  Gen (UTxO era)
utxoWithTx l tx = do
  let allIns = tx ^. bodyTxL ^. allInputsTxBodyF
  outs <- vectorOf (length allIns) (genTxOut @era l)
  pure $ UTxO (Map.fromList $ Set.toList allIns `zip` outs)

genTx ::
  forall era.
  ( TranslatableGen era
  , Arbitrary (TxAuxData era)
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
  SupportedLanguage era ->
  Gen (BabbageTxOut era)
genTxOut (SupportedLanguage slang) = do
  addr <- genNonByronAddr
  value <- scale (`div` 15) arbitrary
  script <- case slang of
    SPlutusV1 -> pure SNothing
    _ -> arbitrary
  datum <- case slang of
    SPlutusV1 -> oneof [pure NoDatum, DatumHash <$> (arbitrary :: Gen DataHash)]
    _ -> arbitrary
  pure $ BabbageTxOut addr value datum script

genTxBody :: SupportedLanguage BabbageEra -> Gen (TxBody BabbageEra)
genTxBody l@(SupportedLanguage slang) = do
  let genTxOuts = fromList <$> listOf1 (mkSized (eraProtVerLow @BabbageEra) <$> genTxOut @BabbageEra l)
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen TxIn)
  BabbageTxBody
    <$> genTxIns
    <*> arbitrary
    <*> ( case slang of -- refinputs
            SPlutusV1 -> pure Set.empty
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

genNonByronAddr :: Gen Addr
genNonByronAddr =
  Addr
    <$> arbitrary
    <*> arbitrary
    <*> frequency
      [ (85, StakeRefBase <$> arbitrary)
      , (15, pure StakeRefNull)
      ]

genTxWits ::
  TranslatableGen era =>
  Gen (AlonzoTxWits era)
genTxWits =
  AlonzoTxWits
    <$> arbitrary
    <*> arbitrary
    <*> genScripts
    <*> arbitrary
    <*> tgRedeemers

genRedeemers ::
  forall era.
  (AlonzoEraScript era, PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era) =>
  Gen (Redeemers era)
genRedeemers = do
  d <- arbitrary :: Gen (Data era)
  eu <- arbitrary :: Gen ExUnits
  -- We provide `RdrmPtr Spend 0` as the only valid reedemer, because
  -- for any other redeemer type, we would have to modify the body of the transaction
  -- in order for the translation to succeed
  Redeemers <$> elements [Map.singleton (AlonzoSpending $ AsIx 0) (d, eu), Map.empty]
