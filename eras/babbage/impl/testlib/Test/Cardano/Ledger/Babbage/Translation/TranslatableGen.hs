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

import Cardano.Ledger.Alonzo.Plutus.Context (SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
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
  tgTx l = genTx @BabbageEra (asSTxTopLevel <$> genTxBody l)
  tgUtxo = utxoWithTx @BabbageEra

utxoWithTx ::
  forall era.
  ( EraTx era
  , BabbageEraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) =>
  SupportedLanguage era ->
  Tx TopTx era ->
  Gen (UTxO era)
utxoWithTx l tx = do
  let allIns = tx ^. bodyTxL ^. allInputsTxBodyF
  outs <- vectorOf (length allIns) (genTxOut @era l)
  pure $ UTxO (Map.fromList $ Set.toList allIns `zip` outs)

genTx ::
  forall era.
  ( TranslatableGen era
  , AlonzoEraTx era
  , Arbitrary (TxAuxData era)
  , AlonzoTxWits era ~ TxWits era
  ) =>
  Gen (TxBody TopTx era) ->
  Gen (Tx TopTx era)
genTx txbGen = do
  txb <- txbGen
  wits <- genTxWits @era
  isValid <- arbitrary
  auxData <- arbitrary
  pure $
    mkBasicTx txb
      & witsTxL .~ wits
      & isValidTxL .~ isValid
      & auxDataTxL .~ auxData

genTxOut ::
  forall era.
  ( BabbageEraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) =>
  SupportedLanguage era ->
  Gen (TxOut era)
genTxOut (SupportedLanguage slang) = do
  addr <- genNonByronAddr
  value <- scale (`div` 15) arbitrary
  script <- case slang of
    SPlutusV1 -> pure SNothing
    _ -> arbitrary
  datum <- case slang of
    SPlutusV1 -> oneof [pure NoDatum, DatumHash <$> (arbitrary :: Gen DataHash)]
    _ -> arbitrary
  pure $
    mkBasicTxOut addr value
      & datumTxOutL .~ datum
      & referenceScriptTxOutL .~ script

genTxBody :: SupportedLanguage BabbageEra -> Gen (TxBody TopTx BabbageEra)
genTxBody l@(SupportedLanguage slang) = do
  let genTxOuts = fromList <$> listOf1 (genTxOut @BabbageEra l)
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen TxIn)
  txIns <- genTxIns
  collIns <- arbitrary
  refIns <- case slang of
    SPlutusV1 -> pure Set.empty
    _ -> arbitrary
  txOuts <- genTxOuts
  collReturn <- arbitrary
  totColl <- arbitrary
  certs <- arbitrary
  withdrawals <- arbitrary
  fee <- arbitrary
  vldt <- arbitrary
  update <- scale (`div` 15) arbitrary
  reqSignerHashes <- arbitrary
  mint <- scale (`div` 15) arbitrary
  scriptIntegrityHash <- arbitrary
  adHash <- arbitrary
  txNetworkId <- arbitrary
  pure $
    mkBasicTxBody
      & inputsTxBodyL .~ txIns
      & collateralInputsTxBodyL .~ collIns
      & referenceInputsTxBodyL .~ refIns
      & outputsTxBodyL .~ txOuts
      & collateralReturnTxBodyL .~ collReturn
      & totalCollateralTxBodyL .~ totColl
      & certsTxBodyL .~ certs
      & withdrawalsTxBodyL .~ withdrawals
      & feeTxBodyL .~ fee
      & vldtTxBodyL .~ vldt
      & updateTxBodyL .~ update
      & reqSignerHashesTxBodyL .~ reqSignerHashes
      & mintTxBodyL .~ mint
      & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      & auxDataHashTxBodyL .~ adHash
      & networkIdTxBodyL .~ txNetworkId

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
  ( TranslatableGen era
  , AlonzoEraTxWits era
  ) =>
  Gen (TxWits era)
genTxWits = do
  addrWits <- arbitrary
  bootAddrWits <- arbitrary
  scripts <- genScripts
  datums <- arbitrary
  redeemers <- tgRedeemers
  pure $
    mkBasicTxWits
      & addrTxWitsL .~ addrWits
      & bootAddrTxWitsL .~ bootAddrWits
      & scriptTxWitsL .~ scripts
      & datsTxWitsL .~ datums
      & rdmrsTxWitsL .~ redeemers

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
