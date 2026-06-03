{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Ledger () where

import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (
  AllegraEraTxBody (..),
  AlonzoEraTx (..),
  AlonzoEraTxBody (..),
  BabbageEraTxBody (..),
  ConwayEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  ScriptHash,
  SubTx,
  TopTx,
  TxLevel (..),
  txIdTx,
  txStAnnTxG,
 )
import Cardano.Ledger.Dijkstra.Tx (DijkstraStAnnTx)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe)
import qualified Data.OMap.Strict as OMap
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (toSpecRepTuple)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Utxo ()

instance SpecTranslate DijkstraEra (Shelley.LedgerEnv DijkstraEra) where
  type SpecRep DijkstraEra (Shelley.LedgerEnv DijkstraEra) = Agda.LedgerEnv
  type
    SpecContext DijkstraEra (Shelley.LedgerEnv DijkstraEra) =
      (StrictMaybe ScriptHash, Conway.EnactState DijkstraEra)

  toSpecRep Shelley.LedgerEnv {..} = do
    (policyHash, enactState) <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkLedgerEnv
        <$> toSpecRep ledgerSlotNo
        <*> toSpecRep policyHash
        <*> toSpecRep ledgerPp
        <*> toSpecRep enactState
        <*> toSpecRep (casTreasury ledgerAccount)

instance SpecTranslate DijkstraEra (LedgerState DijkstraEra) where
  type SpecRep DijkstraEra (LedgerState DijkstraEra) = Agda.LedgerState

  type SpecContext DijkstraEra (LedgerState DijkstraEra) = Network

  toSpecRep (LedgerState {..}) =
    Agda.MkLedgerState
      <$> withCtxSpecTransM () (toSpecRep lsUTxOState)
      <*> withCtxSpecTransM () (toSpecRep (utxosGovState lsUTxOState ^. proposalsGovStateL))
      <*> toSpecRep lsCertState

instance SpecTranslate DijkstraEra (TxBody TopTx DijkstraEra) where
  type SpecRep DijkstraEra (TxBody TopTx DijkstraEra) = Agda.TxBodyTop
  type SpecContext DijkstraEra (TxBody TopTx DijkstraEra) = TxId

  toSpecRep txb = do
    txId <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkTxBodyTop
        <$> toSpecRep (txb ^. inputsTxBodyL)
        <*> toSpecRep (txb ^. referenceInputsTxBodyL)
        <*> toSpecRep (txb ^. collateralInputsTxBodyL)
        <*> (Agda.MkHSMap . zip [0 ..] <$> toSpecRep (txb ^. outputsTxBodyL))
        <*> toSpecRep txId
        <*> toSpecRep (txb ^. certsTxBodyL)
        <*> toSpecRep (txb ^. feeTxBodyL)
        <*> toSpecRep (txb ^. withdrawalsTxBodyL)
        <*> toSpecRep (txb ^. vldtTxBodyL)
        <*> toSpecRep (txb ^. auxDataHashTxBodyL)
        <*> toSpecRep (txb ^. treasuryDonationTxBodyL)
        <*> toSpecRep (txb ^. votingProceduresTxBodyL)
        <*> toSpecRep (txb ^. proposalProceduresTxBodyL)
        <*> toSpecRep (txb ^. networkIdTxBodyL)
        <*> toSpecRep (txb ^. currentTreasuryValueTxBodyL)
        <*> pure 0
        <*> fmap (fmap (const 0)) (toSpecRep (txb ^. scriptIntegrityHashTxBodyL))
        <*> traverse toSpecRep (toList $ OMap.toMap $ txb ^. subTransactionsTxBodyL)
        <*> (Agda.MkHSSet <$> toSpecRep (txb ^. guardsTxBodyL))
        <*> toSpecRep (txb ^. directDepositsTxBodyL)
        <*> toSpecRep (txb ^. accountBalanceIntervalsTxBodyL)

instance SpecTranslate DijkstraEra (TxBody SubTx DijkstraEra) where
  type SpecRep DijkstraEra (TxBody SubTx DijkstraEra) = Agda.TxBodySub
  type SpecContext DijkstraEra (TxBody SubTx DijkstraEra) = TxId

  toSpecRep txb = do
    txId <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkTxBodySub
        <$> toSpecRep (txb ^. inputsTxBodyL)
        <*> toSpecRep (txb ^. referenceInputsTxBodyL)
        <*> (Agda.MkHSMap . zip [0 ..] <$> toSpecRep (txb ^. outputsTxBodyL))
        <*> toSpecRep txId
        <*> toSpecRep (txb ^. certsTxBodyL)
        <*> toSpecRep (txb ^. withdrawalsTxBodyL)
        <*> toSpecRep (txb ^. vldtTxBodyL)
        <*> toSpecRep (txb ^. auxDataHashTxBodyL)
        <*> toSpecRep (txb ^. treasuryDonationTxBodyL)
        <*> toSpecRep (txb ^. votingProceduresTxBodyL)
        <*> toSpecRep (txb ^. proposalProceduresTxBodyL)
        <*> toSpecRep (txb ^. networkIdTxBodyL)
        <*> toSpecRep (txb ^. currentTreasuryValueTxBodyL)
        <*> pure 0
        <*> fmap (fmap (const 0)) (toSpecRep (txb ^. scriptIntegrityHashTxBodyL))
        <*> (Agda.MkHSSet <$> toSpecRep (txb ^. guardsTxBodyL))
        <*> (Agda.MkHSSet <$> traverse toSpecRepTuple (Map.toList $ txb ^. requiredTopLevelGuardsL))
        <*> toSpecRep (txb ^. directDepositsTxBodyL)
        <*> toSpecRep (txb ^. accountBalanceIntervalsTxBodyL)

instance SpecTranslate DijkstraEra (Tx TopTx DijkstraEra) where
  type SpecRep DijkstraEra (Tx TopTx DijkstraEra) = Agda.TxTop

  toSpecRep tx =
    Agda.MkTxTop
      <$> withCtxSpecTransM (txIdTx tx) (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. sizeTxF)
      <*> toSpecRep (tx ^. isValidTxL)
      <*> toSpecRep (tx ^. auxDataTxL)

instance SpecTranslate DijkstraEra (Tx SubTx DijkstraEra) where
  type SpecRep DijkstraEra (Tx SubTx DijkstraEra) = Agda.TxSub

  toSpecRep tx =
    Agda.MkTxSub
      <$> withCtxSpecTransM (txIdTx tx) (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. sizeTxF)
      <*> pure ()
      <*> toSpecRep (tx ^. auxDataTxL)

instance SpecTranslate DijkstraEra (DijkstraStAnnTx TopTx DijkstraEra) where
  type SpecRep DijkstraEra (DijkstraStAnnTx TopTx DijkstraEra) = Agda.TxTop

  toSpecRep stAnnTx = toSpecRep (stAnnTx ^. txStAnnTxG @DijkstraEra)
