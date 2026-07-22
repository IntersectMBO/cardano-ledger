{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Ledger () where

import Cardano.Ledger.Alonzo.Tx (AlonzoStAnnTx)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  AllegraEraTxBody (..),
  AlonzoEraTx (..),
  AlonzoEraTxBody (..),
  BabbageEraTxBody (..),
  ConwayEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  ScriptHash,
  TxLevel (..),
  txIdTx,
  txStAnnTxG,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (askSpecTransM, withCtxSpecTransM)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  SpecTranslate (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance SpecTranslate ConwayEra (Shelley.LedgerEnv ConwayEra) where
  type SpecRep ConwayEra (Shelley.LedgerEnv ConwayEra) = Agda.LEnv
  type
    SpecContext ConwayEra (Shelley.LedgerEnv ConwayEra) =
      (StrictMaybe ScriptHash, Conway.EnactState ConwayEra)

  toSpecRep Shelley.LedgerEnv {..} = do
    (policyHash, enactState) <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkLEnv
        <$> toSpecRep ledgerSlotNo
        <*> toSpecRep policyHash
        <*> toSpecRep ledgerPp
        <*> toSpecRep enactState
        <*> toSpecRep (casTreasury ledgerAccount)

instance SpecTranslate ConwayEra (TxBody TopTx ConwayEra) where
  type SpecRep ConwayEra (TxBody TopTx ConwayEra) = Agda.TxBody
  type SpecContext ConwayEra (TxBody TopTx ConwayEra) = TxId

  toSpecRep txb = do
    txId <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkTxBody
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
        <*> toSpecRep (txb ^. reqSignerHashesTxBodyL)
        -- The script integrity hash is computed using @const 0@ on the Agda
        -- side (@Hashable-ScriptIntegrity = record { hash = λ x → 0 }@).
        -- Until a proper hash function is used in Agda, we emulate the same
        -- behavior here.
        --
        -- The following PR documents the discrepancy on the Agda side:
        -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1086
        <*> fmap (fmap (const 0)) (toSpecRep (txb ^. scriptIntegrityHashTxBodyL))

instance SpecTranslate ConwayEra (Tx TopTx ConwayEra) where
  type SpecRep ConwayEra (Tx TopTx ConwayEra) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtxSpecTransM (txIdTx tx) (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. sizeTxF)
      <*> toSpecRep (tx ^. isPhase2ValidTxL)
      <*> toSpecRep (tx ^. auxDataTxL)

instance SpecTranslate ConwayEra (AlonzoStAnnTx TopTx ConwayEra) where
  type SpecRep ConwayEra (AlonzoStAnnTx TopTx ConwayEra) = Agda.Tx

  toSpecRep stAnnTx = toSpecRep (stAnnTx ^. txStAnnTxG)
