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
  EraPParams (..),
  EraTx (..),
  EraTxBody (..),
  ScriptHash,
  TxLevel (..),
  txIdTx,
  txStAnnTxG,
 )
import Cardano.Ledger.Conway.Rules (EnactState)
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (askSpecTransM, withCtx, withSpecTransM)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  SpecTranslate (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate (LedgerEnv era)
  where
  type SpecRep (LedgerEnv era) = Agda.LEnv
  type SpecContext (LedgerEnv era) = (StrictMaybe ScriptHash, EnactState era)

  toSpecRep LedgerEnv {..} = do
    (policyHash, enactState) <- askSpecTransM
    withSpecTransM (const ()) $
      Agda.MkLEnv
        <$> toSpecRep ledgerSlotNo
        <*> toSpecRep policyHash
        <*> toSpecRep ledgerPp
        <*> toSpecRep enactState
        <*> toSpecRep (casTreasury ledgerAccount)

instance SpecTranslate (TxBody TopTx ConwayEra) where
  type SpecRep (TxBody TopTx ConwayEra) = Agda.TxBody
  type SpecContext (TxBody TopTx ConwayEra) = TxId

  toSpecRep txb = do
    txId <- askSpecTransM
    withSpecTransM (const ()) $
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

instance SpecTranslate (Tx TopTx ConwayEra) where
  type SpecRep (Tx TopTx ConwayEra) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtx (txIdTx tx) (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. sizeTxF)
      <*> toSpecRep (tx ^. isValidTxL)
      <*> toSpecRep (tx ^. auxDataTxL)

instance SpecTranslate (AlonzoStAnnTx TopTx ConwayEra) where
  type SpecRep (AlonzoStAnnTx TopTx ConwayEra) = Agda.Tx

  toSpecRep stAnnTx = toSpecRep (stAnnTx ^. txStAnnTxG)
