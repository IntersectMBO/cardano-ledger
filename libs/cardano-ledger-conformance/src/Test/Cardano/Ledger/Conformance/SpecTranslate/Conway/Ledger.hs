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

import Cardano.Ledger.BaseTypes (Inject)
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
 )
import Cardano.Ledger.Conway.Rules (EnactState)
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (askCtx, withCtx)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  SpecTranslate (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (StrictMaybe ScriptHash)
  , Inject ctx (EnactState era)
  ) =>
  SpecTranslate ctx (LedgerEnv era)
  where
  type SpecRep (LedgerEnv era) = Agda.LEnv

  toSpecRep LedgerEnv {..} = do
    policyHash <- askCtx @(StrictMaybe ScriptHash)
    enactState <- askCtx @(EnactState era)
    Agda.MkLEnv
      <$> toSpecRep ledgerSlotNo
      <*> toSpecRep policyHash
      <*> toSpecRep ledgerPp
      <*> toSpecRep enactState
      <*> toSpecRep (casTreasury ledgerAccount)

instance
  Inject ctx TxId =>
  SpecTranslate ctx (TxBody TopTx ConwayEra)
  where
  type SpecRep (TxBody TopTx ConwayEra) = Agda.TxBody

  toSpecRep txb = do
    txId <- askCtx @TxId
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
      <*> toSpecRep (txb ^. scriptIntegrityHashTxBodyL)

instance SpecTranslate ctx (Tx TopTx ConwayEra) where
  type SpecRep (Tx TopTx ConwayEra) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtx (txIdTx tx) (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. sizeTxF)
      <*> toSpecRep (tx ^. isValidTxL)
      <*> toSpecRep (tx ^. auxDataTxL)
