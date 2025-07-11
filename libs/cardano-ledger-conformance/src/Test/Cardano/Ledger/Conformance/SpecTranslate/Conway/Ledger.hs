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
  EraRule,
  EraTx (..),
  EraTxBody (..),
  ScriptHash,
  txIdTx,
 )
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure, EnactState)
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId)
import Control.State.Transition.Extended (STS (..))
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  OpaqueErrorString (..),
  askCtx,
  showOpaqueErrorString,
  withCtx,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  ConwayTxBodyTransContext (..),
  SpecTranslate (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..))

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
  ( ToExpr (PredicateFailure (EraRule "GOV" era))
  , ToExpr (PredicateFailure (EraRule "CERTS" era))
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  ) =>
  SpecTranslate ctx (ConwayLedgerPredFailure era)
  where
  type SpecRep (ConwayLedgerPredFailure era) = OpaqueErrorString

  toSpecRep = pure . showOpaqueErrorString

instance
  Inject ctx TxId =>
  SpecTranslate ctx (TxBody ConwayEra)
  where
  type SpecRep (TxBody ConwayEra) = Agda.TxBody

  toSpecRep txb = do
    txId <- askCtx @TxId
    Agda.MkTxBody
      <$> toSpecRep (txb ^. inputsTxBodyL)
      <*> toSpecRep (txb ^. referenceInputsTxBodyL)
      <*> (Agda.MkHSMap . zip [0 ..] <$> toSpecRep (txb ^. outputsTxBodyL))
      <*> toSpecRep (txb ^. feeTxBodyL)
      <*> pure 0
      <*> toSpecRep (txb ^. vldtTxBodyL)
      <*> toSpecRep (txb ^. certsTxBodyL)
      <*> toSpecRep (txb ^. withdrawalsTxBodyL)
      <*> toSpecRep (txb ^. votingProceduresTxBodyL)
      <*> toSpecRep (txb ^. proposalProceduresTxBodyL)
      <*> toSpecRep (txb ^. treasuryDonationTxBodyL)
      <*> pure Nothing -- TODO implement this properly
      <*> toSpecRep (txb ^. auxDataHashTxBodyL)
      <*> toSpecRep (txb ^. networkIdTxBodyL)
      <*> toSpecRep (txb ^. currentTreasuryValueTxBodyL)
      <*> toSpecRep txId
      <*> toSpecRep (txb ^. collateralInputsTxBodyL)
      <*> toSpecRep (txb ^. reqSignerHashesTxBodyL)
      <*> toSpecRep (txb ^. scriptIntegrityHashTxBodyL)

instance SpecTranslate ctx (Tx ConwayEra) where
  type SpecRep (Tx ConwayEra) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtx
        (ConwayTxBodyTransContext (txIdTx tx))
        (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. sizeTxF)
      <*> toSpecRep (tx ^. isValidTxL)
      <*> toSpecRep (tx ^. auxDataTxL)
