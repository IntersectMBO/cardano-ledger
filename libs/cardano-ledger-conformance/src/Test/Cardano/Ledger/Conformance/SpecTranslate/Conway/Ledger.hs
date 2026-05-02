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
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (StrictMaybe ScriptHash)
  , Inject ctx (EnactState era)
  ) =>
  SpecTranslate ctx ConwayEra (LedgerEnv era)
  where
  type SpecRep ConwayEra (LedgerEnv era) = Agda.LEnv

  toSpecRep LedgerEnv {..} = do
    policyHash <- askCtx @(StrictMaybe ScriptHash)
    enactState <- askCtx @(EnactState era)
    Agda.MkLEnv
      <$> toSpecRep @_ @ConwayEra ledgerSlotNo
      <*> toSpecRep @_ @ConwayEra policyHash
      <*> toSpecRep @_ @ConwayEra ledgerPp
      <*> toSpecRep @_ @ConwayEra enactState
      <*> toSpecRep @_ @ConwayEra (casTreasury ledgerAccount)

instance
  Inject ctx TxId =>
  SpecTranslate ctx ConwayEra (TxBody TopTx ConwayEra)
  where
  type SpecRep ConwayEra (TxBody TopTx ConwayEra) = Agda.TxBody

  toSpecRep txb = do
    txId <- askCtx @TxId
    Agda.MkTxBody
      <$> toSpecRep @_ @ConwayEra (txb ^. inputsTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. referenceInputsTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. collateralInputsTxBodyL)
      <*> (Agda.MkHSMap . zip [0 ..] <$> toSpecRep @_ @ConwayEra (txb ^. outputsTxBodyL))
      <*> toSpecRep @_ @ConwayEra txId
      <*> toSpecRep @_ @ConwayEra (txb ^. certsTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. feeTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. withdrawalsTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. vldtTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. auxDataHashTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. treasuryDonationTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. votingProceduresTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. proposalProceduresTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. networkIdTxBodyL)
      <*> toSpecRep @_ @ConwayEra (txb ^. currentTreasuryValueTxBodyL)
      <*> pure 0
      <*> toSpecRep @_ @ConwayEra (txb ^. reqSignerHashesTxBodyL)
      -- The script integrity hash is computed using @const 0@ on the Agda
      -- side (@Hashable-ScriptIntegrity = record { hash = λ x → 0 }@).
      -- Until a proper hash function is used in Agda, we emulate the same
      -- behavior here.
      --
      -- The following PR documents the discrepancy on the Agda side:
      -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1086
      <*> fmap (fmap (const 0)) (toSpecRep @_ @ConwayEra (txb ^. scriptIntegrityHashTxBodyL))

instance SpecTranslate ctx ConwayEra (Tx TopTx ConwayEra) where
  type SpecRep ConwayEra (Tx TopTx ConwayEra) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtx (txIdTx tx) (toSpecRep @_ @ConwayEra (tx ^. bodyTxL))
      <*> toSpecRep @_ @ConwayEra (tx ^. witsTxL)
      <*> toSpecRep @_ @ConwayEra (tx ^. sizeTxF)
      <*> toSpecRep @_ @ConwayEra (tx ^. isValidTxL)
      <*> toSpecRep @_ @ConwayEra (tx ^. auxDataTxL)
