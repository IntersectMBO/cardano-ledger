{-# LANGUAGE PatternSynonyms #-}

module Cardano.Ledger.Api.Tx.Cert (
  EraTxCert,
  TxCert,
  getVKeyWitnessTxCert,
  getScriptWitnessTxCert,
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,

  -- * Shelley Era

  -- | Complete set of patterns for Shelley through Babbage `TxCert`:
  --
  -- @
  -- `TxCert` =
  --   `RegPoolTxCert`
  --   `RetirePoolTxCert`
  --   `RegTxCert`
  --   `UnRegTxCert`
  --   `DelegStakeTxCert`
  --   `MirTxCert`
  --   `GenesisDelegTxCert`
  -- @
  ShelleyEraTxCert,
  pattern MirTxCert,
  pattern GenesisDelegTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
  pattern DelegStakeTxCert,

  -- * Conway Era

  -- | Complete set of patterns for Conway `TxCert`:
  --
  -- @
  -- `TxCert` =
  --   `RegPoolTxCert`
  --   `RetirePoolTxCert`
  --   `RegTxCert`
  --   `UnRegTxCert`
  --   `RegDepositTxCert`
  --   `UnRegDepositTxCert`
  --   `DelegTxCert`
  --   `RegDepositDelegTxCert`
  --   `AuthCommitteeHotKeyTxCert`
  --   `ResignCommitteeColdTxCert`
  --   `RegDRepTxCert`
  --   `UnRegDRepTxCert`
  -- @
  ConwayEraTxCert,
  Delegatee (..),
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern AuthCommitteeHotKeyTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
)
where

import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert,
  Delegatee (..),
  pattern AuthCommitteeHotKeyTxCert,
  pattern DelegTxCert,
  pattern RegDRepTxCert,
  pattern RegDepositDelegTxCert,
  pattern RegDepositTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern UnRegDRepTxCert,
  pattern UnRegDepositTxCert,
 )
import Cardano.Ledger.Core (
  EraTxCert,
  TxCert,
  getScriptWitnessTxCert,
  getVKeyWitnessTxCert,
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
 )
import Cardano.Ledger.Shelley.TxCert (
  ShelleyEraTxCert,
  pattern DelegStakeTxCert,
  pattern GenesisDelegTxCert,
  pattern MirTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
 )
