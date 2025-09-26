{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Api.Tx.Cert (
  EraTxCert (TxCert, TxCertUpgradeError),
  upgradeTxCert,
  getVKeyWitnessTxCert,
  getScriptWitnessTxCert,
#if __GLASGOW_HASKELL__ >= 914
  data RegPoolTxCert,
  data RetirePoolTxCert,
#else
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
#endif
  lookupRegStakeTxCert,
  lookupUnRegStakeTxCert,
  isRegStakeTxCert,
  isUnRegStakeTxCert,

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
#if __GLASGOW_HASKELL__ >= 914
  data MirTxCert,
  data GenesisDelegTxCert,
  data RegTxCert,
  data UnRegTxCert,
  data DelegStakeTxCert,
#else
  pattern MirTxCert,
  pattern GenesisDelegTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
  pattern DelegStakeTxCert,
#endif
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
  getDelegateeTxCert,
  Delegatee (..),
  getStakePoolDelegatee,
#if __GLASGOW_HASKELL__ >= 914
  data RegDepositTxCert,
  data UnRegDepositTxCert,
  data DelegTxCert,
  data RegDepositDelegTxCert,
  data AuthCommitteeHotKeyTxCert,
  data ResignCommitteeColdTxCert,
  data RegDRepTxCert,
  data UnRegDRepTxCert,
#else
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern AuthCommitteeHotKeyTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
#endif
) where

import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert,
  Delegatee (..),
  getDelegateeTxCert,
  getStakePoolDelegatee,
#if __GLASGOW_HASKELL__ >= 914
  data AuthCommitteeHotKeyTxCert,
  data DelegTxCert,
  data RegDRepTxCert,
  data RegDepositDelegTxCert,
  data RegDepositTxCert,
  data ResignCommitteeColdTxCert,
  data UnRegDRepTxCert,
  data UnRegDepositTxCert,
#else
  pattern AuthCommitteeHotKeyTxCert,
  pattern DelegTxCert,
  pattern RegDRepTxCert,
  pattern RegDepositDelegTxCert,
  pattern RegDepositTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern UnRegDRepTxCert,
  pattern UnRegDepositTxCert,
#endif
 )
import Cardano.Ledger.Core (
  EraTxCert (
    TxCert,
    TxCertUpgradeError,
    getScriptWitnessTxCert,
    getVKeyWitnessTxCert,
    lookupRegStakeTxCert,
    lookupUnRegStakeTxCert,
    upgradeTxCert
  ),
  isRegStakeTxCert,
  isUnRegStakeTxCert,
#if __GLASGOW_HASKELL__ >= 914
  data RegPoolTxCert,
  data RetirePoolTxCert,
#else
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
#endif
 )
import Cardano.Ledger.Shelley.TxCert (
  ShelleyEraTxCert,
#if __GLASGOW_HASKELL__ >= 914
  data DelegStakeTxCert,
  data GenesisDelegTxCert,
  data MirTxCert,
  data RegTxCert,
  data UnRegTxCert,
#else
  pattern DelegStakeTxCert,
  pattern GenesisDelegTxCert,
  pattern MirTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
#endif
 )
