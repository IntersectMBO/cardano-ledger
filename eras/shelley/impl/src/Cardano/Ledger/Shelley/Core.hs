{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Core (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  ShelleyEraTxCert (..),
#if __GLASGOW_HASKELL__ >= 914
  data MirTxCert,
#else
  pattern MirTxCert,
#endif

  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
#if __GLASGOW_HASKELL__ >= 914
  data GenesisDelegTxCert,
  data RegTxCert,
  data UnRegTxCert,
  data DelegStakeTxCert,
#else
  pattern GenesisDelegTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
  pattern DelegStakeTxCert,
#endif
  module Cardano.Ledger.Core,
  module Cardano.Ledger.Shelley.Governance,
) where

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Core (
  EraPParams (
    DowngradePParams,
    PParamsHKD,
    UpgradePParams,
    applyPPUpdates,
    ppDG,
    ppProtocolVersionL,
    ppuProtocolVersionL
  ),
 )
import Cardano.Ledger.Core hiding (EraPParams (..))
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.Tx ()
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.TxCert (
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  ShelleyEraTxCert (..),
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
