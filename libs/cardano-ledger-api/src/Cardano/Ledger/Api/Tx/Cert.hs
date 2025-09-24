{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Api.Tx.Cert (
  EraTxCert (
    TxCert,
    TxCertUpgradeError,
    getRegPoolTxCert,
    getRetirePoolTxCert,
    lookupRegStakeTxCert,
    lookupUnRegStakeTxCert
  ),
  upgradeTxCert,
  getVKeyWitnessTxCert,
  getScriptWitnessTxCert,
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
  isRegStakeTxCert,
  isUnRegStakeTxCert,

  -- * Any Era
  AnyEraTxCert (..),
  pattern AnyEraRegPoolTxCert,
  pattern AnyEraRetirePoolTxCert,
  pattern AnyEraRegTxCert,
  pattern AnyEraUnRegTxCert,
  pattern AnyEraMirTxCert,
  pattern AnyEraGenesisDelegTxCert,
  pattern AnyEraRegDepositTxCert,
  pattern AnyEraUnRegDepositTxCert,
  pattern AnyEraDelegTxCert,
  pattern AnyEraRegDepositDelegTxCert,
  pattern AnyEraAuthCommitteeHotKeyTxCert,
  pattern AnyEraResignCommitteeColdTxCert,
  pattern AnyEraRegDRepTxCert,
  pattern AnyEraUnRegDRepTxCert,
  pattern AnyEraUpdateDRepTxCert,

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
  ShelleyEraTxCert (
    getRegTxCert,
    getUnRegTxCert,
    getDelegStakeTxCert,
    getGenesisDelegTxCert,
    getMirTxCert
  ),
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
  ConwayEraTxCert (
    getRegDepositTxCert,
    getUnRegDepositTxCert,
    getDelegTxCert,
    getRegDepositDelegTxCert,
    getAuthCommitteeHotKeyTxCert,
    getResignCommitteeColdTxCert,
    getRegDRepTxCert,
    getUpdateDRepTxCert
  ),
  getDelegateeTxCert,
  Delegatee (..),
  getStakePoolDelegatee,
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern AuthCommitteeHotKeyTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.BaseTypes (Anchor, EpochNo, StrictMaybe)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert (
    getAuthCommitteeHotKeyTxCert,
    getDelegTxCert,
    getRegDRepTxCert,
    getRegDepositDelegTxCert,
    getRegDepositTxCert,
    getResignCommitteeColdTxCert,
    getUnRegDRepTxCert,
    getUnRegDepositTxCert,
    getUpdateDRepTxCert
  ),
  Delegatee (..),
  getDelegateeTxCert,
  getStakePoolDelegatee,
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
  EraTxCert (
    TxCert,
    TxCertUpgradeError,
    getRegPoolTxCert,
    getRetirePoolTxCert,
    getScriptWitnessTxCert,
    getVKeyWitnessTxCert,
    lookupRegStakeTxCert,
    lookupUnRegStakeTxCert,
    upgradeTxCert
  ),
  KeyHash,
  KeyRole (..),
  KeyRoleVRF (GenDelegVRF),
  VRFVerKeyHash,
  isRegStakeTxCert,
  isUnRegStakeTxCert,
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.TxCert (
  GenesisDelegCert (..),
  MIRCert,
  ShelleyEraTxCert (
    getDelegStakeTxCert,
    getGenesisDelegTxCert,
    getMirTxCert,
    getRegTxCert,
    getUnRegTxCert
  ),
  pattern DelegStakeTxCert,
  pattern GenesisDelegTxCert,
  pattern MirTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
 )
import Cardano.Ledger.State (PoolParams)

class EraTxCert era => AnyEraTxCert era where
  anyEraToRegTxCert :: TxCert era -> Maybe (Credential 'Staking)
  anyEraToRegTxCert = const Nothing

  anyEraToUnRegTxCert :: TxCert era -> Maybe (Credential 'Staking)
  anyEraToUnRegTxCert = const Nothing

  anyEraToGenesisDelegTxCert :: TxCert era -> Maybe GenesisDelegCert
  anyEraToGenesisDelegTxCert = const Nothing

  anyEraToMirTxCert :: TxCert era -> Maybe MIRCert
  anyEraToMirTxCert = const Nothing

  anyEraToRegDepositTxCert :: TxCert era -> Maybe (Credential 'Staking, Coin)
  default anyEraToRegDepositTxCert ::
    ConwayEraTxCert era => TxCert era -> Maybe (Credential 'Staking, Coin)
  anyEraToRegDepositTxCert = getRegDepositTxCert

  anyEraToUnRegDepositTxCert :: TxCert era -> Maybe (Credential 'Staking, Coin)
  default anyEraToUnRegDepositTxCert ::
    ConwayEraTxCert era => TxCert era -> Maybe (Credential 'Staking, Coin)
  anyEraToUnRegDepositTxCert = getUnRegDepositTxCert

  anyEraToDelegTxCert :: TxCert era -> Maybe (Credential 'Staking, Delegatee)
  default anyEraToDelegTxCert ::
    ConwayEraTxCert era => TxCert era -> Maybe (Credential 'Staking, Delegatee)
  anyEraToDelegTxCert = getDelegTxCert

  anyEraToRegDepositDelegTxCert :: TxCert era -> Maybe (Credential 'Staking, Delegatee, Coin)
  default anyEraToRegDepositDelegTxCert ::
    ConwayEraTxCert era => TxCert era -> Maybe (Credential 'Staking, Delegatee, Coin)
  anyEraToRegDepositDelegTxCert = getRegDepositDelegTxCert

  anyEraToAuthCommitteeHotKeyTxCert ::
    TxCert era -> Maybe (Credential 'ColdCommitteeRole, Credential 'HotCommitteeRole)
  default anyEraToAuthCommitteeHotKeyTxCert ::
    ConwayEraTxCert era =>
    TxCert era ->
    Maybe (Credential 'ColdCommitteeRole, Credential 'HotCommitteeRole)
  anyEraToAuthCommitteeHotKeyTxCert = getAuthCommitteeHotKeyTxCert

  anyEraToResignCommitteeColdTxCert ::
    TxCert era -> Maybe (Credential 'ColdCommitteeRole, StrictMaybe Anchor)
  default anyEraToResignCommitteeColdTxCert ::
    ConwayEraTxCert era =>
    TxCert era ->
    Maybe (Credential 'ColdCommitteeRole, StrictMaybe Anchor)
  anyEraToResignCommitteeColdTxCert = getResignCommitteeColdTxCert

  anyEraToRegDRepTxCert ::
    TxCert era -> Maybe (Credential 'DRepRole, Coin, StrictMaybe Anchor)
  default anyEraToRegDRepTxCert ::
    ConwayEraTxCert era =>
    TxCert era ->
    Maybe (Credential 'DRepRole, Coin, StrictMaybe Anchor)
  anyEraToRegDRepTxCert = getRegDRepTxCert

  anyEraToUnRegDRepTxCert :: TxCert era -> Maybe (Credential 'DRepRole, Coin)
  default anyEraToUnRegDRepTxCert ::
    ConwayEraTxCert era => TxCert era -> Maybe (Credential 'DRepRole, Coin)
  anyEraToUnRegDRepTxCert = getUnRegDRepTxCert

  anyEraToUpdateDRepTxCert ::
    TxCert era -> Maybe (Credential 'DRepRole, StrictMaybe Anchor)
  default anyEraToUpdateDRepTxCert ::
    ConwayEraTxCert era =>
    TxCert era ->
    Maybe (Credential 'DRepRole, StrictMaybe Anchor)
  anyEraToUpdateDRepTxCert = getUpdateDRepTxCert

instance AnyEraTxCert ShelleyEra where
  anyEraToRegTxCert = getRegTxCert
  anyEraToUnRegTxCert = getUnRegTxCert
  anyEraToGenesisDelegTxCert = getGenesisDelegTxCert
  anyEraToMirTxCert = getMirTxCert
  anyEraToRegDepositTxCert = const Nothing
  anyEraToUnRegDepositTxCert = const Nothing
  anyEraToDelegTxCert = const Nothing
  anyEraToRegDepositDelegTxCert = const Nothing
  anyEraToAuthCommitteeHotKeyTxCert = const Nothing
  anyEraToResignCommitteeColdTxCert = const Nothing
  anyEraToRegDRepTxCert = const Nothing
  anyEraToUnRegDRepTxCert = const Nothing
  anyEraToUpdateDRepTxCert = const Nothing

instance AnyEraTxCert AllegraEra where
  anyEraToRegTxCert = getRegTxCert
  anyEraToUnRegTxCert = getUnRegTxCert
  anyEraToGenesisDelegTxCert = getGenesisDelegTxCert
  anyEraToMirTxCert = getMirTxCert
  anyEraToRegDepositTxCert = const Nothing
  anyEraToUnRegDepositTxCert = const Nothing
  anyEraToDelegTxCert = const Nothing
  anyEraToRegDepositDelegTxCert = const Nothing
  anyEraToAuthCommitteeHotKeyTxCert = const Nothing
  anyEraToResignCommitteeColdTxCert = const Nothing
  anyEraToRegDRepTxCert = const Nothing
  anyEraToUnRegDRepTxCert = const Nothing
  anyEraToUpdateDRepTxCert = const Nothing

instance AnyEraTxCert MaryEra where
  anyEraToRegTxCert = getRegTxCert
  anyEraToUnRegTxCert = getUnRegTxCert
  anyEraToGenesisDelegTxCert = getGenesisDelegTxCert
  anyEraToMirTxCert = getMirTxCert
  anyEraToRegDepositTxCert = const Nothing
  anyEraToUnRegDepositTxCert = const Nothing
  anyEraToDelegTxCert = const Nothing
  anyEraToRegDepositDelegTxCert = const Nothing
  anyEraToAuthCommitteeHotKeyTxCert = const Nothing
  anyEraToResignCommitteeColdTxCert = const Nothing
  anyEraToRegDRepTxCert = const Nothing
  anyEraToUnRegDRepTxCert = const Nothing
  anyEraToUpdateDRepTxCert = const Nothing

instance AnyEraTxCert AlonzoEra where
  anyEraToRegTxCert = getRegTxCert
  anyEraToUnRegTxCert = getUnRegTxCert
  anyEraToGenesisDelegTxCert = getGenesisDelegTxCert
  anyEraToMirTxCert = getMirTxCert
  anyEraToRegDepositTxCert = const Nothing
  anyEraToUnRegDepositTxCert = const Nothing
  anyEraToDelegTxCert = const Nothing
  anyEraToRegDepositDelegTxCert = const Nothing
  anyEraToAuthCommitteeHotKeyTxCert = const Nothing
  anyEraToResignCommitteeColdTxCert = const Nothing
  anyEraToRegDRepTxCert = const Nothing
  anyEraToUnRegDRepTxCert = const Nothing
  anyEraToUpdateDRepTxCert = const Nothing

instance AnyEraTxCert BabbageEra where
  anyEraToRegTxCert = getRegTxCert
  anyEraToUnRegTxCert = getUnRegTxCert
  anyEraToGenesisDelegTxCert = getGenesisDelegTxCert
  anyEraToMirTxCert = getMirTxCert
  anyEraToRegDepositTxCert = const Nothing
  anyEraToUnRegDepositTxCert = const Nothing
  anyEraToDelegTxCert = const Nothing
  anyEraToRegDepositDelegTxCert = const Nothing
  anyEraToAuthCommitteeHotKeyTxCert = const Nothing
  anyEraToResignCommitteeColdTxCert = const Nothing
  anyEraToRegDRepTxCert = const Nothing
  anyEraToUnRegDRepTxCert = const Nothing
  anyEraToUpdateDRepTxCert = const Nothing

instance AnyEraTxCert ConwayEra where
  anyEraToRegTxCert = getRegTxCert
  anyEraToUnRegTxCert = getUnRegTxCert

instance AnyEraTxCert DijkstraEra

pattern AnyEraRegPoolTxCert :: EraTxCert era => PoolParams -> TxCert era
pattern AnyEraRegPoolTxCert poolParams = RegPoolTxCert poolParams

pattern AnyEraRetirePoolTxCert ::
  EraTxCert era => KeyHash 'StakePool -> EpochNo -> TxCert era
pattern AnyEraRetirePoolTxCert keyHash epochNo = RetirePoolTxCert keyHash epochNo

pattern AnyEraRegTxCert :: AnyEraTxCert era => Credential 'Staking -> TxCert era
pattern AnyEraRegTxCert c <- (anyEraToRegTxCert -> Just c)

pattern AnyEraUnRegTxCert :: AnyEraTxCert era => Credential 'Staking -> TxCert era
pattern AnyEraUnRegTxCert c <- (anyEraToUnRegTxCert -> Just c)

pattern AnyEraMirTxCert :: AnyEraTxCert era => MIRCert -> TxCert era
pattern AnyEraMirTxCert d <- (anyEraToMirTxCert -> Just d)

pattern AnyEraGenesisDelegTxCert ::
  AnyEraTxCert era =>
  KeyHash 'Genesis ->
  KeyHash 'GenesisDelegate ->
  VRFVerKeyHash 'GenDelegVRF ->
  TxCert era
pattern AnyEraGenesisDelegTxCert genKey genDelegKey vrfKeyHash <-
  (anyEraToGenesisDelegTxCert -> Just (GenesisDelegCert genKey genDelegKey vrfKeyHash))

pattern AnyEraRegDepositTxCert ::
  AnyEraTxCert era =>
  Credential 'Staking ->
  Coin ->
  TxCert era
pattern AnyEraRegDepositTxCert cred c <- (anyEraToRegDepositTxCert -> Just (cred, c))

pattern AnyEraUnRegDepositTxCert ::
  AnyEraTxCert era =>
  Credential 'Staking ->
  Coin ->
  TxCert era
pattern AnyEraUnRegDepositTxCert cred c <- (anyEraToUnRegDepositTxCert -> Just (cred, c))

pattern AnyEraDelegTxCert ::
  AnyEraTxCert era =>
  Credential 'Staking ->
  Delegatee ->
  TxCert era
pattern AnyEraDelegTxCert cred d <- (anyEraToDelegTxCert -> Just (cred, d))

pattern AnyEraRegDepositDelegTxCert ::
  AnyEraTxCert era =>
  Credential 'Staking ->
  Delegatee ->
  Coin ->
  TxCert era
pattern AnyEraRegDepositDelegTxCert cred d c <- (anyEraToRegDepositDelegTxCert -> Just (cred, d, c))

pattern AnyEraAuthCommitteeHotKeyTxCert ::
  AnyEraTxCert era =>
  Credential 'ColdCommitteeRole ->
  Credential 'HotCommitteeRole ->
  TxCert era
pattern AnyEraAuthCommitteeHotKeyTxCert ck hk <- (anyEraToAuthCommitteeHotKeyTxCert -> Just (ck, hk))

pattern AnyEraResignCommitteeColdTxCert ::
  AnyEraTxCert era =>
  Credential 'ColdCommitteeRole ->
  StrictMaybe Anchor ->
  TxCert era
pattern AnyEraResignCommitteeColdTxCert ck a <- (anyEraToResignCommitteeColdTxCert -> Just (ck, a))

pattern AnyEraRegDRepTxCert ::
  AnyEraTxCert era =>
  Credential 'DRepRole ->
  Coin ->
  StrictMaybe Anchor ->
  TxCert era
pattern AnyEraRegDRepTxCert cred deposit mAnchor <- (anyEraToRegDRepTxCert -> Just (cred, deposit, mAnchor))

pattern AnyEraUnRegDRepTxCert ::
  AnyEraTxCert era =>
  Credential 'DRepRole ->
  Coin ->
  TxCert era
pattern AnyEraUnRegDRepTxCert cred deposit <- (anyEraToUnRegDRepTxCert -> Just (cred, deposit))

pattern AnyEraUpdateDRepTxCert ::
  AnyEraTxCert era =>
  Credential 'DRepRole ->
  StrictMaybe Anchor ->
  TxCert era
pattern AnyEraUpdateDRepTxCert cred mAnchor <- (anyEraToUpdateDRepTxCert -> Just (cred, mAnchor))

{-# COMPLETE
  AnyEraRegPoolTxCert
  , AnyEraRetirePoolTxCert
  , AnyEraRegTxCert
  , AnyEraUnRegTxCert
  , AnyEraRegDepositTxCert
  , AnyEraUnRegDepositTxCert
  , AnyEraDelegTxCert
  , AnyEraRegDepositDelegTxCert
  , AnyEraAuthCommitteeHotKeyTxCert
  , AnyEraResignCommitteeColdTxCert
  , AnyEraRegDRepTxCert
  , AnyEraUnRegDRepTxCert
  , AnyEraUpdateDRepTxCert
  #-}
