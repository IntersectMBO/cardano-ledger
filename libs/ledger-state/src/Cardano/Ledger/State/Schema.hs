{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

module Cardano.Ledger.State.Schema where

import Cardano.Ledger.Alonzo.TxBody as Alonzo (AlonzoTxOut)
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Credential as Credential
import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.PParams
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.PoolRank as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.State.Orphans (Enc, SnapShotType (..))
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.TxIn as TxIn
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Database.Persist.Sqlite
import Database.Persist.TH

type FGenDelegs = (Enc (Map.Map (Shelley.FutureGenDeleg C) (Keys.GenDelegPair C)))

type CredentialWitness = Credential.Credential 'Keys.Witness C

type KeyHashWitness = Keys.KeyHash 'Keys.Witness C

deriving newtype instance
  PersistField (PParamsHKD Identity era) => PersistField (PParams era)

deriving newtype instance
  PersistFieldSql (PParamsHKD Identity era) => PersistFieldSql (PParams era)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
EpochState
  treasury Coin
  reserves Coin
  prevPp (PParams CurrentEra)
  pp (PParams CurrentEra)
  nonMyopic (Shelley.NonMyopic C)
  snapShotsFee Coin

SnapShot
  type SnapShotType
  epochStateId EpochStateId
  -- UniqueSnapShot type epochStateId
SnapShotStake
  snapShotId SnapShotId
  credentialId CredentialId
  coin (CompactForm Coin)
  UniqueSnapShotStake snapShotId credentialId
SnapShotDelegation
  snapShotId SnapShotId
  credentialId CredentialId
  keyHash KeyHashId
  UniqueSnapShotDelegation snapShotId credentialId
SnapShotPool
  snapShotId SnapShotId
  keyHashId KeyHashId
  params (Shelley.PoolParams C)
  UniqueSnapShotPool snapShotId keyHashId

LedgerState
  utxoId UtxoStateId
  dstateId DStateId
  epochStateId EpochStateId
  pstateBin (Shelley.PState C)
  UniqueLedgerStateUtxoId utxoId
  UniqueLedgerStateDStateId dstateId
  UniqueLedgerStateEpochStateId epochStateId
UtxoState
  deposited Coin
  fees Coin
  ppups (Shelley.PPUPState CurrentEra)
DState
  fGenDelegs FGenDelegs
  genDelegs (Keys.GenDelegs C)
  irDeltaReserves DeltaCoin
  irDeltaTreasury DeltaCoin

Credential
  witness CredentialWitness
  UniqueCredential witness
KeyHash
  witness KeyHashWitness
  UniqueKeyHash witness
Tx
  inIx TxIx
  inId (TxIn.TxId C)
  out (AlonzoTxOut CurrentEra)
  UniqueTx inIx inId
Txs
  inIx TxIx
  inId (TxIn.TxId C)
  out (AlonzoTxOut CurrentEra)
  stakeCredential CredentialId Maybe
  UniqueTxs inIx inId
UtxoEntry
  txId TxId
  txsId TxsId
  stateId UtxoStateId
Reward
  dstateId DStateId
  credentialId CredentialId
  coin Coin
  UniqueReward dstateId credentialId coin
Delegation
  dstateId DStateId
  credentialId CredentialId
  stakePoolId KeyHashId
  UniqueDelegation dstateId credentialId
Ptr
  dstateId DStateId
  credentialId CredentialId
  ptr Credential.Ptr
  UniquePtrPtr dstateId ptr
  UniquePtrCredential dstateId credentialId
IRReserves
  dstateId DStateId
  credentialId CredentialId
  coin Coin
  UniqueIRReserves dstateId credentialId
IRTreasury
  dstateId DStateId
  credentialId CredentialId
  coin Coin
  UniqueIRTreasury dstateId credentialId
|]
