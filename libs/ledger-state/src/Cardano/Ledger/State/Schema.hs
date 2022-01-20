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

module Cardano.Ledger.State.Schema where

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Credential as Credential
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.PoolRank as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.State.Orphans (Enc, SnapShotType (..))
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.TxIn as TxIn
import qualified Data.Map.Strict as Map
import Data.Word
import Database.Persist.Sqlite
import Database.Persist.TH

type FGenDelegs = (Enc (Map.Map (Shelley.FutureGenDeleg C) (Keys.GenDelegPair C)))

type CredentialWitness = Credential.Credential 'Keys.Witness C

type KeyHashWitness = Keys.KeyHash 'Keys.Witness C

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
EpochState
  treasury Coin
  reserves Coin
  prevPp (Alonzo.PParams CurrentEra)
  pp (Alonzo.PParams CurrentEra)
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
  inIx Word64
  inId (TxIn.TxId C)
  out (Alonzo.TxOut CurrentEra)
  UniqueTx inIx inId
Txs
  inIx Word64
  inId (TxIn.TxId C)
  out (Alonzo.TxOut CurrentEra)
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
