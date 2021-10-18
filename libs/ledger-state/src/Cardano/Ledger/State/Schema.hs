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

import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.State.Orphans (Enc)
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.TxIn as TxIn
import Data.Word
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Map.Strict as Map
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Credential as Credential

type FGenDelegs = (Enc (Map.Map (Shelley.FutureGenDeleg C) (Keys.GenDelegPair C)))

type CredentialWitness = Credential.Credential 'Keys.Witness C
type KeyHashWitness = Keys.KeyHash 'Keys.Witness C

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
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
UtxoEntry
  tx TxId
  state UtxoStateId
UtxoState
  deposited Coin
  fees Coin
  ppups (Shelley.PPUPState CurrentEra)
LedgerState
  utxo UtxoStateId
  dstate DStateId
  pstateBin (Shelley.PState C)
DState
  fGenDelegs FGenDelegs
  genDelegs (Keys.GenDelegs C)
  irDeltaReserves DeltaCoin
  irDeltaTreasury DeltaCoin
Reward
  dstate DStateId
  credential CredentialId
  coin Coin
  UniqueReward dstate credential coin
Delegation
  dstate DStateId
  credential CredentialId
  stakePool KeyHashId
  UniqueDelegation dstate credential
Ptr
  dstate DStateId
  credential CredentialId
  ptr Credential.Ptr
  UniquePtrPtr dstate ptr
  UniquePtrCredential dstate credential
IRReserves
  dstate DStateId
  credential CredentialId
  coin Coin
  UniqueIRReserves dstate credential
IRTreasury
  dstate DStateId
  credential CredentialId
  coin Coin
  UniqueIRTreasury dstate credential
|]
