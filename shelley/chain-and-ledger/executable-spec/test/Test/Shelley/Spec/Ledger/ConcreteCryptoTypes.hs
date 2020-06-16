{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.ConcreteCryptoTypes where

import Cardano.Crypto.DSIGN (MockDSIGN, VerKeyDSIGN)
import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Crypto.KES (MockKES)
import Data.Map (Map)
import qualified Shelley.Spec.Ledger.Address as TxData
import qualified Shelley.Spec.Ledger.BlockChain as BlockChain
import qualified Shelley.Spec.Ledger.Credential as TxData
import Shelley.Spec.Ledger.Crypto
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Delegation.Certificates
import qualified Shelley.Spec.Ledger.EpochBoundary as EpochBoundary
import qualified Shelley.Spec.Ledger.Keys as Keys
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.OCert as OCert
import qualified Shelley.Spec.Ledger.PParams as PParams
import qualified Shelley.Spec.Ledger.Rewards as Rewards
import qualified Shelley.Spec.Ledger.STS.Chain as STS.Chain
import qualified Shelley.Spec.Ledger.STS.Deleg as STS.Deleg
import qualified Shelley.Spec.Ledger.STS.Delegs as STS.Delegs
import qualified Shelley.Spec.Ledger.STS.Delpl as STS.Delpl
import qualified Shelley.Spec.Ledger.STS.Ledger as STS.Ledger
import qualified Shelley.Spec.Ledger.STS.Ledgers as STS.Ledgers
import qualified Shelley.Spec.Ledger.STS.NewEpoch as STS.NewEpoch
import qualified Shelley.Spec.Ledger.STS.Ocert as STS.Ocert
import qualified Shelley.Spec.Ledger.STS.Pool as STS.Pool
import qualified Shelley.Spec.Ledger.STS.PoolReap as STS.PoolReap
import qualified Shelley.Spec.Ledger.STS.Ppup as STS.Ppup
import qualified Shelley.Spec.Ledger.STS.Tick as STS.Tick
import qualified Shelley.Spec.Ledger.STS.Utxo as STS.Utxo
import qualified Shelley.Spec.Ledger.STS.Utxow as STS.Utxow
import qualified Shelley.Spec.Ledger.Scripts as Scripts
import qualified Shelley.Spec.Ledger.Tx as Tx
import qualified Shelley.Spec.Ledger.TxData as TxData
import qualified Shelley.Spec.Ledger.UTxO as UTxO
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)

data ConcreteCrypto (h :: *)

instance HashAlgorithm h => Crypto (ConcreteCrypto h) where
  type HASH (ConcreteCrypto h) = h
  type ADDRHASH (ConcreteCrypto h) = h
  type DSIGN (ConcreteCrypto h) = MockDSIGN
  type KES (ConcreteCrypto h) = MockKES 10
  type VRF (ConcreteCrypto h) = FakeVRF

type DCert h = Delegation.Certificates.DCert (ConcreteCrypto h)

type PoolDistr h = Delegation.Certificates.PoolDistr (ConcreteCrypto h)

type Delegation h = TxData.Delegation (ConcreteCrypto h)

type PoolParams h = TxData.PoolParams (ConcreteCrypto h)

type RewardAcnt h = TxData.RewardAcnt (ConcreteCrypto h)

type StakePools h = TxData.StakePools (ConcreteCrypto h)

type KeyHash h kr = Keys.KeyHash kr (ConcreteCrypto h)

pattern KeyHash ::
  forall (h :: *) (ht :: Keys.HashType) (kr :: Keys.KeyRole ht).
  ( Keys.AlgorithmForHashType (ConcreteCrypto h) ht ~ h
  ) =>
  Keys.Hash (ConcreteCrypto h) (VerKeyDSIGN (DSIGN (ConcreteCrypto h))) ->
  KeyHash h (kr :: Keys.KeyRole ht)
pattern KeyHash h = Keys.KeyHash h

{-# COMPLETE KeyHash #-}

type GenDelegPair h = Keys.GenDelegPair (ConcreteCrypto h)

pattern GenDelegPair :: KeyHash h 'Keys.GenesisDelegate -> VRFKeyHash h -> GenDelegPair h
pattern GenDelegPair kh vrfKH = Keys.GenDelegPair kh vrfKH

{-# COMPLETE GenDelegPair #-}

type GenDelegs h = Keys.GenDelegs (ConcreteCrypto h)

pattern GenDelegs :: (Map (KeyHash h 'Keys.Genesis) (GenDelegPair h)) -> GenDelegs h
pattern GenDelegs m = Keys.GenDelegs m

{-# COMPLETE GenDelegs #-}

type KeyPair h kr = Keys.KeyPair kr (ConcreteCrypto h)

pattern KeyPair :: VKey h kr -> SignKeyDSIGN h -> KeyPair h kr
pattern KeyPair vk sk = Keys.KeyPair vk sk

{-# COMPLETE KeyPair #-}

type GenesisKeyPair h = Keys.KeyPair 'Keys.Genesis (ConcreteCrypto h)

type SignedDSIGN h = Keys.SignedDSIGN (ConcreteCrypto h)

type SignKeyDSIGN h = Keys.SignKeyDSIGN (ConcreteCrypto h)

type VKey h kr = Keys.VKey kr (ConcreteCrypto h)

pattern VKey :: VerKeyDSIGN (DSIGN (ConcreteCrypto h)) -> VKey h kr
pattern VKey x = Keys.VKey x

{-# COMPLETE VKey #-}

type KeyPairs h = LedgerState.KeyPairs (ConcreteCrypto h)

type MultiSigPairs h = [(MultiSig h, MultiSig h)]

type VKeyGenesis h = Keys.VKey 'Keys.Genesis (ConcreteCrypto h)

type EpochState h = LedgerState.EpochState (ConcreteCrypto h)

type NEWEPOCH h = STS.NewEpoch.NEWEPOCH (ConcreteCrypto h)

type LedgerState h = LedgerState.LedgerState (ConcreteCrypto h)

type UTxOState h = LedgerState.UTxOState (ConcreteCrypto h)

type DState h = LedgerState.DState (ConcreteCrypto h)

type PState h = LedgerState.PState (ConcreteCrypto h)

type DPState h = LedgerState.DPState (ConcreteCrypto h)

type StakeReference h = TxData.StakeReference (ConcreteCrypto h)

type Addr h = TxData.Addr (ConcreteCrypto h)

type Tx h = Tx.Tx (ConcreteCrypto h)

type TxBody h = Tx.TxBody (ConcreteCrypto h)

type TxIn h = Tx.TxIn (ConcreteCrypto h)

type TxOut h = Tx.TxOut (ConcreteCrypto h)

type TxId h = TxData.TxId (ConcreteCrypto h)

type UTxO h = UTxO.UTxO (ConcreteCrypto h)

type Block h = BlockChain.Block (ConcreteCrypto h)

type LaxBlock h = BlockChain.LaxBlock (ConcreteCrypto h)

type BHBody h = BlockChain.BHBody (ConcreteCrypto h)

type SignKeyKES h = Keys.SignKeyKES (ConcreteCrypto h)

type VerKeyKES h = Keys.VerKeyKES (ConcreteCrypto h)

type SignKeyVRF h = Keys.SignKeyVRF (ConcreteCrypto h)

type VerKeyVRF h = Keys.VerKeyVRF (ConcreteCrypto h)

type VrfKeyPairs h = [(SignKeyVRF h, VerKeyVRF h)]

type CertifiedVRF h = Keys.CertifiedVRF (ConcreteCrypto h)

type BHeader h = BlockChain.BHeader (ConcreteCrypto h)

type OCert h = OCert.OCert (ConcreteCrypto h)

type OCertEnv h = STS.Ocert.OCertEnv (ConcreteCrypto h)

type HashHeader h = BlockChain.HashHeader (ConcreteCrypto h)

type PrevHash h = BlockChain.PrevHash (ConcreteCrypto h)

type NewEpochState h = LedgerState.NewEpochState (ConcreteCrypto h)

type NonMyopic h = Rewards.NonMyopic (ConcreteCrypto h)

type RewardUpdate h = LedgerState.RewardUpdate (ConcreteCrypto h)

type OBftSlot h = LedgerState.OBftSlot (ConcreteCrypto h)

type ChainState h = STS.Chain.ChainState (ConcreteCrypto h)

type CHAIN h = STS.Chain.CHAIN (ConcreteCrypto h)

type TICK h = STS.Tick.TICK (ConcreteCrypto h)

type TickEnv h = STS.Tick.TickEnv (ConcreteCrypto h)

type UTXOW h = STS.Utxow.UTXOW (ConcreteCrypto h)

type UTXO h = STS.Utxo.UTXO (ConcreteCrypto h)

type UtxoEnv h = STS.Utxo.UtxoEnv (ConcreteCrypto h)

type DELEG h = STS.Deleg.DELEG (ConcreteCrypto h)

type DELPL h = STS.Delpl.DELPL (ConcreteCrypto h)

type LEDGER h = STS.Ledger.LEDGER (ConcreteCrypto h)

type LEDGERS h = STS.Ledgers.LEDGERS (ConcreteCrypto h)

type LedgerEnv = STS.Ledger.LedgerEnv

type DELEGS h = STS.Delegs.DELEGS (ConcreteCrypto h)

type POOL h = STS.Pool.POOL (ConcreteCrypto h)

type POOLREAP h = STS.PoolReap.POOLREAP (ConcreteCrypto h)

type PPUP h = STS.Ppup.PPUP (ConcreteCrypto h)

type Credential h kr = TxData.Credential kr (ConcreteCrypto h)

type StakeCreds h = TxData.StakeCreds (ConcreteCrypto h)

type MultiSig h = Scripts.MultiSig (ConcreteCrypto h)

type ScriptHash h = Scripts.ScriptHash (ConcreteCrypto h)

type WitVKey h = TxData.WitVKey (ConcreteCrypto h)

type WitnessSet h = Tx.WitnessSet (ConcreteCrypto h)

type Wdrl h = TxData.Wdrl (ConcreteCrypto h)

type SnapShot h = EpochBoundary.SnapShot (ConcreteCrypto h)

type SnapShots h = EpochBoundary.SnapShots (ConcreteCrypto h)

type Stake h = EpochBoundary.Stake (ConcreteCrypto h)

type Update h = PParams.Update (ConcreteCrypto h)

type ProposedPPUpdates h = PParams.ProposedPPUpdates (ConcreteCrypto h)

type VRFKeyHash h = Keys.Hash (ConcreteCrypto h) (Keys.VerKeyVRF (ConcreteCrypto h))

hashKeyVRF ::
  HashAlgorithm h =>
  Keys.VerKeyVRF (ConcreteCrypto h) ->
  VRFKeyHash h
hashKeyVRF = Keys.hashVerKeyVRF
