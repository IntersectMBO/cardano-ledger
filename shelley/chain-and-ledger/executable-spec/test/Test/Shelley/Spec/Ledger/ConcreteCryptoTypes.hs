{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.ConcreteCryptoTypes where

import Cardano.Crypto.DSIGN (MockDSIGN, VerKeyDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.Hash (HashAlgorithm, ShortHash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.KES (MockKES)
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF.Class as VRF
import Data.Map (Map)
import qualified Shelley.Spec.Ledger.Address as TxData
import qualified Shelley.Spec.Ledger.Address.Bootstrap as TxData
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
import qualified Shelley.Spec.Ledger.Value as Value
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)

type C = ConcreteCrypto ShortHash

data ConcreteCrypto (h :: *)

type Mock c =
  ( Crypto c,
    Num (DSIGN.SignKeyDSIGN (DSIGN c)),
    Num (VerKeyDSIGN (DSIGN c)),
    (VRF c) ~ FakeVRF,
    KES.Signable (KES c) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    HASH c ~ ADDRHASH c
  )

instance HashAlgorithm h => Crypto (ConcreteCrypto h) where
  type HASH (ConcreteCrypto h) = h
  type ADDRHASH (ConcreteCrypto h) = h
  type DSIGN (ConcreteCrypto h) = MockDSIGN
  type KES (ConcreteCrypto h) = MockKES 10
  type VRF (ConcreteCrypto h) = FakeVRF

type DCert c = Delegation.Certificates.DCert c

type PoolDistr c = Delegation.Certificates.PoolDistr c

type Delegation c = TxData.Delegation c

type PoolParams c = TxData.PoolParams c

type RewardAcnt c = TxData.RewardAcnt c

type GenDelegPair c = Keys.GenDelegPair c

pattern GenDelegPair :: Keys.KeyHash 'Keys.GenesisDelegate c -> Hash.Hash (HASH c) (Keys.VerKeyVRF c) -> GenDelegPair c
pattern GenDelegPair kh vrfKH = Keys.GenDelegPair kh vrfKH

{-# COMPLETE GenDelegPair #-}

type GenDelegs c = Keys.GenDelegs c

pattern GenDelegs :: (Map (Keys.KeyHash 'Keys.Genesis h) (GenDelegPair h)) -> GenDelegs h
pattern GenDelegs m = Keys.GenDelegs m

{-# COMPLETE GenDelegs #-}

type GenesisKeyPair c = Keys.KeyPair 'Keys.Genesis c

type SignedDSIGN c = Keys.SignedDSIGN c

type SignKeyDSIGN c = Keys.SignKeyDSIGN c

type KeyPairs c = LedgerState.KeyPairs c

type MultiSigPairs h = [(MultiSig h, MultiSig h)]

type VKeyGenesis c = Keys.VKey 'Keys.Genesis c

type EpochState c = LedgerState.EpochState c

type NEWEPOCH c = STS.NewEpoch.NEWEPOCH c

type LedgerState c = LedgerState.LedgerState c

type UTxOState c = LedgerState.UTxOState c

type DState c = LedgerState.DState c

type PState c = LedgerState.PState c

type DPState c = LedgerState.DPState c

type StakeReference c = TxData.StakeReference c

type Addr c = TxData.Addr c

type Tx c = Tx.Tx c

type TxBody c = Tx.TxBody c

type TxIn c = Tx.TxIn c

type TxOut c = Tx.TxOut c

type TxId c = TxData.TxId c

type UTxO c = UTxO.UTxO c

type UTxOOut c = TxData.UTxOOut c

type Value c = Value.Value c

type CompactValue c = Value.CompactValue c

type Block c = BlockChain.Block c

type LaxBlock c = BlockChain.LaxBlock c

type BHBody c = BlockChain.BHBody c

type SignKeyKES c = Keys.SignKeyKES c

type VerKeyKES c = Keys.VerKeyKES c

type SignKeyVRF c = Keys.SignKeyVRF c

type VerKeyVRF c = Keys.VerKeyVRF c

type VrfKeyPairs h = [(SignKeyVRF h, VerKeyVRF h)]

type CertifiedVRF c = Keys.CertifiedVRF c

type BHeader c = BlockChain.BHeader c

type OCert c = OCert.OCert c

type OCertEnv c = STS.Ocert.OCertEnv c

type HashHeader c = BlockChain.HashHeader c

type PrevHash c = BlockChain.PrevHash c

type NewEpochState c = LedgerState.NewEpochState c

type NonMyopic c = Rewards.NonMyopic c

type RewardUpdate c = LedgerState.RewardUpdate c

type OBftSlot c = LedgerState.OBftSlot c

type ChainState c = STS.Chain.ChainState c

type CHAIN c = STS.Chain.CHAIN c

type TICK c = STS.Tick.TICK c

type TickEnv c = STS.Tick.TickEnv c

type UTXOW c = STS.Utxow.UTXOW c

type UTXO c = STS.Utxo.UTXO c

type UtxoEnv c = STS.Utxo.UtxoEnv c

type DELEG c = STS.Deleg.DELEG c

type DELPL c = STS.Delpl.DELPL c

type LEDGER c = STS.Ledger.LEDGER c

type LEDGERS c = STS.Ledgers.LEDGERS c

type LedgerEnv = STS.Ledger.LedgerEnv

type DELEGS c = STS.Delegs.DELEGS c

type POOL c = STS.Pool.POOL c

type POOLREAP c = STS.PoolReap.POOLREAP c

type PPUP c = STS.Ppup.PPUP c

type Credential c kr = TxData.Credential kr c

type StakeCreds c = TxData.StakeCreds c

type MultiSig c = Scripts.MultiSig c

type ScriptHash c = Scripts.ScriptHash c

type WitVKey c = TxData.WitVKey c

type WitnessSet c = Tx.WitnessSet c

type Wdrl c = TxData.Wdrl c

type SnapShot c = EpochBoundary.SnapShot c

type SnapShots c = EpochBoundary.SnapShots c

type Stake c = EpochBoundary.Stake c

type Update c = PParams.Update c

type ProposedPPUpdates c = PParams.ProposedPPUpdates c

type VRFKeyHash c = Keys.Hash c (Keys.VerKeyVRF c)

hashKeyVRF :: (HashAlgorithm h, VRF.VRFAlgorithm v) => VRF.VerKeyVRF v -> Hash.Hash h (VRF.VerKeyVRF v)
hashKeyVRF = Keys.hashVerKeyVRF

type BootstrapWitness c = TxData.BootstrapWitness c
