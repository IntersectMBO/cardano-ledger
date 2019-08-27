{-# LANGUAGE DataKinds #-}

module MockTypes where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.KES (MockKES)
import           Cardano.Crypto.VRF.Fake (FakeVRF)

import qualified BlockChain
import qualified Delegation.Certificates
import qualified EpochBoundary
import qualified Keys
import qualified LedgerState
import qualified OCert
import qualified STS.Chain
import qualified STS.Deleg
import qualified STS.Delegs
import qualified STS.Ledger
import qualified STS.NewEpoch
import qualified STS.Pool
import qualified STS.PoolReap
import qualified STS.Utxo
import qualified STS.Utxow
import qualified Tx
import qualified TxData
import qualified Updates
import qualified UTxO

type DCert = Delegation.Certificates.DCert ShortHash MockDSIGN FakeVRF

type PoolDistr = Delegation.Certificates.PoolDistr ShortHash MockDSIGN FakeVRF

type Delegation = TxData.Delegation ShortHash MockDSIGN

type PoolParams = TxData.PoolParams ShortHash MockDSIGN FakeVRF

type RewardAcnt = TxData.RewardAcnt ShortHash MockDSIGN

type StakePools = TxData.StakePools ShortHash MockDSIGN

type KeyHash = Keys.KeyHash ShortHash MockDSIGN

type GenKeyHash = Keys.GenKeyHash ShortHash MockDSIGN

type KeyPair = Keys.KeyPair 'Keys.Regular MockDSIGN

type VKey = Keys.VKey MockDSIGN

type SKey = Keys.SKey MockDSIGN

type KeyPairs = LedgerState.KeyPairs MockDSIGN

type VKeyGenesis = Keys.VKeyGenesis MockDSIGN

type EpochState = LedgerState.EpochState ShortHash MockDSIGN FakeVRF

type NEWEPOCH = STS.NewEpoch.NEWEPOCH ShortHash MockDSIGN FakeVRF

type LedgerState = LedgerState.LedgerState ShortHash MockDSIGN FakeVRF

type LedgerValidation = LedgerState.LedgerValidation ShortHash MockDSIGN FakeVRF

type UTxOState = LedgerState.UTxOState ShortHash MockDSIGN FakeVRF

type DState = LedgerState.DState ShortHash MockDSIGN

type PState = LedgerState.PState ShortHash MockDSIGN FakeVRF

type DPState = LedgerState.DPState ShortHash MockDSIGN FakeVRF

type Addr = TxData.Addr ShortHash MockDSIGN

type Tx = Tx.Tx ShortHash MockDSIGN FakeVRF

type TxBody = Tx.TxBody ShortHash MockDSIGN FakeVRF

type TxIn = Tx.TxIn ShortHash MockDSIGN FakeVRF

type TxOut = Tx.TxOut ShortHash MockDSIGN

type TxId = TxData.TxId ShortHash MockDSIGN FakeVRF

type UTxO = UTxO.UTxO ShortHash MockDSIGN FakeVRF

type Block = BlockChain.Block ShortHash MockDSIGN MockKES FakeVRF

type BHBody = BlockChain.BHBody ShortHash MockDSIGN MockKES FakeVRF

type SKeyES = Keys.SKeyES MockKES

type VKeyES = Keys.VKeyES MockKES

type SignKeyVRF = Keys.SignKeyVRF FakeVRF

type VerKeyVRF = Keys.VerKeyVRF FakeVRF

type CertifiedVRF = Keys.CertifiedVRF FakeVRF

type KESig = Keys.KESig MockKES BHBody

type Sig a = Keys.Sig MockDSIGN a

type BHeader = BlockChain.BHeader ShortHash MockDSIGN MockKES

type OCert = OCert.OCert MockDSIGN MockKES

type HashHeader = BlockChain.HashHeader ShortHash MockDSIGN MockKES FakeVRF

type NewEpochState = LedgerState.NewEpochState ShortHash MockDSIGN

type RewardUpdate = LedgerState.RewardUpdate ShortHash MockDSIGN

type ChainState = STS.Chain.ChainState ShortHash MockDSIGN MockKES FakeVRF

type CHAIN = STS.Chain.CHAIN ShortHash MockDSIGN MockKES FakeVRF

type UTXOW = STS.Utxow.UTXOW ShortHash MockDSIGN FakeVRF

type UTXO = STS.Utxo.UTXO ShortHash MockDSIGN FakeVRF

type UtxoEnv = STS.Utxo.UtxoEnv ShortHash MockDSIGN

type DELEG = STS.Deleg.DELEG ShortHash MockDSIGN FakeVRF

type LEDGER = STS.Ledger.LEDGER ShortHash MockDSIGN FakeVRF

type LedgerEnv = STS.Ledger.LedgerEnv

type DELEGS = STS.Delegs.DELEGS ShortHash MockDSIGN FakeVRF

type POOL = STS.Pool.POOL ShortHash MockDSIGN FakeVRF

type POOLREAP = STS.PoolReap.POOLREAP ShortHash MockDSIGN FakeVRF

type Credential = TxData.Credential ShortHash MockDSIGN

type StakeCredential = TxData.StakeCredential ShortHash MockDSIGN
type StakeKeys = TxData.StakeKeys ShortHash MockDSIGN

type MultiSig = TxData.MultiSig ShortHash MockDSIGN

type ScriptHash = TxData.ScriptHash ShortHash MockDSIGN

type WitVKey = TxData.WitVKey ShortHash MockDSIGN

type Wdrl = TxData.Wdrl ShortHash MockDSIGN

type SnapShots = EpochBoundary.SnapShots ShortHash MockDSIGN FakeVRF

type Stake = EpochBoundary.Stake ShortHash MockDSIGN

type Mdt = Updates.Mdt ShortHash

type Applications = Updates.Applications ShortHash

type Update = Updates.Update ShortHash MockDSIGN

type UpdateState = Updates.UpdateState ShortHash MockDSIGN

type PPUpdate = Updates.PPUpdate ShortHash MockDSIGN

type AVUpdate = Updates.AVUpdate ShortHash MockDSIGN
