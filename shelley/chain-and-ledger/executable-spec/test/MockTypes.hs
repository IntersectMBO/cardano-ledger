{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MockTypes where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.KES (MockKES)
import           Cardano.Crypto.VRF.Fake (FakeVRF)

import qualified BlockChain
import           Cardano.Ledger.Shelley.Crypto
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

data MockCrypto

instance Crypto MockCrypto where
  type HASH MockCrypto = ShortHash
  type DSIGN MockCrypto = MockDSIGN
  type KES MockCrypto = MockKES
  type VRF MockCrypto = FakeVRF

type DCert = Delegation.Certificates.DCert MockCrypto

type PoolDistr = Delegation.Certificates.PoolDistr MockCrypto

type Delegation = TxData.Delegation MockCrypto

type PoolParams = TxData.PoolParams MockCrypto

type RewardAcnt = TxData.RewardAcnt MockCrypto

type StakePools = TxData.StakePools MockCrypto

type KeyHash = Keys.KeyHash MockCrypto

type GenKeyHash = Keys.GenKeyHash MockCrypto

type KeyPair = Keys.KeyPair 'Keys.Regular MockCrypto

type CoreKeyPair = Keys.KeyPair 'Keys.Genesis MockCrypto

type CoreKeyPairs = [CoreKeyPair]

type VKey = Keys.VKey MockCrypto

type SKey = Keys.SKey MockCrypto

type KeyPairs = LedgerState.KeyPairs MockCrypto

type VKeyGenesis = Keys.VKeyGenesis MockCrypto

type EpochState = LedgerState.EpochState MockCrypto

type NEWEPOCH = STS.NewEpoch.NEWEPOCH MockCrypto

type LedgerState = LedgerState.LedgerState MockCrypto

type LedgerValidation = LedgerState.LedgerValidation MockCrypto

type UTxOState = LedgerState.UTxOState MockCrypto

type DState = LedgerState.DState MockCrypto

type PState = LedgerState.PState MockCrypto

type DPState = LedgerState.DPState MockCrypto

type Addr = TxData.Addr MockCrypto

type Tx = Tx.Tx MockCrypto

type TxBody = Tx.TxBody MockCrypto

type TxIn = Tx.TxIn MockCrypto

type TxOut = Tx.TxOut MockCrypto

type TxId = TxData.TxId MockCrypto

type UTxO = UTxO.UTxO MockCrypto

type Block = BlockChain.Block MockCrypto

type BHBody = BlockChain.BHBody MockCrypto

type SKeyES = Keys.SKeyES MockCrypto

type VKeyES = Keys.VKeyES MockCrypto

type SignKeyVRF = Keys.SignKeyVRF (VRF MockCrypto)

type VerKeyVRF = Keys.VerKeyVRF (VRF MockCrypto)

type VrfKeyPairs = [(SignKeyVRF, VerKeyVRF)]

type CertifiedVRF = Keys.CertifiedVRF (VRF MockCrypto)

type KESig = Keys.KESig MockCrypto BHBody

type Sig a = Keys.Sig MockCrypto a

type BHeader = BlockChain.BHeader MockCrypto

type OCert = OCert.OCert MockCrypto

type HashHeader = BlockChain.HashHeader MockCrypto

type NewEpochState = LedgerState.NewEpochState MockCrypto

type RewardUpdate = LedgerState.RewardUpdate MockCrypto

type ChainState = STS.Chain.ChainState MockCrypto

type CHAIN = STS.Chain.CHAIN MockCrypto

type UTXOW = STS.Utxow.UTXOW MockCrypto

type UTXO = STS.Utxo.UTXO MockCrypto

type UtxoEnv = STS.Utxo.UtxoEnv MockCrypto

type DELEG = STS.Deleg.DELEG MockCrypto

type LEDGER = STS.Ledger.LEDGER MockCrypto

type LedgerEnv = STS.Ledger.LedgerEnv

type DELEGS = STS.Delegs.DELEGS MockCrypto

type POOL = STS.Pool.POOL MockCrypto

type POOLREAP = STS.PoolReap.POOLREAP MockCrypto

type Credential = TxData.Credential MockCrypto

type StakeCredential = TxData.StakeCredential MockCrypto

type StakeCreds = TxData.StakeCreds MockCrypto

type MultiSig = TxData.MultiSig MockCrypto

type ScriptHash = TxData.ScriptHash MockCrypto

type WitVKey = TxData.WitVKey MockCrypto

type Wdrl = TxData.Wdrl MockCrypto

type SnapShots = EpochBoundary.SnapShots MockCrypto

type Stake = EpochBoundary.Stake MockCrypto

type Mdt = Updates.Mdt MockCrypto

type Applications = Updates.Applications MockCrypto

type InstallerHash = Updates.InstallerHash MockCrypto

type Update = Updates.Update MockCrypto

type UpdateState = Updates.UpdateState MockCrypto

type PPUpdate = Updates.PPUpdate MockCrypto

type AVUpdate = Updates.AVUpdate MockCrypto

type VRFKeyHash = Keys.Hash ShortHash (Keys.VerKeyVRF FakeVRF)

hashKeyVRF
  :: Keys.VerKeyVRF FakeVRF
  -> VRFKeyHash
hashKeyVRF = Keys.hashKeyVRF @MockCrypto
