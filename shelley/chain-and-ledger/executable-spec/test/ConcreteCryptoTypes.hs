{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ConcreteCryptoTypes where

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

data ConcreteCrypto

instance Crypto ConcreteCrypto where
  type HASH ConcreteCrypto = ShortHash
  type DSIGN ConcreteCrypto = MockDSIGN
  type KES ConcreteCrypto = MockKES
  type VRF ConcreteCrypto = FakeVRF

type DCert = Delegation.Certificates.DCert ConcreteCrypto

type PoolDistr = Delegation.Certificates.PoolDistr ConcreteCrypto

type Delegation = TxData.Delegation ConcreteCrypto

type PoolParams = TxData.PoolParams ConcreteCrypto

type RewardAcnt = TxData.RewardAcnt ConcreteCrypto

type StakePools = TxData.StakePools ConcreteCrypto

type KeyHash = Keys.KeyHash ConcreteCrypto

type GenKeyHash = Keys.GenKeyHash ConcreteCrypto

type KeyPair = Keys.KeyPair 'Keys.Regular ConcreteCrypto

type CoreKeyPair = Keys.KeyPair 'Keys.Genesis ConcreteCrypto

type VKey = Keys.VKey ConcreteCrypto

type SKey = Keys.SKey ConcreteCrypto

type KeyPairs = LedgerState.KeyPairs ConcreteCrypto

type MultiSigPairs = [(MultiSig, MultiSig)]

type VKeyGenesis = Keys.VKeyGenesis ConcreteCrypto

type EpochState = LedgerState.EpochState ConcreteCrypto

type NEWEPOCH = STS.NewEpoch.NEWEPOCH ConcreteCrypto

type LedgerState = LedgerState.LedgerState ConcreteCrypto

type LedgerValidation = LedgerState.LedgerValidation ConcreteCrypto

type UTxOState = LedgerState.UTxOState ConcreteCrypto

type DState = LedgerState.DState ConcreteCrypto

type PState = LedgerState.PState ConcreteCrypto

type DPState = LedgerState.DPState ConcreteCrypto

type Addr = TxData.Addr ConcreteCrypto

type Tx = Tx.Tx ConcreteCrypto

type TxBody = Tx.TxBody ConcreteCrypto

type TxIn = Tx.TxIn ConcreteCrypto

type TxOut = Tx.TxOut ConcreteCrypto

type TxId = TxData.TxId ConcreteCrypto

type UTxO = UTxO.UTxO ConcreteCrypto

type Block = BlockChain.Block ConcreteCrypto

type BHBody = BlockChain.BHBody ConcreteCrypto

type SKeyES = Keys.SKeyES ConcreteCrypto

type VKeyES = Keys.VKeyES ConcreteCrypto

type SignKeyVRF = Keys.SignKeyVRF (VRF ConcreteCrypto)

type VerKeyVRF = Keys.VerKeyVRF (VRF ConcreteCrypto)

type VrfKeyPairs = [(SignKeyVRF, VerKeyVRF)]

type CertifiedVRF = Keys.CertifiedVRF (VRF ConcreteCrypto)

type KESig = Keys.KESig ConcreteCrypto BHBody

type Sig a = Keys.Sig ConcreteCrypto a

type BHeader = BlockChain.BHeader ConcreteCrypto

type OCert = OCert.OCert ConcreteCrypto

type HashHeader = BlockChain.HashHeader ConcreteCrypto

type NewEpochState = LedgerState.NewEpochState ConcreteCrypto

type RewardUpdate = LedgerState.RewardUpdate ConcreteCrypto

type ChainState = STS.Chain.ChainState ConcreteCrypto

type CHAIN = STS.Chain.CHAIN ConcreteCrypto

type UTXOW = STS.Utxow.UTXOW ConcreteCrypto

type UTXO = STS.Utxo.UTXO ConcreteCrypto

type UtxoEnv = STS.Utxo.UtxoEnv ConcreteCrypto

type DELEG = STS.Deleg.DELEG ConcreteCrypto

type LEDGER = STS.Ledger.LEDGER ConcreteCrypto

type LedgerEnv = STS.Ledger.LedgerEnv

type DELEGS = STS.Delegs.DELEGS ConcreteCrypto

type POOL = STS.Pool.POOL ConcreteCrypto

type POOLREAP = STS.PoolReap.POOLREAP ConcreteCrypto

type Credential = TxData.Credential ConcreteCrypto

type StakeCreds = TxData.StakeCreds ConcreteCrypto

type MultiSig = TxData.MultiSig ConcreteCrypto

type ScriptHash = TxData.ScriptHash ConcreteCrypto

type WitVKey = TxData.WitVKey ConcreteCrypto

type Wdrl = TxData.Wdrl ConcreteCrypto

type SnapShots = EpochBoundary.SnapShots ConcreteCrypto

type Stake = EpochBoundary.Stake ConcreteCrypto

type Mdt = Updates.Mdt ConcreteCrypto

type Applications = Updates.Applications ConcreteCrypto

type InstallerHash = Updates.InstallerHash ConcreteCrypto

type Update = Updates.Update ConcreteCrypto

type UpdateState = Updates.UpdateState ConcreteCrypto

type PPUpdate = Updates.PPUpdate ConcreteCrypto

type AVUpdate = Updates.AVUpdate ConcreteCrypto

type VRFKeyHash = Keys.Hash ShortHash (Keys.VerKeyVRF FakeVRF)

hashKeyVRF
  :: Keys.VerKeyVRF FakeVRF
  -> VRFKeyHash
hashKeyVRF = Keys.hashKeyVRF @ConcreteCrypto
