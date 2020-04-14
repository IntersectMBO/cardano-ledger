{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.ConcreteCryptoTypes where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.KES (MockKES)
import           Test.Cardano.Crypto.VRF.Fake (FakeVRF)

import           Shelley.Spec.Ledger.Crypto
import qualified Shelley.Spec.Ledger.BlockChain as BlockChain
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Delegation.Certificates
import qualified Shelley.Spec.Ledger.EpochBoundary as EpochBoundary
import qualified Shelley.Spec.Ledger.Keys as Keys
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.OCert as OCert
import qualified Shelley.Spec.Ledger.PParams as PParams
import qualified Shelley.Spec.Ledger.Rewards as Rewards
import qualified Shelley.Spec.Ledger.Scripts as Scripts
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
import qualified Shelley.Spec.Ledger.STS.Tick as STS.Tick
import qualified Shelley.Spec.Ledger.STS.Utxo as STS.Utxo
import qualified Shelley.Spec.Ledger.STS.Utxow as STS.Utxow
import qualified Shelley.Spec.Ledger.Tx as Tx
import qualified Shelley.Spec.Ledger.TxData as TxData
import qualified Shelley.Spec.Ledger.UTxO as UTxO

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

type AnyKeyHash = Keys.AnyKeyHash ConcreteCrypto

type KeyHash = Keys.KeyHash ConcreteCrypto

type GenKeyHash = Keys.GenKeyHash ConcreteCrypto

type GenDelegs = Keys.GenDelegs ConcreteCrypto

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

type LaxBlock = BlockChain.LaxBlock ConcreteCrypto

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

type OCertEnv = STS.Ocert.OCertEnv ConcreteCrypto

type HashHeader = BlockChain.HashHeader ConcreteCrypto

type PrevHash = BlockChain.PrevHash ConcreteCrypto

type NewEpochState = LedgerState.NewEpochState ConcreteCrypto

type NonMyopic = Rewards.NonMyopic ConcreteCrypto

type RewardUpdate = LedgerState.RewardUpdate ConcreteCrypto

type OBftSlot = LedgerState.OBftSlot ConcreteCrypto

type ChainState = STS.Chain.ChainState ConcreteCrypto

type CHAIN = STS.Chain.CHAIN ConcreteCrypto

type TICK = STS.Tick.TICK ConcreteCrypto

type TickEnv = STS.Tick.TickEnv ConcreteCrypto

type UTXOW = STS.Utxow.UTXOW ConcreteCrypto

type UTXO = STS.Utxo.UTXO ConcreteCrypto

type UtxoEnv = STS.Utxo.UtxoEnv ConcreteCrypto

type DELEG = STS.Deleg.DELEG ConcreteCrypto

type DELPL = STS.Delpl.DELPL ConcreteCrypto

type LEDGER = STS.Ledger.LEDGER ConcreteCrypto

type LEDGERS = STS.Ledgers.LEDGERS ConcreteCrypto

type LedgerEnv = STS.Ledger.LedgerEnv

type DELEGS = STS.Delegs.DELEGS ConcreteCrypto

type POOL = STS.Pool.POOL ConcreteCrypto

type POOLREAP = STS.PoolReap.POOLREAP ConcreteCrypto

type Credential = TxData.Credential ConcreteCrypto

type StakeCreds = TxData.StakeCreds ConcreteCrypto

type MultiSig = Scripts.MultiSig ConcreteCrypto

type ScriptHash = Scripts.ScriptHash ConcreteCrypto

type WitVKey = TxData.WitVKey ConcreteCrypto

type Wdrl = TxData.Wdrl ConcreteCrypto

type SnapShot = EpochBoundary.SnapShot ConcreteCrypto

type SnapShots = EpochBoundary.SnapShots ConcreteCrypto

type Stake = EpochBoundary.Stake ConcreteCrypto

type Update = PParams.Update ConcreteCrypto

type ProposedPPUpdates = PParams.ProposedPPUpdates ConcreteCrypto

type VRFKeyHash = Keys.Hash ShortHash (Keys.VerKeyVRF FakeVRF)

hashKeyVRF
  :: Keys.VerKeyVRF FakeVRF
  -> VRFKeyHash
hashKeyVRF = Keys.hashKeyVRF @ConcreteCrypto
