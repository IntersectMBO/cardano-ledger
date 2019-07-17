module MockTypes where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.KES (MockKES)

import qualified BlockChain
import qualified Delegation.Certificates
import qualified Keys
import qualified LedgerState
import qualified OCert
import qualified STS.Chain
import qualified STS.Utxow
import qualified Tx
import qualified TxData
import qualified UTxO

type DCert = Delegation.Certificates.DCert ShortHash MockDSIGN

type Delegation = TxData.Delegation ShortHash MockDSIGN

type PoolParams = TxData.PoolParams ShortHash MockDSIGN

type RewardAcnt = TxData.RewardAcnt ShortHash MockDSIGN

type KeyHash = Keys.KeyHash ShortHash MockDSIGN

type KeyPair = Keys.KeyPair MockDSIGN

type VKey = Keys.VKey MockDSIGN

type SKey = Keys.SKey MockDSIGN

type KeyPairs = LedgerState.KeyPairs MockDSIGN

type LedgerState = LedgerState.LedgerState ShortHash MockDSIGN

type LedgerValidation = LedgerState.LedgerValidation ShortHash MockDSIGN

type UTxOState = LedgerState.UTxOState ShortHash MockDSIGN

type DPState = LedgerState.DPState ShortHash MockDSIGN

type Addr = TxData.Addr ShortHash MockDSIGN

type Tx = Tx.Tx ShortHash MockDSIGN

type TxBody = Tx.TxBody ShortHash MockDSIGN

type TxIn = Tx.TxIn ShortHash MockDSIGN

type TxOut = Tx.TxOut ShortHash MockDSIGN

type TxId = TxData.TxId ShortHash MockDSIGN

type UTxO = UTxO.UTxO ShortHash MockDSIGN

type Block = BlockChain.Block ShortHash MockDSIGN MockKES

type BHBody = BlockChain.BHBody ShortHash MockDSIGN MockKES

type SKeyES = Keys.SKeyES MockKES

type VKeyES = Keys.VKeyES MockKES

type KESig = Keys.KESig MockKES BHBody

type Sig a = Keys.Sig MockDSIGN a

type Proof a = BlockChain.Proof MockDSIGN

type BHeader = BlockChain.BHeader ShortHash MockDSIGN MockKES

type OCert = OCert.OCert MockDSIGN MockKES

type HashHeader = BlockChain.HashHeader ShortHash MockDSIGN MockKES

type NewEpochState = LedgerState.NewEpochState ShortHash MockDSIGN

type CHAIN = STS.Chain.CHAIN ShortHash MockDSIGN MockKES

type UTXOW = STS.Utxow.UTXOW ShortHash MockDSIGN

type StakeObject = TxData.StakeObject ShortHash MockDSIGN

type MultiSig = TxData.MultiSig ShortHash MockDSIGN

type ScriptHash = TxData.ScriptHash ShortHash MockDSIGN

type WitVKey = TxData.WitVKey ShortHash MockDSIGN
