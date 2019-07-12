module MockTypes where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.KES (MockKES)

import qualified BlockChain
import qualified Delegation.Certificates
import qualified Delegation.PoolParams
import qualified Keys
import qualified LedgerState
import qualified OCert
import qualified STS.Chain
import qualified UTxO

type DCert = Delegation.Certificates.DCert ShortHash MockDSIGN

type Delegation = Delegation.PoolParams.Delegation MockDSIGN

type PoolParams = Delegation.PoolParams.PoolParams ShortHash MockDSIGN

type RewardAcnt = Delegation.PoolParams.RewardAcnt ShortHash MockDSIGN

type KeyHash = Keys.KeyHash ShortHash MockDSIGN

type KeyPair = Keys.KeyPair MockDSIGN

type VKey = Keys.VKey MockDSIGN

type KeyPairs = LedgerState.KeyPairs MockDSIGN

type LedgerState = LedgerState.LedgerState ShortHash MockDSIGN

type LedgerValidation = LedgerState.LedgerValidation ShortHash MockDSIGN

type UTxOState = LedgerState.UTxOState ShortHash MockDSIGN

type DPState = LedgerState.DPState ShortHash MockDSIGN

type Addr = UTxO.Addr ShortHash MockDSIGN

type Tx = UTxO.Tx ShortHash MockDSIGN

type TxBody = UTxO.TxBody ShortHash MockDSIGN

type TxIn = UTxO.TxIn ShortHash MockDSIGN

type TxOut = UTxO.TxOut ShortHash MockDSIGN

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
