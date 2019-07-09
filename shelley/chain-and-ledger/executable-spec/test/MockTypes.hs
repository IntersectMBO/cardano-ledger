module MockTypes where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)

import qualified Delegation.Certificates
import qualified Delegation.PoolParams
import qualified Keys
import qualified LedgerState
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
