{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Utxo
  ( UTXO
  ) where

import qualified Data.Map.Strict          as Map

import           Lens.Micro               ((%~), (&), (^.))

import           Coin
import           Delegation.Certificates
import           LedgerState
import           PParams
import           Slot
import           UTxO

import           Control.State.Transition

data UTXO

instance STS UTXO where
  type State UTXO = UTxOState
  type Signal UTXO = Tx
  type Environment UTXO = (Slot, PParams, StakeKeys, StakePools)
  data PredicateFailure UTXO = BadInputsUTxO
                           | ExpiredUTxO Slot Slot
                           | InputSetEmptyUTxO
                           | FeeTooSmallUTxO Coin Coin
                           | ValueNotConservedUTxO Coin Coin
                           | UnexpectedFailureUTXO [ValidationError]
                           | UnexpectedSuccessUTXO
                               deriving (Eq, Show)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

initialLedgerState :: InitialRule UTXO
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0)

utxoInductive :: TransitionRule UTXO
utxoInductive = do
  TRC ((slot, pp, stakeKeys, stakePools), u, tx) <- judgmentContext
  validInputs tx u == Valid ?! BadInputsUTxO
  current tx slot == Valid ?! ExpiredUTxO (tx ^. ttl) slot
  validNoReplay tx == Valid ?! InputSetEmptyUTxO
  let validateFee = validFee pp tx
  validateFee == Valid ?! unwrapFailureUTXO validateFee
  let validateBalance = preserveBalance stakePools stakeKeys pp tx u
  validateBalance == Valid ?! unwrapFailureUTXO validateBalance
  let refunded = keyRefunds pp stakeKeys tx
  let decayed = decayedTx pp stakeKeys tx
  let depositChange =
        deposits pp stakePools (tx ^. certs) - (refunded + decayed)
  let u' = applyUTxOUpdate u tx
  pure
    (u' & deposited %~ (+) depositChange & fees %~ (+) ((tx ^. txfee) + decayed))

unwrapFailureUTXO :: Validity -> PredicateFailure UTXO
unwrapFailureUTXO (Invalid [e]) = unwrapFailureUTXO' e
unwrapFailureUTXO Valid         = UnexpectedSuccessUTXO
unwrapFailureUTXO (Invalid x)   = UnexpectedFailureUTXO x

unwrapFailureUTXO' :: ValidationError -> PredicateFailure UTXO
unwrapFailureUTXO' BadInputs                = BadInputsUTxO
unwrapFailureUTXO' (Expired s s')           = ExpiredUTxO s s'
unwrapFailureUTXO' InputSetEmpty            = InputSetEmptyUTxO
unwrapFailureUTXO' (FeeTooSmall c c')       = FeeTooSmallUTxO c c'
unwrapFailureUTXO' (ValueNotConserved c c') = ValueNotConservedUTxO c c'
unwrapFailureUTXO' x                        = UnexpectedFailureUTXO [x]
