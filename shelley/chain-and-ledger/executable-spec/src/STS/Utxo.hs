{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Utxo
  ( UTXO
  ) where

import qualified Data.Map.Strict          as Map

import           Lens.Micro               ((%~), (&), (^.), (.~))

import           Coin
import           Delegation.Certificates
import           Keys
import           LedgerState hiding (dms)
import           PParams
import           Slot
import           Updates
import           UTxO

import           Control.State.Transition

import           STS.Up

data UTXO

instance STS UTXO where
  type State UTXO = UTxOState
  type Signal UTXO = Tx
  type Environment UTXO = (Slot, PParams, StakeKeys, StakePools, Dms)
  data PredicateFailure UTXO = BadInputsUTxO
                           | ExpiredUTxO Slot Slot
                           | InputSetEmptyUTxO
                           | FeeTooSmallUTxO Coin Coin
                           | ValueNotConservedUTxO Coin Coin
                           | UnexpectedFailureUTXO [ValidationError]
                           | UnexpectedSuccessUTXO
                           | BadExtraEntropyUTxO
                           | UpdateFailure (PredicateFailure UP)
                               deriving (Eq, Show)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

initialLedgerState :: InitialRule UTXO
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

utxoInductive :: TransitionRule UTXO
utxoInductive = do
  TRC ((slot, pp, stakeKeys, stakePools, dms), u, tx) <- judgmentContext

  let txbody = _body tx
  validInputs txbody u == Valid ?! BadInputsUTxO
  _ttl txbody >= slot ?! ExpiredUTxO (_ttl txbody) slot
  validNoReplay txbody == Valid ?! InputSetEmptyUTxO

  let validateFee = validFee pp txbody
  validateFee == Valid ?! unwrapFailureUTXO validateFee

  let validateBalance = preserveBalance stakePools stakeKeys pp txbody u
  validateBalance == Valid ?! unwrapFailureUTXO validateBalance

  let refunded = keyRefunds pp stakeKeys txbody
  let decayed = decayedTx pp stakeKeys txbody
  let depositChange =
        deposits pp stakePools (txbody ^. certs) - (refunded + decayed)

  let u' = applyUTxOUpdate u txbody  -- change UTxO

  -- process Update Proposals
  ups' <- trans @UP $ TRC ((slot, dms), u ^. ups, txup tx)

  pure $
    u' & deposited %~ (+) depositChange
       & fees      %~ (+) ((txbody ^. txfee) + decayed)
       & ups       .~ ups'

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

instance Embed UP UTXO where
  wrapFailed = UpdateFailure
