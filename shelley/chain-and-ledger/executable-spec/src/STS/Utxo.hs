{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Utxo
  ( UTXO
  )
where

import qualified Data.Map.Strict as Map

import           Lens.Micro ((%~), (&), (.~), (^.))

import           Coin
import           Delegation.Certificates
import           Keys
import           LedgerState
import           PParams
import           Slot
import           Tx
import           Updates
import           UTxO

import           Control.State.Transition

import           STS.Up

data UTXO hashAlgo dsignAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (UTXO hashAlgo dsignAlgo)
 where
  type State (UTXO hashAlgo dsignAlgo) = UTxOState hashAlgo dsignAlgo
  type Signal (UTXO hashAlgo dsignAlgo) = Tx hashAlgo dsignAlgo
  type Environment (UTXO hashAlgo dsignAlgo)
    = ( Slot
      , PParams
      , StakeKeys hashAlgo dsignAlgo
      , StakePools hashAlgo dsignAlgo
      , Dms dsignAlgo
      )
  data PredicateFailure (UTXO hashAlgo dsignAlgo)
    = BadInputsUTxO
    | ExpiredUTxO Slot Slot
    | InputSetEmptyUTxO
    | FeeTooSmallUTxO Coin Coin
    | ValueNotConservedUTxO Coin Coin
    | UnexpectedFailureUTXO [ValidationError] -- TODO maybe restructure Validity
                                              -- to prevent these predicate
                                              -- failures?
    | UnexpectedSuccessUTXO
    | UpdateFailure (PredicateFailure (UP dsignAlgo))
    deriving (Eq, Show)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

initialLedgerState :: InitialRule (UTXO hashAlgo dsignAlgo)
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

utxoInductive
  :: forall hashAlgo dsignAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (UTXO hashAlgo dsignAlgo)
utxoInductive = do
  TRC ((_slot, pp, stakeKeys, stakePools, _dms), u, tx) <- judgmentContext

  let txbody = _body tx
  validInputs txbody u == Valid ?! BadInputsUTxO
  _ttl txbody >= _slot ?! ExpiredUTxO (_ttl txbody) _slot
  validNoReplay txbody == Valid ?! InputSetEmptyUTxO

  let validateFee = validFee pp txbody
  validateFee == Valid ?! unwrapFailureUTXO validateFee

  let validateBalance = preserveBalance stakePools stakeKeys pp txbody u
  validateBalance == Valid ?! unwrapFailureUTXO validateBalance

  let refunded = keyRefunds pp stakeKeys txbody
  let decayed  = decayedTx pp stakeKeys txbody
  let depositChange =
        deposits pp stakePools (txbody ^. certs) - (refunded + decayed)

  let u' = applyUTxOUpdate u txbody  -- change UTxO

  -- process Update Proposals
  ups' <- trans @(UP dsignAlgo) $ TRC ((_slot, _dms), u ^. ups, txup tx)

  pure
    $  u'
    &  deposited
    %~ (+) depositChange
    &  fees
    %~ (+) ((txbody ^. txfee) + decayed)
    &  ups
    .~ ups'

unwrapFailureUTXO :: Validity -> PredicateFailure (UTXO hashAlgo dsignAlgo)
unwrapFailureUTXO (Invalid [e]) = unwrapFailureUTXO' e
unwrapFailureUTXO Valid         = UnexpectedSuccessUTXO
unwrapFailureUTXO (Invalid x)   = UnexpectedFailureUTXO x

unwrapFailureUTXO'
  :: ValidationError
  -> PredicateFailure (UTXO hashAlgo dsignAlgo)
unwrapFailureUTXO' BadInputs                = BadInputsUTxO
unwrapFailureUTXO' (Expired s s')           = ExpiredUTxO s s'
unwrapFailureUTXO' InputSetEmpty            = InputSetEmptyUTxO
unwrapFailureUTXO' (FeeTooSmall       c c') = FeeTooSmallUTxO c c'
unwrapFailureUTXO' (ValueNotConserved c c') = ValueNotConservedUTxO c c'
unwrapFailureUTXO' x                        = UnexpectedFailureUTXO [x]

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (UP dsignAlgo) (UTXO hashAlgo dsignAlgo)
 where
  wrapFailed = UpdateFailure
