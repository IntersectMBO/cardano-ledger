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

import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Lens.Micro ((^.))

import           Coin
import           Delegation.Certificates
import           Keys
import           Ledger.Core (dom, range, (∪), (⊆), (⋪))
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
    | MaxTxSizeUTxO Integer Integer
    | InputSetEmptyUTxO
    | FeeTooSmallUTxO Coin Coin
    | ValueNotConservedUTxO Coin Coin
    | NonPositiveOutputsUTxO
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
  TRC ((slot_, pp, stakeKeys, stakePools, dms_), u, tx) <- judgmentContext
  let txBody = _body tx

  _ttl txBody >= slot_ ?! ExpiredUTxO (_ttl txBody) slot_

  txins txBody /= Set.empty ?! InputSetEmptyUTxO

  let minFee = minfee pp txBody
      txFee  = txBody ^. txfee
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  txins txBody ⊆ dom (u ^. utxo) ?! BadInputsUTxO

  let consumed_ = consumed pp (u ^. utxo) stakeKeys txBody
      produced_ = produced pp stakePools txBody
  consumed_ == produced_ ?! ValueNotConservedUTxO consumed_ produced_

  -- process Update Proposals
  ups' <- trans @(UP dsignAlgo) $ TRC ((slot_, dms_), u ^. ups, txup tx)

  let outputCoins = [c | (TxOut _ c) <- Set.toList (range (txouts txBody))]
  all (0<) outputCoins ?! NonPositiveOutputsUTxO

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize txBody
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp stakeKeys txBody
      decayed = decayedTx pp stakeKeys txBody
      txCerts = toList $ txBody ^. certs

      depositChange = deposits pp stakePools txCerts - (refunded + decayed)

  pure $ UTxOState
          { _utxo      = (txins txBody ⋪ (u ^. utxo)) ∪ txouts txBody
          , _deposited = _deposited u + depositChange
          , _fees      = _fees u + (txBody ^. txfee) + decayed
          , _ups       = ups'
          }

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (UP dsignAlgo) (UTXO hashAlgo dsignAlgo)
 where
  wrapFailed = UpdateFailure
