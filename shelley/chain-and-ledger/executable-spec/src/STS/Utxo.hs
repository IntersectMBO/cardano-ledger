{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Utxo
  ( UTXO
  , UtxoEnv (..)
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
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           STS.Up

import           Hedgehog (Gen)

data UTXO hashAlgo dsignAlgo vrfAlgo

data UtxoEnv hashAlgo dsignAlgo
  = UtxoEnv
      Slot
      PParams
      (StakeCreds hashAlgo dsignAlgo)
      (StakePools hashAlgo dsignAlgo)
      (GenDelegs hashAlgo dsignAlgo)
      deriving(Show)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => STS (UTXO hashAlgo dsignAlgo vrfAlgo)
 where
  type State (UTXO hashAlgo dsignAlgo vrfAlgo) = UTxOState hashAlgo dsignAlgo vrfAlgo
  type Signal (UTXO hashAlgo dsignAlgo vrfAlgo) = Tx hashAlgo dsignAlgo vrfAlgo
  type Environment (UTXO hashAlgo dsignAlgo vrfAlgo) = UtxoEnv hashAlgo dsignAlgo
  data PredicateFailure (UTXO hashAlgo dsignAlgo vrfAlgo)
    = BadInputsUTxO
    | ExpiredUTxO Slot Slot
    | MaxTxSizeUTxO Integer Integer
    | InputSetEmptyUTxO
    | FeeTooSmallUTxO Coin Coin
    | ValueNotConservedUTxO Coin Coin
    | NegativeOutputsUTxO
    | UnexpectedFailureUTXO [ValidationError] -- TODO maybe restructure Validity
                                              -- to prevent these predicate
                                              -- failures?
    | UnexpectedSuccessUTXO
    | UpdateFailure (PredicateFailure (UP hashAlgo dsignAlgo))
    deriving (Eq, Show)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

initialLedgerState :: InitialRule (UTXO hashAlgo dsignAlgo vrfAlgo)
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

utxoInductive
  :: forall hashAlgo dsignAlgo vrfAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => TransitionRule (UTXO hashAlgo dsignAlgo vrfAlgo)
utxoInductive = do
  TRC (UtxoEnv slot_ pp stakeKeys stakePools genDelegs_, u, tx) <- judgmentContext
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
  ups' <- trans @(UP hashAlgo dsignAlgo) $ TRC (UpdateEnv slot_ pp genDelegs_, u ^. ups, txup tx)

  let outputCoins = [c | (TxOut _ c) <- Set.toList (range (txouts txBody))]
  all (0 <=) outputCoins ?! NegativeOutputsUTxO

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize txBody
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp stakeKeys txBody
      decayed = decayedTx pp stakeKeys txBody
      txCerts = toList $ txBody ^. certs

      depositChange = deposits pp stakePools txCerts - (refunded + decayed)

  pure UTxOState
        { _utxo      = (txins txBody ⋪ (u ^. utxo)) ∪ txouts txBody
        , _deposited = _deposited u + depositChange
        , _fees      = _fees u + (txBody ^. txfee) + decayed
        , _ups       = ups'
        }

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => Embed (UP hashAlgo dsignAlgo) (UTXO hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = UpdateFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  )
  => HasTrace (UTXO hashAlgo dsignAlgo vrfAlgo) where
  envGen _ = undefined :: Gen (UtxoEnv hashAlgo dsignAlgo)
  sigGen _ _ = undefined :: Gen (Tx hashAlgo dsignAlgo vrfAlgo)
