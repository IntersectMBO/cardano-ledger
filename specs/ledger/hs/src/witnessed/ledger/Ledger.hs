{-# LANGUAGE OverloadedStrings #-}
module Ledger where

import qualified Data.Map.Strict   as Map
import           Data.Maybe        (isJust)
import qualified Data.Set          as Set
import           Extension
import           Ledger.Abstract
import qualified Ledger.Simple
import           Ledger.Transition
import           UTxO
import Data.List (find)

utxoInductive :: Transition
utxoInductive = Transition
  { _tPredicates =
     [ predInsufficientWitness
     , predValidInputs
     , predNoIncreasedBalance
     ]
  , _tUncheckedApply = \st sig -> case sig of
      NoSignal    -> st
      TxSignal (TxWits tx _) -> st
        { getUtxo = txins tx ⋪ (getUtxo st) ∪ txouts tx
        , peelState = (st, sig)
        }
  }
  where
    liftResult :: Ledger.Simple.PredicateResult -> PredicateResult
    liftResult Ledger.Simple.Passed = Passed
    liftResult (Ledger.Simple.Failed x) = Failed $ liftFailure x
    liftFailure :: Ledger.Simple.PredicateFailure -> PredicateFailure
    liftFailure = TxFailure

    predInsufficientWitness :: Predicate
    predInsufficientWitness = Predicate
      { _pName = "predInsuffcientWitness"
      , _pValidate = \st sig -> case sig of
          NoSignal -> Passed
          TxSignal tw -> witnessed tw st
      }
    predValidInputs :: Predicate
    predValidInputs = Predicate
      { _pName = "predValidInputs"
      , _pValidate = \st sig -> case sig of
          NoSignal -> Passed
          TxSignal (TxWits tx _) -> liftResult $ Ledger.Simple.validInputs tx (getUtxo st)
      }
    predNoIncreasedBalance :: Predicate
    predNoIncreasedBalance = Predicate
      { _pName = "predValidInputs"
      , _pValidate = \st sig -> case sig of
          NoSignal -> Passed
          TxSignal (TxWits tx _) -> liftResult $ Ledger.Simple.noIncreasedBalance tx (getUtxo st)
      }


-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) =
  case Map.lookup txin utxo of
    Just (TxOut (Addr pay) _) -> hash key == pay
    _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> State -> PredicateResult
witnessed (TxWits tx wits) l =
  if Set.size wits == Set.size ins && all (hasWitness wits) ins
    then Passed
    else Failed InsufficientWitnesses
  where
    utxo = getUtxo l
    ins = inputs tx
    hasWitness witnesses input =
      isJust $ find (isWitness tx input utxo) witnesses
    isWitness tx' input unspent (Wit key sig) =
      verify key tx' sig && authTxin key input unspent
