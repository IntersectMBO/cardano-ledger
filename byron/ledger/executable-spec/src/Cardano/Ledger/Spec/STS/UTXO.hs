{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | UTXO transition system
module Cardano.Ledger.Spec.STS.UTXO where

import qualified Data.Set as Set

import Control.State.Transition
  ( Environment
  , IRC(IRC)
  , PredicateFailure
  , STS
  , Signal
  , State
  , TRC(TRC)
  , initialRules
  , transitionRules
  , (?!)
  , judgmentContext
  )
import Data.AbstractSize (HasTypeReps)

import Ledger.Core (Lovelace, (∪), (⊆), (⋪), (◁), dom, range)
import Ledger.GlobalParams (lovelaceCap)
import Ledger.Update (PParams)
import Ledger.UTxO (Tx, UTxO, balance, pcMinFee, txins, txouts, value)

data UTXO id

data UTxOEnv id = UTxOEnv { utxo0 :: UTxO id
                          , pps   :: PParams
                          } deriving (Eq, Show)

data UTxOState id = UTxOState { utxo :: UTxO id
                              , reserves :: Lovelace
                              }
  deriving (Eq, Show)

instance (Ord id, HasTypeReps id) => STS (UTXO id) where

  type Environment (UTXO id) = UTxOEnv id
  type State (UTXO id) = UTxOState id
  type Signal (UTXO id) = Tx id
  data PredicateFailure (UTXO id)
    = EmptyTxInputs
    | FeeTooLow
    | IncreasedTotalBalance
    | InputsNotInUTxO
    | NonPositiveOutputs
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC UTxOEnv {utxo0} <- judgmentContext
        return $ UTxOState { utxo     = utxo0
                           , reserves = lovelaceCap - balance utxo0
                           }
    ]
  transitionRules =
    [ do
        TRC ( UTxOEnv _ pps
            , UTxOState {utxo, reserves}
            , tx
            ) <- judgmentContext

        txins tx ⊆ dom utxo ?! InputsNotInUTxO

        let fee = balance (txins tx ◁ utxo) - balance (txouts tx)

        pcMinFee pps tx <= fee ?! FeeTooLow

        (not . null) (txins tx) ?! EmptyTxInputs

        let
          outputValues = fmap value $ Set.toList $ range (txouts tx)
        all (0<) outputValues ?! NonPositiveOutputs

        return $ UTxOState { utxo     = (txins tx ⋪ utxo) ∪ txouts tx
                           , reserves = reserves + fee
                           }

    ]
