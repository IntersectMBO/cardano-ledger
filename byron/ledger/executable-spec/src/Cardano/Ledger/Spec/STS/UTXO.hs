{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


-- | UTXO transition system
module Cardano.Ledger.Spec.STS.UTXO
  ( UTXO
  , UTxOEnv (UTxOEnv)
  , UTxOState (UTxOState)
  , PredicateFailure(..)
  , utxo
  , utxo0
  , pps
  , reserves
  )
where

import           Data.Data (Data, Typeable)
import qualified Data.Set as Set

import           Control.State.Transition (Environment, IRC (IRC), PredicateFailure, STS, Signal,
                     State, TRC (TRC), initialRules, judgmentContext, transitionRules, (?!))
import           Ledger.Core (Lovelace, dom, range, (∪), (⊆), (⋪), (◁))
import           Ledger.GlobalParams (lovelaceCap)
import           Ledger.Update (PParams)
import           Ledger.UTxO (Tx, UTxO, balance, pcMinFee, txins, txouts, unUTxO, value)

import           Test.Goblin (SeedGoblin (..))
import           Test.Goblin.TH (deriveSeedGoblin)


data UTXO deriving (Data, Typeable)


data UTxOEnv = UTxOEnv
  { utxo0 :: UTxO
  , pps   :: PParams
  } deriving (Eq, Show)

data UTxOState = UTxOState
  { utxo     :: UTxO
  , reserves :: Lovelace
  } deriving (Eq, Show)

instance STS UTXO where

  type Environment UTXO = UTxOEnv
  type State UTXO = UTxOState
  type Signal UTXO = Tx

  -- | These `PredicateFailure`s are all "throwable". The disjunction of the
  --   rules' preconditions is not `True` - the `PredicateFailure`s represent
  --   `False` cases.
  data PredicateFailure UTXO
    = EmptyTxInputs
    | EmptyTxOutputs
    | FeeTooLow
    | IncreasedTotalBalance
    | InputsNotInUTxO
    | NonPositiveOutputs
    deriving (Eq, Show, Data, Typeable)

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

        (not . null . unUTxO) (txouts tx) ?! EmptyTxOutputs

        let
          outputValues = fmap value $ Set.toList $ range (txouts tx)
        all (0<) outputValues ?! NonPositiveOutputs

        return $ UTxOState { utxo     = (txins tx ⋪ utxo) ∪ txouts tx
                           , reserves = reserves + fee
                           }

    ]


--------------------------------------------------------------------------------
-- SeedGoblin instances
--------------------------------------------------------------------------------

deriveSeedGoblin ''UTxOEnv
deriveSeedGoblin ''UTxOState
