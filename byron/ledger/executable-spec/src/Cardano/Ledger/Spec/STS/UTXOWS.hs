{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transition system that models the application of multiple transactions to
-- the UTxO part of the ledger state.

module Cardano.Ledger.Spec.STS.UTXOWS where

import           Data.Data (Data, Typeable)

import           Cardano.Ledger.Spec.STS.UTXO (UTxOEnv, UTxOState)
import           Cardano.Ledger.Spec.STS.UTXOW (UTXOW)
import           Control.State.Transition (Embed, Environment, IRC (IRC), PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext, trans,
                     transitionRules, wrapFailed)
import           Control.State.Transition.Generator (HasTrace, envGen, genTrace, sigGen)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst), traceSignals)
import           Ledger.UTxO (TxWits)

data UTXOWS deriving (Data, Typeable)

instance STS UTXOWS where
  type State UTXOWS = UTxOState
  type Signal UTXOWS = [TxWits]
  type Environment UTXOWS = UTxOEnv
  data PredicateFailure UTXOWS
    = UtxowFailure (PredicateFailure UTXOW)
    deriving (Eq, Show, Data, Typeable)

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @UTXOW $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxo, txWits) <- judgmentContext
        case (txWits :: [TxWits]) of
          []     -> return utxo
          (tx:gamma) -> do
            utxo'  <- trans @UTXOW  $ TRC (env, utxo, tx)
            utxo'' <- trans @UTXOWS $ TRC (env, utxo', gamma)
            return utxo''
    ]

instance Embed UTXOW UTXOWS where
  wrapFailed = UtxowFailure

instance HasTrace UTXOWS where
  envGen = envGen @UTXOW

  -- We generate signal for UTXOWS as a list of signals from UTXOW
  sigGen env st =
    traceSignals OldestFirst <$> genTrace @UTXOW () 20 env st (sigGen @UTXOW)
