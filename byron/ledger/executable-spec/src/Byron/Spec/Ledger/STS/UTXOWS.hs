{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transition system that models the application of multiple transactions to
-- the UTxO part of the ledger state.
module Byron.Spec.Ledger.STS.UTXOWS where

import Byron.Spec.Ledger.STS.UTXO (UTxOEnv, UTxOState)
import Byron.Spec.Ledger.STS.UTXOW (UTXOW)
import Byron.Spec.Ledger.UTxO (Tx)
import Control.State.Transition
  ( Embed,
    Environment,
    IRC (IRC),
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
    initialRules,
    judgmentContext,
    trans,
    transitionRules,
    wrapFailed,
  )
import Control.State.Transition.Generator (HasTrace, envGen, genTrace, sigGen)
import Control.State.Transition.Trace (TraceOrder (OldestFirst), traceSignals)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data UTXOWS deriving (Data, Typeable)

data UtxowsPredicateFailure
  = UtxowFailure (PredicateFailure UTXOW)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UTXOWS where
  type State UTXOWS = UTxOState
  type Signal UTXOWS = [Tx]
  type Environment UTXOWS = UTxOEnv
  type PredicateFailure UTXOWS = UtxowsPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @UTXOW $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxo, txWits) <- judgmentContext
        case (txWits :: [Tx]) of
          [] -> return utxo
          (tx : gamma) -> do
            utxo' <- trans @UTXOW $ TRC (env, utxo, tx)
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
