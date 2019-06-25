{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Ledger
  ( LEDGER
  )
where

import           Lens.Micro ((^.))

import           LedgerState
import           PParams hiding (d)
import           Slot
import           UTxO

import           Control.State.Transition

import           STS.Delegs
import           STS.Utxow

data LEDGER

instance STS LEDGER where
    type State LEDGER       = (UTxOState, DPState)
    type Signal LEDGER      = Tx
    type Environment LEDGER = (Slot, Ix, PParams)
    data PredicateFailure LEDGER = UtxowFailure (PredicateFailure UTXOW)
                                 | DelegsFailure (PredicateFailure DELEGS)
                    deriving (Show, Eq)

    initialRules    = [ ]
    transitionRules = [ ledgerTransition         ]

ledgerTransition :: TransitionRule LEDGER
ledgerTransition = do
  TRC ((slot, ix, pp), (u, d), tx) <- judgmentContext
  utxo' <- trans @UTXOW $ TRC
    ( (slot, pp, d ^. dstate . stKeys, d ^. pstate . stPools, d ^. dstate . dms)
    , u
    , tx
    )
  deleg' <- trans @DELEGS $ TRC ((slot, ix, pp, tx), d, tx ^. body . certs)
  pure (utxo', deleg')

instance Embed DELEGS LEDGER where
  wrapFailed = DelegsFailure

instance Embed UTXOW LEDGER where
  wrapFailed = UtxowFailure
