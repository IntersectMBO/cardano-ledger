{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Ledger
  ( LEDGER
  ) where

import qualified Data.Map.Strict as Map

import           Lens.Micro              ((^.))

import           LedgerState
import           UTxO
import           PParams
import           Slot
import           Delegation.Certificates

import           Control.State.Transition

import           STS.Delegs
import           STS.Utxow

data LEDGER

instance STS LEDGER where
    type State LEDGER       = (UTxOState, DWState)
    type Signal LEDGER      = TxWits
    type Environment LEDGER = (PParams, Slot, Ix)
    data PredicateFailure LEDGER = UtxowFailure (PredicateFailure UTXOW)
                                 | DelegsFailure (PredicateFailure DELEGS)
                    deriving (Show, Eq)

    initialRules    = [ initialLedgerStateLEDGER ]
    transitionRules = [ ledgerTransition         ]

initialLedgerStateLEDGER :: InitialRule LEDGER
initialLedgerStateLEDGER = do
  IRC (pp, slot, ix) <- judgmentContext
  utxo' <- trans @UTXOW  $ IRC (slot, pp, StakeKeys Map.empty, StakePools Map.empty)
  deleg <- trans @DELEGS $ IRC (slot, ix, pp)
  pure (utxo', deleg)

ledgerTransition :: TransitionRule LEDGER
ledgerTransition = do
  TRC ((pp, slot, ix), (u, d), txwits) <- judgmentContext
  utxo'  <- trans @UTXOW  $ TRC ((slot, pp, d ^. dstate . stKeys, d ^. pstate . stPools), u, txwits)
  deleg' <- trans @DELEGS $ TRC ((slot, ix, pp), d, txwits ^. body . certs)
  pure (utxo', deleg')

instance Embed DELEGS LEDGER where
  wrapFailed = DelegsFailure

instance Embed UTXOW LEDGER where
  wrapFailed = UtxowFailure
