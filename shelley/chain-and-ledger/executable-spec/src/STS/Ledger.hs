{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Ledger
  ( LEDGER
  ) where

import qualified Data.Map.Strict as Map

import           Lens.Micro              ((^.))

import           Keys
import           LedgerState
import           UTxO
import           PParams hiding (d)
import           Slot
import           Delegation.Certificates

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

    initialRules    = [ initialLedgerStateLEDGER ]
    transitionRules = [ ledgerTransition         ]

initialLedgerStateLEDGER :: InitialRule LEDGER
initialLedgerStateLEDGER = do
  IRC (slot, ix, pp) <- judgmentContext
  utxo' <- trans @UTXOW
    $ IRC (slot, pp, StakeKeys Map.empty, StakePools Map.empty, Dms Map.empty)
  deleg <- trans @DELEGS $ IRC (slot, ix, pp)
  pure (utxo', deleg)

ledgerTransition :: TransitionRule LEDGER
ledgerTransition = do
  TRC ((slot, ix, pp), (u, d), txwits) <- judgmentContext
  utxo'  <- trans @UTXOW
    $ TRC (( slot
           , pp, d ^. dstate . stKeys
           , d ^. pstate . stPools
           , d ^. dstate . dms), u, txwits)
  deleg' <- trans @DELEGS $ TRC ((slot, ix, pp), d, txwits ^. body . certs)
  pure (utxo', deleg')

instance Embed DELEGS LEDGER where
  wrapFailed = DelegsFailure

instance Embed UTXOW LEDGER where
  wrapFailed = UtxowFailure
