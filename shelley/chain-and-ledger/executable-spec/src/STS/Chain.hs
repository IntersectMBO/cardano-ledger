{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Chain
  ( CHAIN
  )
where

import qualified Data.Map.Strict               as Map

import           BaseTypes
import           BlockChain
import           LedgerState
import           Slot

import           Control.State.Transition

import           STS.Bbody
import           STS.Bhead
import           STS.Prtcl

data CHAIN

instance STS CHAIN where
  type State CHAIN = ( NewEpochState
                     , Seed
                     , Seed
                     , HashHeader
                     , Slot)
  type Signal CHAIN = Block
  type Environment CHAIN = Slot
  data PredicateFailure CHAIN = BbodyFailure (PredicateFailure BBODY)
                              | BheadFailure (PredicateFailure BHEAD)
                              | PrtclFailure (PredicateFailure PRTCL)
                                deriving (Show, Eq)
  initialRules = []
  transitionRules = [chainTransition]

chainTransition :: TransitionRule CHAIN
chainTransition = do
  TRC (sNow, (nes, etaV, etaC, h, sL), block@(Block bh _)) <- judgmentContext

  let gkeys = getGKeys nes
  nes' <- trans @BHEAD $ TRC ((etaC, gkeys), nes, bh)

  let NewEpochState _ eta0 _ bcur es _ _pd osched = nes'
  let EpochState _ _ ls pp                        = es
  let LedgerState _ (DPState (DState _ _ _ _ _dms) (PState _ _ _ cs)) _ = ls

  (cs', h', sL', etaV', etaC') <- trans @PRTCL
    $ TRC (((pp, osched, eta0, _pd, _dms), sNow), (cs, h, sL, etaV, etaC), bh)

  let ls' = setIssueNumbers ls cs'
  (ls'', bcur') <- trans @BBODY
    $ TRC ((Map.keysSet osched, pp), (ls', bcur), block)

  let nes'' = updateNES nes' bcur' ls''

  pure (nes'', etaV', etaC', h', sL')

instance Embed BBODY CHAIN where
  wrapFailed = BbodyFailure

instance Embed BHEAD CHAIN where
  wrapFailed = BheadFailure

instance Embed PRTCL CHAIN where
  wrapFailed = PrtclFailure
