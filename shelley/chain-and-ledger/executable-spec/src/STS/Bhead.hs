{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Bhead
  ( BHEAD
  )
where

import qualified Data.Set                      as Set

import           BaseTypes
import           BlockChain
import           Keys
import           LedgerState
import           PParams
import           Slot

import           STS.NewEpoch
import           STS.Rupd

import           Control.State.Transition

data BHEAD

instance STS BHEAD where
  type State BHEAD = NewEpochState
  type Signal BHEAD = BHeader
  type Environment BHEAD = (Seed, Set.Set VKeyGenesis)
  data PredicateFailure BHEAD = HeaderSizeTooLargeBHEAD
                              | BlockSizeTooLargeBHEAD
                              | NewEpochFailure (PredicateFailure NEWEPOCH)
                              | RupdFailure (PredicateFailure RUPD)
                                deriving (Show, Eq)
  initialRules = []
  transitionRules = [bheadTransition]

bheadTransition :: TransitionRule BHEAD
bheadTransition = do
  TRC ((etaC, gkeys), nes@(NewEpochState _ _ bprev _ es ru _ _), bh@(BHeader bhb _)) <-
    judgmentContext
  let slot                = bheaderSlot bhb
  let EpochState _ _ _ pp = es

  fromIntegral (bHeaderSize bh) > _maxBHSize pp ?! HeaderSizeTooLargeBHEAD
  fromIntegral (hBbsize bhb) > _maxBBSize pp ?! BlockSizeTooLargeBHEAD

  nes' <- trans @NEWEPOCH
    $ TRC ((NewEpochEnv etaC slot gkeys), nes, epochFromSlot slot)

  ru' <- trans @RUPD $ TRC ((bprev, es), ru, slot)
  let nes'' = nes' { nesRu = ru' }
  pure nes''

instance Embed NEWEPOCH BHEAD where
  wrapFailed = NewEpochFailure

instance Embed RUPD BHEAD where
  wrapFailed = RupdFailure
