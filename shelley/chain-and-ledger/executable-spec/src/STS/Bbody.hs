{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Bbody
  ( BBODY
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           BlockChain
import           EpochBoundary
import           Keys
import           LedgerState
import           PParams
import           Slot

import           Control.State.Transition

import           STS.Ledgers

data BBODY

instance STS BBODY where
  type State BBODY = (LedgerState, BlocksMade)
  type Signal BBODY = Block
  type Environment BBODY = (Set.Set Slot, PParams)
  data PredicateFailure BBODY = WrongBlockBodySizeBBODY
                            | LedgersFailure (PredicateFailure LEDGERS)
                                deriving (Show, Eq)
  initialRules = [pure (emptyLedgerState, BlocksMade Map.empty)]
  transitionRules = [bbodyTransition]

bbodyTransition :: TransitionRule BBODY
bbodyTransition = do
  TRC ((oslots, pp), (ls, b), (Block (BHeader bhb _) txs)) <- judgmentContext
  let hk = hashKey $ bvkcold bhb
  bBodySize txs == (fromIntegral $ hBbsize bhb) ?! WrongBlockBodySizeBBODY

  ls' <- trans @LEDGERS $ TRC ((bheaderSlot bhb, pp), ls, txs)

  let b' = incrBlocks (Set.member (bheaderSlot bhb) oslots) hk b

  pure (ls', b')

instance Embed LEDGERS BBODY where
  wrapFailed = LedgersFailure
