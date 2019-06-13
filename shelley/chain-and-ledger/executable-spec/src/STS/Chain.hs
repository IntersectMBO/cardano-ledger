{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Chain
  ( CHAIN
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set

import           BaseTypes
import           BlockChain
import           EpochBoundary
import           Keys
import           LedgerState
import           PParams
import           Slot

import           Delegation.Certificates

import           Control.State.Transition

import           STS.Bbody
import           STS.NewEpoch
import           STS.Ocert
import           STS.Rupd
import           STS.Updn

data CHAIN

instance STS CHAIN where
  type State CHAIN = ( (Seed, Seed, Seed)
                     , BlocksMade
                     , Slot
                     , Epoch
                     , EpochState
                     , Maybe RewardUpdate
                     , PoolDistr)
  type Signal CHAIN = Block
  type Environment CHAIN = (Slot, PParams)
  data PredicateFailure CHAIN = BbodyFailure (PredicateFailure BBODY)
                            | RupdFailure (PredicateFailure RUPD)
                            | OcertFailure (PredicateFailure OCERT)
                            | NewepochFailure (PredicateFailure NEWEPOCH)
                            | UpdnFailure (PredicateFailure UPDN)
                                deriving (Show, Eq)
  initialRules =
    [ pure $
      ( (mkNonce 0, mkNonce 1, mkNonce 2)
      , BlocksMade Map.empty
      , Slot 0
      , Epoch 0
      , emptyEpochState
      , Nothing
      , PoolDistr Map.empty)
    ]
  transitionRules = [chainTransition]

chainTransition :: TransitionRule CHAIN
chainTransition = do
  TRC ((_, ppN), ((eta0, etaC, etaV), b, sL, eL, es, ru, pd), block@(Block bh _)) <-
    judgmentContext
  let bhb = bhbody bh
  let s = bheaderSlot bhb
  let e = epochFromSlot s -- TODO where is Epoch `e` coming from?
  (etaV', etaC') <- trans @UPDN $ TRC (eta0, (etaV, etaC), s)
  -- (eL', eta0', b', es'@(EpochState acnt' ls' ss' pp'), ru', pd') <-
  --   trans @NEWEPOCH $ TRC (undefined, undefined, e) -- TODO:
  -- TODO add PRTCL rule
  let ls' = undefined
  let pp' = undefined
  let es' = undefined
  let ru' = undefined
  let b'  = undefined
  let acnt' = undefined
  let ss' = undefined
  let eta0' = undefined
  let eL' = undefined
  let pd' = undefined
  ru'' <- trans @RUPD $ TRC ((b, es'), ru', s)
  let delegationState' = _delegationState ls'
  _ <- trans @OCERT $ TRC ((), Map.empty, bh) -- TODO: OVERLAY -> PRTCL
  let ls'' = ls' -- {_delegationState = delegationState' {_pstate = ps''}}
  let sL' = sL
  -- TODO after removal of Vrf, add PRTCL to get sL'
  (ls''', b'') <- trans @BBODY $ TRC (pp', (ls'', b'), block)
  let es'' = EpochState acnt' ss' ls''' pp'
  pure $ ((eta0', etaC', etaV'), b'', sL', eL', es'', ru'', pd')

instance Embed BBODY CHAIN where
  wrapFailed = BbodyFailure

instance Embed OCERT CHAIN where
  wrapFailed = OcertFailure

instance Embed NEWEPOCH CHAIN where
  wrapFailed = NewepochFailure

instance Embed UPDN CHAIN where
  wrapFailed = UpdnFailure

instance Embed RUPD CHAIN where
  wrapFailed = RupdFailure
