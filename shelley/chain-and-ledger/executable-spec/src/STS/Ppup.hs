{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Ppup
  ( PPUP
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           BlockChain
import           Keys
import           Slot
import           Updates

import           Control.State.Transition

data PPUP dsignAlgo

instance DSIGNAlgorithm dsignAlgo => STS (PPUP dsignAlgo) where
  type State (PPUP dsignAlgo) = PPUpdate dsignAlgo
  type Signal (PPUP dsignAlgo) = PPUpdate dsignAlgo
  type Environment (PPUP dsignAlgo) = (Slot, Dms dsignAlgo)
  data PredicateFailure (PPUP dsignAlgo)
    = NonGenesisUpdatePPUP (Set.Set (VKeyGenesis dsignAlgo)) (Set.Set (VKeyGenesis dsignAlgo))
    | PPUpdateTooEarlyPPUP
    | PPUpdateNonEmpty
    | PVCannotFollowPPUP
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [ppupTransitionEmpty, ppupTransitionNonEmpty]

ppupTransitionEmpty :: TransitionRule (PPUP dsignAlgo)
ppupTransitionEmpty = do
  TRC ((_, _), pupS, PPUpdate pup') <-
    judgmentContext
  do
    Map.null pup' ?! PPUpdateNonEmpty
    pure pupS

ppupTransitionNonEmpty :: DSIGNAlgorithm dsignAlgo => TransitionRule (PPUP dsignAlgo)
ppupTransitionNonEmpty = do
  TRC ((s, Dms _dms), pupS, pup@(PPUpdate pup')) <-
    judgmentContext
  do
    (Map.keysSet pup' `Set.isSubsetOf` Map.keysSet _dms)
      ?! NonGenesisUpdatePPUP (Map.keysSet pup') (Map.keysSet _dms)
    let Epoch slotEpoch = epochFromSlot (Slot 1)
    s
      <  (firstSlot (Epoch $ slotEpoch + 1) *- slotsPrior)
      ?! PPUpdateTooEarlyPPUP
    pure $ updatePPup pupS pup
