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
    = NonGenesisUpdatePPUP
    | PPUpdateTooLatePPUP
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [ppupTransition]

ppupTransition :: DSIGNAlgorithm dsignAlgo => TransitionRule (PPUP dsignAlgo)
ppupTransition = do
  TRC ((s, Dms _dms), pupS@(PPUpdate pupS'), pup@(PPUpdate pup')) <-
    judgmentContext
  if Map.null pupS'
    then pure pupS
    else do
      not (Map.keysSet pup' `Set.isSubsetOf` Map.keysSet _dms)
        ?! NonGenesisUpdatePPUP
      let Epoch slotEpoch = epochFromSlot (Slot 1)
      s
        <  (firstSlot (Epoch $ slotEpoch + 1) *- slotsPrior)
        ?! PPUpdateTooLatePPUP
      pure $ updatePPup pupS pup
