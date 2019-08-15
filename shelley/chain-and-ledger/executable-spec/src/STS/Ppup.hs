{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Ppup
  ( PPUP
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           BaseTypes
import           BlockChain
import           Keys
import           Ledger.Core (dom, (⊆))
import           PParams
import           Slot
import           Updates

import           Control.State.Transition
import           Data.Ix (inRange)
import           Numeric.Natural (Natural)

data PPUP dsignAlgo

instance DSIGNAlgorithm dsignAlgo => STS (PPUP dsignAlgo) where
  type State (PPUP dsignAlgo) = PPUpdate dsignAlgo
  type Signal (PPUP dsignAlgo) = PPUpdate dsignAlgo
  type Environment (PPUP dsignAlgo) = (Slot, PParams, Dms dsignAlgo)
  data PredicateFailure (PPUP dsignAlgo)
    = NonGenesisUpdatePPUP (Set.Set (VKeyGenesis dsignAlgo)) (Set.Set (VKeyGenesis dsignAlgo))
    | PPUpdateTooEarlyPPUP
    | PPUpdateEmpty
    | PPUpdateNonEmpty
    | PVCannotFollowPPUP
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [ppupTransitionEmpty, ppupTransitionNonEmpty]

pvCanFollow :: (Natural, Natural, Natural) -> Ppm -> Bool
pvCanFollow (mjp, mip, ap) (ProtocolVersion (mjn, mn, an))
  = (mjp, mip, ap) < (mjn, mn, an)
  && inRange (0,1) (mjn - mjp)
  && ((mjp == mjn) ==> (mip + 1 == mn))
  && ((mjp + 1 == mjn) ==> (mn == 0))
pvCanFollow _ _ = True

ppupTransitionEmpty :: TransitionRule (PPUP dsignAlgo)
ppupTransitionEmpty = do
  TRC ((_, _, _), pupS, PPUpdate pup') <- judgmentContext

  Map.null pup' ?! PPUpdateNonEmpty

  pure pupS

ppupTransitionNonEmpty :: DSIGNAlgorithm dsignAlgo => TransitionRule (PPUP dsignAlgo)
ppupTransitionNonEmpty = do
  TRC ((s, pp, Dms _dms), pupS, pup@(PPUpdate pup')) <- judgmentContext

  pup' /= Map.empty ?! PPUpdateEmpty

  all (all (pvCanFollow (_protocolVersion pp))) pup' ?! PVCannotFollowPPUP

  (dom pup' ⊆ dom _dms) ?! NonGenesisUpdatePPUP (dom pup') (dom _dms)

  let Epoch slotEpoch = epochFromSlot (Slot 1)
  s
    <  (firstSlot (Epoch $ slotEpoch + 1) *- slotsPrior)
    ?! PPUpdateTooEarlyPPUP

  pure $ updatePPup pupS pup
