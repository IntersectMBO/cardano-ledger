{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Up
  ( UP
  )
where

import qualified Data.Map.Strict as Map

import           Keys
import           PParams
import           Slot
import           Updates

import           Control.State.Transition

import           STS.Avup
import           STS.Ppup

data UP dsignAlgo

instance DSIGNAlgorithm dsignAlgo => STS (UP dsignAlgo) where
  type State (UP dsignAlgo)
    = ( PPUpdate dsignAlgo
      , AVUpdate dsignAlgo
      , Map.Map Slot Applications
      , Applications
      )
  type Signal (UP dsignAlgo) = Update dsignAlgo
  type Environment (UP dsignAlgo) = (Slot, PParams, Dms dsignAlgo)
  data PredicateFailure (UP dsignAlgo)
    = NonGenesisUpdateUP
    | AvupFailure (PredicateFailure (AVUP dsignAlgo))
    | PpupFailure (PredicateFailure (PPUP dsignAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [upTransition]

upTransition
  :: forall dsignAlgo
   . DSIGNAlgorithm dsignAlgo
  => TransitionRule (UP dsignAlgo)
upTransition = do
  TRC ((_slot, pp, _dms), (pupS, aupS, favs, avs), Update pup _aup) <- judgmentContext

  pup' <- trans @(PPUP dsignAlgo) $ TRC ((_slot, pp, _dms), pupS, pup)
  (aup', favs', avs') <-
    trans @(AVUP dsignAlgo) $ TRC ((_slot, _dms), (aupS, favs, avs), _aup)

  pure (pup', aup', favs', avs')

instance DSIGNAlgorithm dsignAlgo => Embed (AVUP dsignAlgo) (UP dsignAlgo) where
  wrapFailed = AvupFailure

instance DSIGNAlgorithm dsignAlgo => Embed (PPUP dsignAlgo) (UP dsignAlgo) where
  wrapFailed = PpupFailure
