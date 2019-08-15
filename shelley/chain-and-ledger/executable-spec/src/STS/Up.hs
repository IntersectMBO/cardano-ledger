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

data UP hashAlgo dsignAlgo

instance DSIGNAlgorithm dsignAlgo => STS (UP hashAlgo dsignAlgo) where
  type State (UP hashAlgo dsignAlgo)
    = ( PPUpdate hashAlgo dsignAlgo
      , AVUpdate hashAlgo dsignAlgo
      , Map.Map Slot Applications
      , Applications
      )
  type Signal (UP hashAlgo dsignAlgo) = Update hashAlgo dsignAlgo
  type Environment (UP hashAlgo dsignAlgo) = (Slot, PParams, Dms hashAlgo dsignAlgo)
  data PredicateFailure (UP hashAlgo dsignAlgo)
    = NonGenesisUpdateUP
    | AvupFailure (PredicateFailure (AVUP hashAlgo dsignAlgo))
    | PpupFailure (PredicateFailure (PPUP hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [upTransition]

upTransition
  :: forall hashAlgo dsignAlgo
   . DSIGNAlgorithm dsignAlgo
  => TransitionRule (UP hashAlgo dsignAlgo)
upTransition = do
  TRC ((_slot, pp, _dms), (pupS, aupS, favs, avs), Update pup _aup) <- judgmentContext

  pup' <- trans @(PPUP hashAlgo dsignAlgo) $ TRC ((_slot, pp, _dms), pupS, pup)
  (aup', favs', avs') <-
    trans @(AVUP hashAlgo dsignAlgo) $ TRC ((_slot, _dms), (aupS, favs, avs), _aup)

  pure (pup', aup', favs', avs')

instance DSIGNAlgorithm dsignAlgo => Embed (AVUP hashAlgo dsignAlgo) (UP hashAlgo dsignAlgo) where
  wrapFailed = AvupFailure

instance DSIGNAlgorithm dsignAlgo => Embed (PPUP hashAlgo dsignAlgo) (UP hashAlgo dsignAlgo) where
  wrapFailed = PpupFailure
