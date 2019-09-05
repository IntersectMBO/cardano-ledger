{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Up
  ( UP
  , UpdateEnv(..)
  )
where

import           Keys
import           PParams
import           Slot
import           Updates

import           Control.State.Transition

import           STS.Avup
import           STS.Ppup

data UP hashAlgo dsignAlgo

data UpdateEnv hashAlgo dsignAlgo
  = UpdateEnv Slot PParams (Dms hashAlgo dsignAlgo)

instance DSIGNAlgorithm dsignAlgo => STS (UP hashAlgo dsignAlgo) where
  type State (UP hashAlgo dsignAlgo) = UpdateState hashAlgo dsignAlgo
  type Signal (UP hashAlgo dsignAlgo) = Update hashAlgo dsignAlgo
  type Environment (UP hashAlgo dsignAlgo) = UpdateEnv hashAlgo dsignAlgo
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
  TRC (UpdateEnv _slot pp _dms, UpdateState pupS aupS favs avs, Update pup _aup) <- judgmentContext

  pup' <- trans @(PPUP hashAlgo dsignAlgo) $ TRC (PPUPEnv _slot pp _dms, pupS, pup)
  AVUPState aup' favs' avs' <-
    trans @(AVUP hashAlgo dsignAlgo) $ TRC (AVUPEnv _slot _dms, AVUPState aupS favs avs, _aup)

  pure $ UpdateState pup' aup' favs' avs'

instance DSIGNAlgorithm dsignAlgo => Embed (AVUP hashAlgo dsignAlgo) (UP hashAlgo dsignAlgo) where
  wrapFailed = AvupFailure

instance DSIGNAlgorithm dsignAlgo => Embed (PPUP hashAlgo dsignAlgo) (UP hashAlgo dsignAlgo) where
  wrapFailed = PpupFailure
