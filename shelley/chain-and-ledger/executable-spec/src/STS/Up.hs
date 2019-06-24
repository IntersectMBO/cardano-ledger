{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Up
  ( UP
  )
where

import qualified Data.Map.Strict               as Map

import           Keys
import           Slot
import           Updates

import           Control.State.Transition

import           STS.Avup
import           STS.Ppup

data UP

instance STS UP where
  type State UP = (PPUpdate, AVUpdate, Map.Map Slot Applications, Applications)
  type Signal UP = Update
  type Environment UP = (Slot, Dms)
  data PredicateFailure UP = NonGenesisUpdateUP
                           | AvupFailure (PredicateFailure AVUP)
                           | PpupFailure (PredicateFailure PPUP)
                             deriving (Show, Eq)
  initialRules = []

  transitionRules = [upTransition]

upTransition :: TransitionRule UP
upTransition = do
  TRC (env, (pupS, aupS, favs, avs), Update pup _aup) <- judgmentContext

  pup'                <- trans @PPUP $ TRC (env, pupS, pup)
  (aup', favs', avs') <- trans @AVUP $ TRC (env, (aupS, favs, avs), _aup)

  pure (pup', aup', favs', avs')

instance Embed AVUP UP where
  wrapFailed = AvupFailure

instance Embed PPUP UP where
  wrapFailed = PpupFailure
