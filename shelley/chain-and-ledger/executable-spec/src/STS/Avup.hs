{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Avup
  ( AVUP
  , AVUPState(..)
  , AVUPEnv(..)
  )
where

import qualified Data.Map.Strict as Map

import           BlockChain
import           Keys
import           Slot
import           Updates

import           Control.State.Transition
import           Data.Maybe
import           Ledger.Core (dom, range, (⊆), (⨃))

data AVUP hashAlgo dsignAlgo

data AVUPState hashAlgo dsignAlgo
  = AVUPState
      (AVUpdate hashAlgo dsignAlgo)
      (Map.Map Slot (Applications hashAlgo))
      (Applications hashAlgo)

data AVUPEnv hashAlgo dsignAlgo
  = AVUPEnv Slot (Dms hashAlgo dsignAlgo)

instance STS (AVUP hashAlgo dsignAlgo) where
  type State (AVUP hashAlgo dsignAlgo)
    = AVUPState hashAlgo dsignAlgo
  type Signal (AVUP hashAlgo dsignAlgo) = AVUpdate hashAlgo dsignAlgo
  type Environment (AVUP hashAlgo dsignAlgo) = AVUPEnv hashAlgo dsignAlgo
  data PredicateFailure (AVUP hashAlgo dsignAlgo)
    = EmptyAVUP
    | NonEmptyAVUP
    | NoAVConsensus
    | AVConsensus
    | NonGenesisUpdateAVUP
    | CannotFollow
    | InvalidName
    | InvalidSystemTags
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [avUpdateEmpty, avUpdateNoConsensus, avUpdateConsensus]

avUpdateEmpty :: TransitionRule (AVUP hashAlgo dsignAlgo)
avUpdateEmpty = do
  TRC (_, src, AVUpdate _aup) <-
    judgmentContext

  Map.null _aup ?! NonEmptyAVUP
  pure src

avUpdateNoConsensus :: TransitionRule (AVUP hashAlgo dsignAlgo)
avUpdateNoConsensus = do
  TRC (AVUPEnv _slot (Dms _dms), AVUPState (AVUpdate aupS) favs avs, AVUpdate _aup) <-
    judgmentContext

  not (Map.null _aup) ?! EmptyAVUP

  dom _aup ⊆ dom _dms ?! NonGenesisUpdateAVUP

  all allApNamesValid (range _aup) ?! InvalidName

  all (allSvCanFollow_ avs favs) (range _aup) ?! CannotFollow

  all allTagsValid (range _aup) ?! InvalidSystemTags

  let aup' = _aup ⨃ Map.toList aupS
  let fav  = votedValue aup'

  fav == Nothing ?! AVConsensus

  pure $ AVUPState (AVUpdate aup') favs avs

avUpdateConsensus :: TransitionRule (AVUP hashAlgo dsignAlgo)
avUpdateConsensus = do
  TRC (AVUPEnv _slot (Dms _dms), AVUPState (AVUpdate aupS) favs avs, AVUpdate _aup) <-
    judgmentContext

  not (Map.null _aup) ?! EmptyAVUP

  dom _aup ⊆ dom _dms ?! NonGenesisUpdateAVUP

  all allApNamesValid (range _aup) ?! InvalidName

  all (allSvCanFollow_ avs favs) (range _aup) ?! CannotFollow

  all allTagsValid (range _aup) ?! InvalidSystemTags

  let aup' = _aup ⨃ Map.toList aupS
  let fav  = votedValue aup'

  fav /= Nothing ?! NoAVConsensus
  let fav' = fromMaybe (Applications Map.empty) fav

  let s = _slot +* slotsPrior

  pure $ AVUPState
    (AVUpdate Map.empty)
    (favs ⨃ [(s, fav')])
    avs

allApNamesValid :: Applications hashAlgo -> Bool
allApNamesValid = all apNameValid . dom . apps

allSvCanFollow_ :: Applications hashAlgo -> Favs hashAlgo -> Applications hashAlgo -> Bool
allSvCanFollow_ avs favs = all (svCanFollow avs favs) . Map.toList . apps

allTagsValid :: Applications hashAlgo -> Bool
allTagsValid = all sTagsValid . range . range . apps
