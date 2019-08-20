{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Avup
  ( AVUP
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

instance STS (AVUP hashAlgo dsignAlgo) where
  type State (AVUP hashAlgo dsignAlgo)
    = (AVUpdate hashAlgo dsignAlgo, Map.Map Slot Applications, Applications)
  type Signal (AVUP hashAlgo dsignAlgo) = AVUpdate hashAlgo dsignAlgo
  type Environment (AVUP hashAlgo dsignAlgo) = (Slot, Dms hashAlgo dsignAlgo)
  data PredicateFailure (AVUP hashAlgo dsignAlgo)
    = EmptyAVUP
    | NonEmptyAVUP
    | NoAVConsensus
    | AVConsensus
    | NonGenesisUpdateAVUP
    | CannotFollow
    | InvalidName
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
  TRC ((_slot, Dms _dms), (AVUpdate aupS, favs, avs), AVUpdate _aup) <-
    judgmentContext

  not (Map.null _aup) ?! EmptyAVUP

  dom _aup ⊆ dom _dms ?! NonGenesisUpdateAVUP

  all allApNamesValid (range _aup) ?! InvalidName

  all (allSvCanFollow_ avs favs) (range _aup) ?! CannotFollow

  -- TODO - do we need system tags? if so, check them here

  let aup' = _aup ⨃ Map.toList aupS
  let fav  = votedValue aup'

  fav == Nothing ?! AVConsensus

  pure (AVUpdate aup', favs, avs)

avUpdateConsensus :: TransitionRule (AVUP hashAlgo dsignAlgo)
avUpdateConsensus = do
  TRC ((_slot, Dms _dms), (AVUpdate aupS, favs, avs), AVUpdate _aup) <-
    judgmentContext

  not (Map.null _aup) ?! EmptyAVUP

  dom _aup ⊆ dom _dms ?! NonGenesisUpdateAVUP

  all allApNamesValid (range _aup) ?! InvalidName

  all (allSvCanFollow_ avs favs) (range _aup) ?! CannotFollow

  -- TODO - do we need system tags? if so, check them here

  let aup' = _aup ⨃ Map.toList aupS
  let fav  = votedValue aup'

  fav /= Nothing ?! NoAVConsensus
  let fav' = fromMaybe (Applications Map.empty) fav

  let s = _slot +* slotsPrior

  pure
    ( AVUpdate Map.empty
    , favs ⨃ [(s, fav')]
    , avs
    )

allApNamesValid :: Applications -> Bool
allApNamesValid = all apNameValid . dom . apps

allSvCanFollow_ :: Applications -> Favs -> Applications -> Bool
allSvCanFollow_ avs favs = all (svCanFollow avs favs) . Map.toList . apps
