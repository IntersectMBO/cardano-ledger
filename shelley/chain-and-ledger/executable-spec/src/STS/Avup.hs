{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Avup
  ( AVUP
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           BlockChain
import           Keys
import           Slot
import           Updates

import           Control.State.Transition

data AVUP dsignAlgo

instance DSIGNAlgorithm dsignAlgo => STS (AVUP dsignAlgo) where
  type State (AVUP dsignAlgo)
    = (AVUpdate dsignAlgo, Map.Map Slot Applications, Applications)
  type Signal (AVUP dsignAlgo) = AVUpdate dsignAlgo
  type Environment (AVUP dsignAlgo) = (Slot, Dms dsignAlgo)
  data PredicateFailure (AVUP dsignAlgo)
    = NonGenesisUpdateAVUP
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [avupTransition]

avupTransition :: DSIGNAlgorithm dsignAlgo => TransitionRule (AVUP dsignAlgo)
avupTransition = do
  TRC ((_slot, Dms _dms), src@(AVUpdate aupS, favs, avs), AVUpdate _aup) <-
    judgmentContext

  if Map.null _aup
    then pure src
    else do
      Map.keysSet _aup `Set.isSubsetOf` Map.keysSet _dms ?! NonGenesisUpdateAVUP

      let aup'         = Map.union _aup aupS
      let fav          = votedValue aup'
      case fav of
        Nothing -> pure (AVUpdate aup', favs, avs)
        Just fav' ->
          pure
            ( AVUpdate Map.empty
            , Map.insert (_slot +* slotsPrior) fav' favs
            , avs
            )
