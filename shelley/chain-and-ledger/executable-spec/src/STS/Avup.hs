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

data AVUP hashAlgo dsignAlgo

instance STS (AVUP hashAlgo dsignAlgo) where
  type State (AVUP hashAlgo dsignAlgo)
    = (AVUpdate hashAlgo dsignAlgo, Map.Map Slot Applications, Applications)
  type Signal (AVUP hashAlgo dsignAlgo) = AVUpdate hashAlgo dsignAlgo
  type Environment (AVUP hashAlgo dsignAlgo) = (Slot, Dms hashAlgo dsignAlgo)
  data PredicateFailure (AVUP hashAlgo dsignAlgo)
    = NonGenesisUpdateAVUP
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [avupTransition]

avupTransition :: TransitionRule (AVUP hashAlgo dsignAlgo)
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
