{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Avup
  ( AVUP
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           BlockChain
import           Keys
import           Slot
import           Updates

import           Control.State.Transition

data AVUP

instance STS AVUP where
  type State AVUP = (AVUpdate, Map.Map Slot Applications, Applications)
  type Signal AVUP = AVUpdate
  type Environment AVUP = (Slot, Dms)
  data PredicateFailure AVUP = NonGenesisUpdateAVUP

                             deriving (Show, Eq)
  initialRules = []

  transitionRules = [avupTransition]

avupTransition :: TransitionRule AVUP
avupTransition = do
  TRC ((_slot, Dms _dms), src@(AVUpdate aupS, favs, avs), AVUpdate _aup) <-
    judgmentContext

  if Map.null aupS
    then pure src
    else do
      Map.keysSet _aup `Set.isSubsetOf` Map.keysSet _dms ?! NonGenesisUpdateAVUP

      let aup'         = Map.union _aup aupS
      let (cur, favs') = Map.partitionWithKey (\s _ -> s >= _slot) favs
      let avs'         = newAVs avs cur
      let fav          = votedValue aup'
      case fav of
        Nothing -> pure (AVUpdate aup', favs', avs')
        Just fav' ->
          pure
            ( AVUpdate Map.empty
            , Map.insert (_slot +* slotsPrior) fav' favs'
            , avs'
            )
