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
  TRC ((slot, Dms dms), src@(AVUpdate aupS, favs, avs), AVUpdate aup) <-
    judgmentContext

  if Map.null aupS
    then pure src
    else do
      Map.keysSet aup `Set.isSubsetOf` Map.keysSet dms ?! NonGenesisUpdateAVUP

      let aup'         = Map.union aup aupS
      let (cur, favs') = Map.partitionWithKey (\s _ -> s >= slot) favs
      let avs'         = newAVs avs cur
      let fav          = votedValue aup'
      case fav of
        Nothing -> pure (AVUpdate aup', favs', avs')
        Just fav' ->
          pure
            ( AVUpdate Map.empty
            , Map.insert (slot +* slotsPrior) fav' favs'
            , avs'
            )
