{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.NewEpoch
  ( NEWEPOCH
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Maybe               as Maybe (fromMaybe)

import           BlockChain
import           Coin
import           EpochBoundary
import           LedgerState
import           PParams
import           Slot

import           STS.Epoch

import           Delegation.Certificates

import           Control.State.Transition

data NEWEPOCH

instance STS NEWEPOCH where
  type State NEWEPOCH = ( Epoch
                        , Seed
                        , BlocksMade
                        , EpochState
                        , Maybe RewardUpdate
                        , PoolDistr)
  type Signal NEWEPOCH = Epoch
  type Environment NEWEPOCH = (Seed, PParams)
  data PredicateFailure NEWEPOCH = EpochFailure (PredicateFailure
                                                 EPOCH)
                                   deriving (Show, Eq)
  initialRules =
    [ pure $
      ( Epoch 0
      , mkNonce 0
      , BlocksMade Map.empty
      , emptyEpochState
      , Nothing
      , PoolDistr Map.empty)
    ]
  transitionRules = [ocertTransition]

ocertTransition :: TransitionRule NEWEPOCH
ocertTransition = do
  TRC ((eta1, ppN), (Epoch eL, eta0, b, es, ru, pd), Epoch e) <- judgmentContext
  if eL /= e + 1
    then pure $ (Epoch eL, eta0, b, es, ru, pd)
    else do
      let es' =
            case ru of
              Nothing  -> es'
              Just ru' -> applyRUpd ru' es
      es'' <- trans @EPOCH $ TRC ((ppN, b), es', Epoch e)
      let EpochState _ _ ss _ = es
      let (Stake stake, delegs) = _pstakeSet ss
      let pd' =
            foldr
              (\(hk, c) m -> Map.insertWith (+) hk c m)
              Map.empty
              [ (poolKey, Maybe.fromMaybe (Coin 0) (Map.lookup stakeKey stake))
              | (stakeKey, poolKey) <- Map.toList delegs
              ]
      pure $ (Epoch e, eta1, BlocksMade Map.empty, es'', Nothing, PoolDistr pd')

instance Embed EPOCH NEWEPOCH where
  wrapFailed = EpochFailure
