{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.NewEpoch
  ( NEWEPOCH
  )
where

import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           BaseTypes
import           Coin
import           EpochBoundary
import           Keys (DSIGNAlgorithm, HashAlgorithm, VRFAlgorithm)
import           LedgerState
import           PParams
import           Slot
import           TxData

import           STS.Epoch

import           Delegation.Certificates

import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Hedgehog (Gen)

data NEWEPOCH hashAlgo dsignAlgo vrfAlgo

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
    => STS (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) where
  type State (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) = NewEpochState hashAlgo dsignAlgo vrfAlgo
  type Signal (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) = Epoch
  type Environment (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) = NewEpochEnv hashAlgo dsignAlgo
  data PredicateFailure (NEWEPOCH hashAlgo dsignAlgo vrfAlgo)
    = EpochFailure (PredicateFailure (EPOCH hashAlgo dsignAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules =
    [ pure $
      NewEpochState
        (Epoch 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        emptyEpochState
        Nothing
        (PoolDistr Map.empty)
        Map.empty]
  transitionRules = [newEpochTransition]

newEpochTransition :: forall hashAlgo dsignAlgo vrfAlgo
  .  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => TransitionRule (NEWEPOCH hashAlgo dsignAlgo vrfAlgo)
newEpochTransition = do
  TRC ( NewEpochEnv _s gkeys
      , src@(NewEpochState (Epoch eL) _ bcur es ru _pd _osched)
      , e@(Epoch e_)) <- judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      let es_ = case ru of
            Nothing  -> es
            Just ru' -> applyRUpd ru' e es
      es' <- trans @(EPOCH hashAlgo dsignAlgo vrfAlgo) $ TRC ((), es_, e)
      let EpochState acnt ss ls pp = es'

          (Stake stake, delegs) = _pstakeSet ss

          Coin total = Map.foldl (+) (Coin 0) stake

          sd = aggregatePlus $ catMaybes
                 [ (, fromIntegral c / fromIntegral total) <$> Map.lookup hk delegs
                 | (hk, Coin c) <- Map.toList stake]

          pd' = Map.intersectionWith (,) sd (Map.map _poolVrf (_poolsSS ss))

          osched' = overlaySchedule e gkeys pp

          es'' = EpochState acnt ss ls (pp { _extraEntropy = NeutralNonce })

      pure $ NewEpochState e
                           bcur
                           (BlocksMade Map.empty)
                           es''
                           Nothing
                           (PoolDistr pd')
                           osched'
     where
       aggregatePlus = Map.fromListWith (+)

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
    => Embed (EPOCH hashAlgo dsignAlgo vrfAlgo) (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) where
  wrapFailed = EpochFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  )
  => HasTrace (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) where
  envGen _ = undefined :: Gen (NewEpochEnv hashAlgo dsignAlgo)
  sigGen _ _ = undefined :: Gen Epoch
