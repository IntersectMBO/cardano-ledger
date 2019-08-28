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
import           Keys (DSIGNAlgorithm, HashAlgorithm)
import           LedgerState
import           PParams
import           Slot
import           TxData

import           STS.Epoch

import           Delegation.Certificates

import           Control.State.Transition

data NEWEPOCH hashAlgo dsignAlgo

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
    => STS (NEWEPOCH hashAlgo dsignAlgo) where
  type State (NEWEPOCH hashAlgo dsignAlgo) = NewEpochState hashAlgo dsignAlgo
  type Signal (NEWEPOCH hashAlgo dsignAlgo) = Epoch
  type Environment (NEWEPOCH hashAlgo dsignAlgo) = NewEpochEnv hashAlgo dsignAlgo
  data PredicateFailure (NEWEPOCH hashAlgo dsignAlgo)
    = EpochFailure (PredicateFailure (EPOCH hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules =
    [ pure $
      NewEpochState
        (Epoch 0)
        (mkNonce 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        emptyEpochState
        Nothing
        (PoolDistr Map.empty)
        Map.empty]
  transitionRules = [newEpochTransition]

newEpochTransition :: forall hashAlgo dsignAlgo
  .  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (NEWEPOCH hashAlgo dsignAlgo)
newEpochTransition = do
  TRC ( NewEpochEnv eta1 _s gkeys
      , src@(NewEpochState (Epoch eL) _ _ bcur es ru _pd _osched)
      , e@(Epoch e_)) <- judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      let es_ = case ru of
            Nothing  -> es
            Just ru' -> applyRUpd ru' es
      es' <- trans @(EPOCH hashAlgo dsignAlgo) $ TRC ((), es_, e)
      let EpochState acnt ss ls pp = es'

          (Stake stake, delegs) = _pstakeSet ss

          Coin total = Map.foldl (+) (Coin 0) stake

          sd = aggregatePlus $ catMaybes
                 [ (, fromIntegral c / fromIntegral total) <$> Map.lookup hk delegs
                 | (hk, Coin c) <- Map.toList stake]

          pd' = Map.intersectionWith (,) sd (Map.map _poolVrf (_poolsSS ss))

          osched' = overlaySchedule e gkeys eta1 pp

          etaE = _extraEntropy pp

          es'' = EpochState acnt ss ls (pp { _extraEntropy = NeutralSeed })

      pure $ NewEpochState e
                           (eta1 ⭒ etaE)
                           bcur
                           (BlocksMade Map.empty)
                           es''
                           Nothing
                           (PoolDistr pd')
                           osched'
     where
       aggregatePlus = Map.fromListWith (+)

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
    => Embed (EPOCH hashAlgo dsignAlgo) (NEWEPOCH hashAlgo dsignAlgo) where
  wrapFailed = EpochFailure
