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

import           Coin
import           EpochBoundary
import           LedgerState
import           Slot
import           TxData

import           STS.Epoch

import           Delegation.Certificates

import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Hedgehog (Gen)

data NEWEPOCH crypto

instance Crypto crypto
    => STS (NEWEPOCH crypto) where
  type State (NEWEPOCH crypto) = NewEpochState crypto
  type Signal (NEWEPOCH crypto) = Epoch
  type Environment (NEWEPOCH crypto) = NewEpochEnv crypto
  data PredicateFailure (NEWEPOCH crypto)
    = EpochFailure (PredicateFailure (EPOCH crypto))
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

newEpochTransition :: forall crypto
  .  Crypto crypto
  => TransitionRule (NEWEPOCH crypto)
newEpochTransition = do
  TRC ( NewEpochEnv _s gkeys
      , src@(NewEpochState (Epoch eL) _ bcur es ru _pd _osched)
      , e@(Epoch e_)) <- judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      let es_ = case ru of
            Nothing  -> es
            Just ru' -> applyRUpd ru' es
      es' <- trans @(EPOCH crypto) $ TRC ((), es_, e)
      let EpochState _acnt ss _ls pp = es'

          (Stake stake, delegs) = _pstakeSet ss

          Coin total = Map.foldl (+) (Coin 0) stake

          sd = aggregatePlus $ catMaybes
                 [ (, fromIntegral c / fromIntegral total) <$> Map.lookup hk delegs
                 | (hk, Coin c) <- Map.toList stake]

          pd' = Map.intersectionWith (,) sd (Map.map _poolVrf (_poolsSS ss))

          osched' = overlaySchedule e gkeys pp

      pure $ NewEpochState e
                           bcur
                           (BlocksMade Map.empty)
                           es'
                           Nothing
                           (PoolDistr pd')
                           osched'
     where
       aggregatePlus = Map.fromListWith (+)

instance Crypto crypto
    => Embed (EPOCH crypto) (NEWEPOCH crypto) where
  wrapFailed = EpochFailure

instance Crypto crypto
  => HasTrace (NEWEPOCH crypto) where
  envGen _ = undefined :: Gen (NewEpochEnv crypto)
  sigGen _ _ = undefined :: Gen Epoch
