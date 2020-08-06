{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.NewEpoch
  ( NEWEPOCH,
    PredicateFailure (..),
    calculatePoolDistr,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.STS.Epoch
import Shelley.Spec.Ledger.STS.Mir
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.TxData
import Shelley.Spec.Ledger.Value

data NEWEPOCH crypto v

instance
  CV crypto v =>
  STS (NEWEPOCH crypto v)
  where
  type State (NEWEPOCH crypto v) = NewEpochState crypto v

  type Signal (NEWEPOCH crypto v) = EpochNo

  type Environment (NEWEPOCH crypto v) = NewEpochEnv crypto

  type BaseM (NEWEPOCH crypto v) = ShelleyBase

  data PredicateFailure (NEWEPOCH crypto v)
    = EpochFailure (PredicateFailure (EPOCH crypto v)) -- Subtransition Failures
    | CorruptRewardUpdate
        !(RewardUpdate crypto) -- The reward update which violates an invariant
    | MirFailure (PredicateFailure (MIR crypto v)) -- Subtransition Failures
    deriving (Show, Generic, Eq)

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          emptyEpochState
          SNothing
          (PoolDistr Map.empty)
          Map.empty
    ]

  transitionRules = [newEpochTransition]

instance NoUnexpectedThunks (PredicateFailure (NEWEPOCH crypto v))

newEpochTransition ::
  forall crypto v.
  CV crypto v =>
  TransitionRule (NEWEPOCH crypto v)
newEpochTransition = do
  TRC
    ( NewEpochEnv _s gkeys,
      src@(NewEpochState (EpochNo eL) _ bcur es ru _pd _osched),
      e@(EpochNo e_)
      ) <-
    judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      es' <- case ru of
        SNothing -> pure es
        SJust ru' -> do
          let RewardUpdate dt dr rs_ df _ = ru'
          dt + dr + (sum rs_) + df == 0 ?! CorruptRewardUpdate ru'
          pure $ applyRUpd ru' es

      es'' <- trans @(MIR crypto v) $ TRC ((), es', ())
      es''' <- trans @(EPOCH crypto v) $ TRC ((), es'', e)
      let EpochState _acnt ss _ls _pr pp _ = es'''
          pd' = calculatePoolDistr (_pstakeSet ss)
      osched' <- liftSTS $ overlaySchedule e gkeys pp
      pure $
        NewEpochState
          e
          bcur
          (BlocksMade Map.empty)
          es'''
          SNothing
          pd'
          osched'

calculatePoolDistr :: SnapShot crypto -> PoolDistr crypto
calculatePoolDistr (SnapShot (Stake stake) delegs poolParams) =
  let Coin total = Map.foldl' (+) (Coin 0) stake
      sd =
        Map.fromListWith (+) $
          catMaybes
            [ (,fromIntegral c / fromIntegral (if total == 0 then 1 else total))
                <$> Map.lookup hk delegs -- TODO mgudemann total could be zero (in
                -- particular when shrinking)
              | (hk, Coin c) <- Map.toList stake
            ]
   in PoolDistr $ Map.intersectionWith IndividualPoolStake sd (Map.map _poolVrf poolParams)

instance
  CV crypto v =>
  Embed (EPOCH crypto v) (NEWEPOCH crypto v)
  where
  wrapFailed = EpochFailure

instance
  CV crypto v =>
  Embed (MIR crypto v) (NEWEPOCH crypto v)
  where
  wrapFailed = MirFailure
