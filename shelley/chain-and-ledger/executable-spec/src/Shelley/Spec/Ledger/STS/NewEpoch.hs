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
    NewEpochPredicateFailure (..),
    PredicateFailure,
    calculatePoolDistr,
  )
where

import Cardano.Ledger.Era
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Delegation.Certificates
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.OverlaySchedule
import Shelley.Spec.Ledger.STS.Epoch
import Shelley.Spec.Ledger.STS.Mir
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.TxBody
import qualified Cardano.Ledger.Val as Val

data NEWEPOCH era

data NewEpochPredicateFailure era
  = EpochFailure (PredicateFailure (EPOCH era)) -- Subtransition Failures
  | CorruptRewardUpdate
      !(RewardUpdate era) -- The reward update which violates an invariant
  | MirFailure (PredicateFailure (MIR era)) -- Subtransition Failures
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (NewEpochPredicateFailure era)

instance
  Era era =>
  STS (NEWEPOCH era)
  where
  type State (NEWEPOCH era) = NewEpochState era

  type Signal (NEWEPOCH era) = EpochNo

  type Environment (NEWEPOCH era) = NewEpochEnv era

  type BaseM (NEWEPOCH era) = ShelleyBase
  type PredicateFailure (NEWEPOCH era) = NewEpochPredicateFailure era

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          emptyEpochState
          SNothing
          (PoolDistr Map.empty)
          emptyOverlaySchedule
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  Era era =>
  TransitionRule (NEWEPOCH era)
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
          Val.isZero (dt <> dr <> (fold rs_) <> df) ?! CorruptRewardUpdate ru'
          pure $ applyRUpd ru' es

      es'' <- trans @(MIR era) $ TRC ((), es', ())
      es''' <- trans @(EPOCH era) $ TRC ((), es'', e)
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

calculatePoolDistr :: SnapShot era -> PoolDistr era
calculatePoolDistr (SnapShot (Stake stake) delegs poolParams) =
  let Coin total = Map.foldl' (<>) mempty stake
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
  Era era =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = EpochFailure

instance
  Era era =>
  Embed (MIR era) (NEWEPOCH era)
  where
  wrapFailed = MirFailure
