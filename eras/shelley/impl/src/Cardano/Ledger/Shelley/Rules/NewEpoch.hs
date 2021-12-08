{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.NewEpoch
  ( NEWEPOCH,
    NewEpochPredicateFailure (..),
    NewEpochEvent (..),
    PredicateFailure,
    calculatePoolDistr,
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (Reward, sumRewards)
import Cardano.Ledger.Shelley.Rules.Epoch
import Cardano.Ledger.Shelley.Rules.Mir
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Slot
import qualified Cardano.Ledger.Val as Val
import Control.Provenance (runProvM)
import Control.State.Transition
import Data.Compact.ViewMap as VMap
import Data.Default.Class (Default, def)
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))

data NEWEPOCH era

data NewEpochPredicateFailure era
  = EpochFailure (PredicateFailure (Core.EraRule "EPOCH" era)) -- Subtransition Failures
  | CorruptRewardUpdate
      !(RewardUpdate (Crypto era)) -- The reward update which violates an invariant
  | MirFailure (PredicateFailure (Core.EraRule "MIR" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "EPOCH" era)),
    Show (PredicateFailure (Core.EraRule "MIR" era))
  ) =>
  Show (NewEpochPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "EPOCH" era)),
    Eq (PredicateFailure (Core.EraRule "MIR" era))
  ) =>
  Eq (NewEpochPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (Core.EraRule "EPOCH" era)),
    NoThunks (PredicateFailure (Core.EraRule "MIR" era))
  ) =>
  NoThunks (NewEpochPredicateFailure era)

data NewEpochEvent era
  = RewardEvent !EpochNo !(Map.Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))))
  | EpochEvent (Event (Core.EraRule "EPOCH" era))
  | MirEvent (Event (Core.EraRule "MIR" era))

instance
  ( UsesTxOut era,
    UsesValue era,
    Embed (Core.EraRule "MIR" era) (NEWEPOCH era),
    Embed (Core.EraRule "EPOCH" era) (NEWEPOCH era),
    Environment (Core.EraRule "MIR" era) ~ (),
    State (Core.EraRule "MIR" era) ~ EpochState era,
    Signal (Core.EraRule "MIR" era) ~ (),
    Environment (Core.EraRule "EPOCH" era) ~ (),
    State (Core.EraRule "EPOCH" era) ~ EpochState era,
    Signal (Core.EraRule "EPOCH" era) ~ EpochNo,
    Default (EpochState era),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    Default (State (Core.EraRule "PPUP" era)),
    Default (Core.PParams era)
  ) =>
  STS (NEWEPOCH era)
  where
  type State (NEWEPOCH era) = NewEpochState era

  type Signal (NEWEPOCH era) = EpochNo

  type Environment (NEWEPOCH era) = ()

  type BaseM (NEWEPOCH era) = ShelleyBase
  type PredicateFailure (NEWEPOCH era) = NewEpochPredicateFailure era
  type Event (NEWEPOCH era) = NewEpochEvent era

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          def
          SNothing
          (PoolDistr Map.empty)
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  ( Embed (Core.EraRule "MIR" era) (NEWEPOCH era),
    Embed (Core.EraRule "EPOCH" era) (NEWEPOCH era),
    Environment (Core.EraRule "MIR" era) ~ (),
    State (Core.EraRule "MIR" era) ~ EpochState era,
    Signal (Core.EraRule "MIR" era) ~ (),
    Environment (Core.EraRule "EPOCH" era) ~ (),
    State (Core.EraRule "EPOCH" era) ~ EpochState era,
    Signal (Core.EraRule "EPOCH" era) ~ EpochNo,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    UsesTxOut era,
    UsesValue era,
    Default (State (Core.EraRule "PPUP" era)),
    Default (Core.PParams era)
  ) =>
  TransitionRule (NEWEPOCH era)
newEpochTransition = do
  TRC
    ( _,
      src@(NewEpochState (EpochNo eL) _ bcur es ru _pd),
      e@(EpochNo e_)
      ) <-
    judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      let updateRewards ru'@(RewardUpdate dt dr rs_ df _) = do
            let totRs = sumRewards (esPrevPp es) rs_
            Val.isZero (dt <> (dr <> toDeltaCoin totRs <> df)) ?! CorruptRewardUpdate ru'
            let (es', regRU) = applyRUpd' ru' es
            tellEvent $ RewardEvent e regRU
            pure es'
      es' <- case ru of
        SNothing -> pure es
        SJust p@(Pulsing _ _) -> liftSTS (runProvM $ completeRupd p) >>= updateRewards
        SJust (Complete ru') -> updateRewards ru'
      es'' <- trans @(Core.EraRule "MIR" era) $ TRC ((), es', ())
      es''' <- trans @(Core.EraRule "EPOCH" era) $ TRC ((), es'', e)
      let EpochState _acnt ss _ls _pr _ _ = es'''
          pd' = calculatePoolDistr (_pstakeSet ss)
      pure $
        NewEpochState
          e
          bcur
          (BlocksMade Map.empty)
          es'''
          SNothing
          pd'

calculatePoolDistr :: SnapShot crypto -> PoolDistr crypto
calculatePoolDistr (SnapShot stake delegs poolParams) =
  let Coin total = sumAllStake stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotal = if total == 0 then 1 else total
      sd =
        Map.fromListWith (+) $
          [ (d, c % nonZeroTotal)
            | (hk, compactCoin) <- VMap.toAscList (unStake stake),
              let Coin c = fromCompact compactCoin,
              Just d <- [VMap.lookup hk delegs]
          ]
   in PoolDistr $
        Map.intersectionWith
          IndividualPoolStake
          sd
          (toMap (VMap.map _poolVrf poolParams))

instance
  ( UsesTxOut era,
    UsesValue era,
    STS (EPOCH era),
    PredicateFailure (Core.EraRule "EPOCH" era) ~ EpochPredicateFailure era,
    Event (Core.EraRule "EPOCH" era) ~ EpochEvent era
  ) =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = EpochFailure
  wrapEvent = EpochEvent

instance
  ( Era era,
    Default (EpochState era),
    PredicateFailure (Core.EraRule "MIR" era) ~ MirPredicateFailure era,
    Event (Core.EraRule "MIR" era) ~ MirEvent era
  ) =>
  Embed (MIR era) (NEWEPOCH era)
  where
  wrapFailed = MirFailure
  wrapEvent = MirEvent
