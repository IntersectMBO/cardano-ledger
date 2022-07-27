{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  ( BlocksMade (BlocksMade),
    ProtVer,
    ShelleyBase,
    StrictMaybe (SJust, SNothing),
  )
import Cardano.Ledger.Coin (Coin (Coin), toDeltaCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Shelley.AdaPots (AdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (Reward, sumRewards)
import Cardano.Ledger.Shelley.Rules.Epoch
import Cardano.Ledger.Shelley.Rules.Mir (MIR, MirEvent, MirPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Rupd (RupdEvent (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (_poolVrf))
import Cardano.Ledger.Slot (EpochNo (EpochNo))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition
import Data.Default.Class (Default, def)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))

data NEWEPOCH era

data NewEpochPredicateFailure era
  = EpochFailure (PredicateFailure (EraRule "EPOCH" era)) -- Subtransition Failures
  | CorruptRewardUpdate
      !(RewardUpdate (Crypto era)) -- The reward update which violates an invariant
  | MirFailure (PredicateFailure (EraRule "MIR" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (EraRule "EPOCH" era)),
    Show (PredicateFailure (EraRule "MIR" era))
  ) =>
  Show (NewEpochPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "EPOCH" era)),
    Eq (PredicateFailure (EraRule "MIR" era))
  ) =>
  Eq (NewEpochPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "EPOCH" era)),
    NoThunks (PredicateFailure (EraRule "MIR" era))
  ) =>
  NoThunks (NewEpochPredicateFailure era)

data NewEpochEvent era
  = DeltaRewardEvent (Event (EraRule "RUPD" era))
  | RestrainedRewards
      EpochNo
      (Map.Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))))
      (Set (Credential 'Staking (Crypto era)))
  | TotalRewardEvent EpochNo (Map.Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))))
  | EpochEvent (Event (EraRule "EPOCH" era))
  | MirEvent (Event (EraRule "MIR" era))
  | TotalAdaPotsEvent AdaPots

instance
  ( EraTxOut era,
    Embed (EraRule "MIR" era) (NEWEPOCH era),
    Embed (EraRule "EPOCH" era) (NEWEPOCH era),
    Environment (EraRule "MIR" era) ~ (),
    State (EraRule "MIR" era) ~ EpochState era,
    Signal (EraRule "MIR" era) ~ (),
    Event (EraRule "RUPD" era) ~ RupdEvent (Crypto era),
    Environment (EraRule "EPOCH" era) ~ (),
    State (EraRule "EPOCH" era) ~ EpochState era,
    Signal (EraRule "EPOCH" era) ~ EpochNo,
    Default (EpochState era),
    HasField "_protocolVersion" (PParams era) ProtVer,
    Default (State (EraRule "PPUP" era)),
    Default (PParams era),
    Default (StashedAVVMAddresses era)
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
          def
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  ( EraTxOut era,
    Embed (EraRule "MIR" era) (NEWEPOCH era),
    Embed (EraRule "EPOCH" era) (NEWEPOCH era),
    Event (EraRule "RUPD" era) ~ RupdEvent (Crypto era),
    Environment (EraRule "MIR" era) ~ (),
    State (EraRule "MIR" era) ~ EpochState era,
    Signal (EraRule "MIR" era) ~ (),
    Environment (EraRule "EPOCH" era) ~ (),
    State (EraRule "EPOCH" era) ~ EpochState era,
    Signal (EraRule "EPOCH" era) ~ EpochNo,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Default (State (EraRule "PPUP" era)),
    Default (PParams era),
    Default (StashedAVVMAddresses era),
    Event (EraRule "RUPD" era) ~ RupdEvent (Crypto era)
  ) =>
  TransitionRule (NEWEPOCH era)
newEpochTransition = do
  TRC
    ( _,
      src@(NewEpochState (EpochNo eL) _ bcur es ru _pd _),
      e@(EpochNo e_)
      ) <-
    judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      let updateRewards ru'@(RewardUpdate dt dr rs_ df _) = do
            let totRs = sumRewards (esPrevPp es) rs_
            Val.isZero (dt <> (dr <> toDeltaCoin totRs <> df)) ?! CorruptRewardUpdate ru'
            let (es', regRU, eraIgnored, unregistered) = applyRUpd' ru' es
            tellEvent $ RestrainedRewards e eraIgnored unregistered
            -- This event (which is only generated once per epoch) must be generated even if the
            -- map is empty (db-sync depends on it).
            tellEvent $ TotalRewardEvent e regRU
            pure es'
      es' <- case ru of
        SNothing -> pure es
        SJust p@(Pulsing _ _) -> do
          (ans, event) <- liftSTS (completeRupd p)
          tellReward (DeltaRewardEvent (RupdEvent e event))
          updateRewards ans
        SJust (Complete ru') -> updateRewards ru'
      es'' <- trans @(EraRule "MIR" era) $ TRC ((), es', ())
      es''' <- trans @(EraRule "EPOCH" era) $ TRC ((), es'', e)
      let adaPots = totalAdaPotsES es'''
      tellEvent $ TotalAdaPotsEvent adaPots
      let ss = esSnapshots es'''
          pd' = calculatePoolDistr (_pstakeSet ss)
      pure $
        src
          { nesEL = e,
            nesBprev = bcur,
            nesBcur = BlocksMade mempty,
            nesEs = es''',
            nesRu = SNothing,
            nesPd = pd'
          }

-- | tell a RupdEvent as a DeltaRewardEvent only if the map is non-empty
tellReward ::
  (Event (EraRule "RUPD" era) ~ RupdEvent (Crypto era)) =>
  NewEpochEvent era ->
  Rule (NEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

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
  ( STS (EPOCH era),
    PredicateFailure (EraRule "EPOCH" era) ~ EpochPredicateFailure era,
    Event (EraRule "EPOCH" era) ~ EpochEvent era
  ) =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = EpochFailure
  wrapEvent = EpochEvent

instance
  ( Era era,
    Default (EpochState era),
    PredicateFailure (EraRule "MIR" era) ~ MirPredicateFailure era,
    Event (EraRule "MIR" era) ~ MirEvent era
  ) =>
  Embed (MIR era) (NEWEPOCH era)
  where
  wrapFailed = MirFailure
  wrapEvent = MirEvent
