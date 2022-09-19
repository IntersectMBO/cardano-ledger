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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.NewEpoch
  ( ShelleyNEWEPOCH,
    ShelleyNewEpochPredFailure (..),
    ShelleyNewEpochEvent (..),
    PredicateFailure,
    calculatePoolDistr,
    calculatePoolDistr',
  )
where

import Cardano.Ledger.BaseTypes
  ( BlocksMade (BlocksMade),
    ShelleyBase,
    StrictMaybe (SJust, SNothing),
  )
import Cardano.Ledger.Coin (Coin (Coin), toDeltaCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Shelley.AdaPots (AdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.Era (ShelleyNEWEPOCH)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules.Epoch
import Cardano.Ledger.Shelley.Rules.Mir (ShelleyMIR, ShelleyMirEvent, ShelleyMirPredFailure)
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
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data ShelleyNewEpochPredFailure era
  = EpochFailure (PredicateFailure (EraRule "EPOCH" era)) -- Subtransition Failures
  | CorruptRewardUpdate
      !(RewardUpdate (EraCrypto era)) -- The reward update which violates an invariant
  | MirFailure (PredicateFailure (EraRule "MIR" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (EraRule "EPOCH" era)),
    Show (PredicateFailure (EraRule "MIR" era))
  ) =>
  Show (ShelleyNewEpochPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "EPOCH" era)),
    Eq (PredicateFailure (EraRule "MIR" era))
  ) =>
  Eq (ShelleyNewEpochPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "EPOCH" era)),
    NoThunks (PredicateFailure (EraRule "MIR" era))
  ) =>
  NoThunks (ShelleyNewEpochPredFailure era)

data ShelleyNewEpochEvent era
  = DeltaRewardEvent (Event (EraRule "RUPD" era))
  | RestrainedRewards
      EpochNo
      (Map.Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
      (Set (Credential 'Staking (EraCrypto era)))
  | TotalRewardEvent EpochNo (Map.Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
  | EpochEvent (Event (EraRule "EPOCH" era))
  | MirEvent (Event (EraRule "MIR" era))
  | TotalAdaPotsEvent AdaPots

instance
  ( EraTxOut era,
    Embed (EraRule "MIR" era) (ShelleyNEWEPOCH era),
    Embed (EraRule "EPOCH" era) (ShelleyNEWEPOCH era),
    Environment (EraRule "MIR" era) ~ (),
    State (EraRule "MIR" era) ~ EpochState era,
    Signal (EraRule "MIR" era) ~ (),
    Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era),
    Environment (EraRule "EPOCH" era) ~ (),
    State (EraRule "EPOCH" era) ~ EpochState era,
    Signal (EraRule "EPOCH" era) ~ EpochNo,
    Default (EpochState era),
    Default (State (EraRule "PPUP" era)),
    Default (PParams era),
    Default (StashedAVVMAddresses era),
    EraPParams era
  ) =>
  STS (ShelleyNEWEPOCH era)
  where
  type State (ShelleyNEWEPOCH era) = NewEpochState era

  type Signal (ShelleyNEWEPOCH era) = EpochNo

  type Environment (ShelleyNEWEPOCH era) = ()

  type BaseM (ShelleyNEWEPOCH era) = ShelleyBase
  type PredicateFailure (ShelleyNEWEPOCH era) = ShelleyNewEpochPredFailure era
  type Event (ShelleyNEWEPOCH era) = ShelleyNewEpochEvent era

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
    Embed (EraRule "MIR" era) (ShelleyNEWEPOCH era),
    Embed (EraRule "EPOCH" era) (ShelleyNEWEPOCH era),
    Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era),
    Environment (EraRule "MIR" era) ~ (),
    State (EraRule "MIR" era) ~ EpochState era,
    Signal (EraRule "MIR" era) ~ (),
    Environment (EraRule "EPOCH" era) ~ (),
    State (EraRule "EPOCH" era) ~ EpochState era,
    Signal (EraRule "EPOCH" era) ~ EpochNo,
    Default (State (EraRule "PPUP" era)),
    Default (PParams era),
    Default (StashedAVVMAddresses era),
    Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era),
    EraPParams era
  ) =>
  TransitionRule (ShelleyNEWEPOCH era)
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
            let totRs = sumRewards (esPrevPp es ^. ppProtocolVersionL) rs_
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
  (Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)) =>
  ShelleyNewEpochEvent era ->
  Rule (ShelleyNEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

calculatePoolDistr :: SnapShot c -> PoolDistr c
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' :: forall c. (KeyHash 'StakePool c -> Bool) -> SnapShot c -> PoolDistr c
calculatePoolDistr' includeHash (SnapShot stake delegs poolParams) =
  let Coin total = sumAllStake stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotal = if total == 0 then 1 else total
      sd =
        Map.fromListWith (+) $
          [ (d, c % nonZeroTotal)
            | (hk, compactCoin) <- VMap.toAscList (unStake stake),
              let Coin c = fromCompact compactCoin,
              Just d <- [VMap.lookup hk delegs],
              includeHash d
          ]
   in PoolDistr $
        Map.intersectionWith
          IndividualPoolStake
          sd
          (toMap (VMap.map _poolVrf poolParams))

instance
  ( STS (ShelleyEPOCH era),
    PredicateFailure (EraRule "EPOCH" era) ~ ShelleyEpochPredFailure era,
    Event (EraRule "EPOCH" era) ~ ShelleyEpochEvent era
  ) =>
  Embed (ShelleyEPOCH era) (ShelleyNEWEPOCH era)
  where
  wrapFailed = EpochFailure
  wrapEvent = EpochEvent

instance
  ( Era era,
    Default (EpochState era),
    PredicateFailure (EraRule "MIR" era) ~ ShelleyMirPredFailure era,
    Event (EraRule "MIR" era) ~ ShelleyMirEvent era
  ) =>
  Embed (ShelleyMIR era) (ShelleyNEWEPOCH era)
  where
  wrapFailed = MirFailure
  wrapEvent = MirEvent
