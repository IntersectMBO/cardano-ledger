{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.NewEpoch (
  DijkstraNEWEPOCH,
  DijkstraNewEpochPredFailure (..),
  DijkstraNewEpochEvent (..),
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (BlocksMade),
  ShelleyBase,
  StrictMaybe (SJust, SNothing),
 )
import Cardano.Ledger.Coin (toDeltaCoin)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEPOCH, DijkstraEra, DijkstraNEWEPOCH)
import Cardano.Ledger.Dijkstra.Governance (
  DijkstraEraGov,
  DijkstraGovState,
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  newEpochStateDRepPulsingStateL,
  predictFuturePParams,
  pulseDRepPulsingState,
 )
import Cardano.Ledger.Dijkstra.Rules.Epoch (DijkstraEpochEvent)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), totalAdaPotsES)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules (
  RupdEvent (..),
  ShelleyTICK,
  ShelleyTickEvent (..),
  ShelleyTickPredFailure (..),
 )
import Cardano.Ledger.Slot (EpochNo (EpochNo))
import Cardano.Ledger.State
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.State.Transition
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (^.))

newtype DijkstraNewEpochPredFailure era
  = CorruptRewardUpdate
      RewardUpdate -- The reward update which violates an invariant
  deriving (Generic)

deriving instance Eq (DijkstraNewEpochPredFailure era)

deriving instance
  ( Show (PredicateFailure (EraRule "EPOCH" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  Show (DijkstraNewEpochPredFailure era)

instance NFData (DijkstraNewEpochPredFailure era)

data DijkstraNewEpochEvent era
  = DeltaRewardEvent !(Event (EraRule "RUPD" era))
  | RestrainedRewards
      !EpochNo
      !(Map.Map (Credential 'Staking) (Set Reward))
      !(Set (Credential 'Staking))
  | TotalRewardEvent
      !EpochNo
      !(Map.Map (Credential 'Staking) (Set Reward))
  | EpochEvent !(Event (EraRule "EPOCH" era))
  | TotalAdaPotsEvent !AdaPots
  deriving (Generic)

type instance EraRuleEvent "NEWEPOCH" DijkstraEra = DijkstraNewEpochEvent DijkstraEra

deriving instance
  ( Eq (Event (EraRule "EPOCH" era))
  , Eq (Event (EraRule "RUPD" era))
  ) =>
  Eq (DijkstraNewEpochEvent era)

instance
  ( NFData (Event (EraRule "EPOCH" era))
  , NFData (Event (EraRule "RUPD" era))
  ) =>
  NFData (DijkstraNewEpochEvent era)

instance
  ( EraTxOut era
  , DijkstraEraGov era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "EPOCH" era) (DijkstraNEWEPOCH era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  , Environment (EraRule "EPOCH" era) ~ ()
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (EpochState era)
  , Default (StashedAVVMAddresses era)
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ DijkstraGovState era
  , Eq (PredicateFailure (EraRule "RATIFY" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  STS (DijkstraNEWEPOCH era)
  where
  type State (DijkstraNEWEPOCH era) = NewEpochState era
  type Signal (DijkstraNEWEPOCH era) = EpochNo
  type Environment (DijkstraNEWEPOCH era) = ()
  type BaseM (DijkstraNEWEPOCH era) = ShelleyBase
  type PredicateFailure (DijkstraNEWEPOCH era) = DijkstraNewEpochPredFailure era
  type Event (DijkstraNEWEPOCH era) = DijkstraNewEpochEvent era

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          def
          SNothing
          (PoolDistr Map.empty mempty)
          def
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  ( EraTxOut era
  , DijkstraEraGov era
  , EraCertState era
  , Embed (EraRule "EPOCH" era) (DijkstraNEWEPOCH era)
  , Environment (EraRule "EPOCH" era) ~ ()
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (StashedAVVMAddresses era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ DijkstraGovState era
  , Eq (PredicateFailure (EraRule "RATIFY" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  TransitionRule (DijkstraNEWEPOCH era)
newEpochTransition = do
  TRC
    ( _
      , nes@(NewEpochState eL _ bcur es0 ru _ _)
      , eNo
      ) <-
    judgmentContext
  if eNo /= succ eL
    then
      pure $
        nes
          & newEpochStateDRepPulsingStateL %~ pulseDRepPulsingState
          & newEpochStateGovStateL %~ predictFuturePParams
    else do
      es1 <- case ru of -- Here is where we extract the result of Reward pulsing.
        SNothing -> pure es0
        SJust p@(Pulsing _ _) -> do
          (ans, event) <- liftSTS (completeRupd p)
          tellReward (DeltaRewardEvent (RupdEvent eNo event))
          updateRewards es0 eNo ans
        SJust (Complete ru') -> updateRewards es0 eNo ru'
      es2 <- trans @(EraRule "EPOCH" era) $ TRC ((), es1, eNo)
      let adaPots = totalAdaPotsES es2
      tellEvent $ TotalAdaPotsEvent adaPots
      let pd' = ssStakeMarkPoolDistr (esSnapshots es0)
      -- See `ShelleyNEWEPOCH` for details on the implementation
      pure $
        nes
          { nesEL = eNo
          , nesBprev = bcur
          , nesBcur = BlocksMade mempty
          , nesEs = es2
          , nesRu = SNothing
          , nesPd = pd'
          }

-- | tell a RupdEvent as a DeltaRewardEvent only if the map is non-empty
tellReward ::
  Event (EraRule "RUPD" era) ~ RupdEvent =>
  DijkstraNewEpochEvent era ->
  Rule (DijkstraNEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

updateRewards ::
  (EraGov era, EraCertState era) =>
  EpochState era ->
  EpochNo ->
  RewardUpdate ->
  Rule (DijkstraNEWEPOCH era) 'Transition (EpochState era)
updateRewards es e ru'@(RewardUpdate dt dr rs_ df _) = do
  let totRs = sumRewards (es ^. prevPParamsEpochStateL . ppProtocolVersionL) rs_
  Val.isZero (dt <> dr <> toDeltaCoin totRs <> df) ?! CorruptRewardUpdate ru'
  let !(!es', filtered) = applyRUpdFiltered ru' es
  tellEvent $ RestrainedRewards e (frShelleyIgnored filtered) (frUnregistered filtered)
  -- This event (which is only generated once per epoch) must be generated even if the
  -- map is empty (db-sync depends on it).
  tellEvent $ TotalRewardEvent e (frRegistered filtered)
  pure es'

instance
  ( STS (DijkstraNEWEPOCH era)
  , PredicateFailure (EraRule "NEWEPOCH" era) ~ DijkstraNewEpochPredFailure era
  , Event (EraRule "NEWEPOCH" era) ~ DijkstraNewEpochEvent era
  ) =>
  Embed (DijkstraNEWEPOCH era) (ShelleyTICK era)
  where
  wrapFailed = NewEpochFailure
  wrapEvent = TickNewEpochEvent

instance
  ( STS (DijkstraEPOCH era)
  , Event (EraRule "EPOCH" era) ~ DijkstraEpochEvent era
  ) =>
  Embed (DijkstraEPOCH era) (DijkstraNEWEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = EpochEvent
