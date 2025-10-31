{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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

module Cardano.Ledger.Conway.Rules.NewEpoch (
  ConwayNEWEPOCH,
  ConwayNewEpochEvent (..),
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (BlocksMade),
  ShelleyBase,
  StrictMaybe (SJust, SNothing),
 )
import Cardano.Ledger.Coin (toDeltaCoin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayEra, ConwayNEWEPOCH)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  ConwayGovState,
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  newEpochStateDRepPulsingStateL,
  predictFuturePParams,
  pulseDRepPulsingState,
 )
import Cardano.Ledger.Conway.Rules.Epoch (ConwayEpochEvent)
import Cardano.Ledger.Conway.Rules.HardFork (ConwayHardForkEvent (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), totalAdaPotsES)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules (
  RupdEvent (..),
  ShelleyTICK,
  ShelleyTickEvent (..),
 )
import Cardano.Ledger.Slot (EpochNo (EpochNo))
import Cardano.Ledger.State
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.State.Transition
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (^.))

data ConwayNewEpochEvent era
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

type instance EraRuleEvent "NEWEPOCH" ConwayEra = ConwayNewEpochEvent ConwayEra

deriving instance
  ( Eq (Event (EraRule "EPOCH" era))
  , Eq (Event (EraRule "RUPD" era))
  ) =>
  Eq (ConwayNewEpochEvent era)

instance
  ( NFData (Event (EraRule "EPOCH" era))
  , NFData (Event (EraRule "RUPD" era))
  ) =>
  NFData (ConwayNewEpochEvent era)

instance
  ( EraTxOut era
  , ConwayEraGov era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "EPOCH" era) (ConwayNEWEPOCH era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  , Environment (EraRule "EPOCH" era) ~ ()
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (EpochState era)
  , Default (StashedAVVMAddresses era)
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ ConwayGovState era
  , Eq (PredicateFailure (EraRule "RATIFY" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  , Eq (PredicateFailure (ConwayNEWEPOCH era))
  , Show (PredicateFailure (ConwayNEWEPOCH era))
#endif
  ) =>
  STS (ConwayNEWEPOCH era)
  where
  type State (ConwayNEWEPOCH era) = NewEpochState era
  type Signal (ConwayNEWEPOCH era) = EpochNo
  type Environment (ConwayNEWEPOCH era) = ()
  type BaseM (ConwayNEWEPOCH era) = ShelleyBase
  type PredicateFailure (ConwayNEWEPOCH era) = Void
  type Event (ConwayNEWEPOCH era) = ConwayNewEpochEvent era

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
  , ConwayEraGov era
  , EraCertState era
  , Embed (EraRule "EPOCH" era) (ConwayNEWEPOCH era)
  , Environment (EraRule "EPOCH" era) ~ ()
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (StashedAVVMAddresses era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ ConwayGovState era
  , Eq (PredicateFailure (EraRule "RATIFY" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  , Eq (PredicateFailure (ConwayNEWEPOCH era))
  , Show (PredicateFailure (ConwayNEWEPOCH era))
#endif
  ) =>
  TransitionRule (ConwayNEWEPOCH era)
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
  ConwayNewEpochEvent era ->
  Rule (ConwayNEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

updateRewards ::
  (EraGov era, EraCertState era) =>
  EpochState era ->
  EpochNo ->
  RewardUpdate ->
  Rule (ConwayNEWEPOCH era) 'Transition (EpochState era)
updateRewards es e ru'@(RewardUpdate dt dr rs_ df _) = do
  let totRs = sumRewards (es ^. prevPParamsEpochStateL . ppProtocolVersionL) rs_
   in assert (Val.isZero (dt <> dr <> toDeltaCoin totRs <> df)) (pure ())
  let !(!es', filtered) = applyRUpdFiltered ru' es
  tellEvent $ RestrainedRewards e (frShelleyIgnored filtered) (frUnregistered filtered)
  -- This event (which is only generated once per epoch) must be generated even if the
  -- map is empty (db-sync depends on it).
  tellEvent $ TotalRewardEvent e (frRegistered filtered)
  pure es'

instance
  ( STS (ConwayNEWEPOCH era)
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , PredicateFailure (EraRule "NEWEPOCH" era) ~ PredicateFailure (ConwayNEWEPOCH era)
  ) =>
  Embed (ConwayNEWEPOCH era) (ShelleyTICK era)
  where
  wrapFailed = \case {}
  wrapEvent = TickNewEpochEvent

instance
  ( STS (ConwayEPOCH era)
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  Embed (ConwayEPOCH era) (ConwayNEWEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = EpochEvent

instance InjectRuleEvent "NEWEPOCH" ConwayEpochEvent ConwayEra where
  injectEvent = EpochEvent

instance InjectRuleEvent "NEWEPOCH" ConwayHardForkEvent ConwayEra where
  injectEvent = EpochEvent . injectEvent
