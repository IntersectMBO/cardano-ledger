{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
  ConwayNewEpochPredFailure (..),
  ConwayNewEpochEvent (..),
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (BlocksMade),
  Globals (securityParameter),
  ShelleyBase,
  StrictMaybe (SJust, SNothing),
 )
import Cardano.Ledger.Coin (toDeltaCoin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEPOCH, ConwayNEWEPOCH)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  ConwayGovState (..),
  epochStateDRepDistrL,
  freshDRepPulser,
  newEpochStateDRepDistrL,
 )
import Cardano.Ledger.Conway.Rules.Epoch (ConwayEpochEvent)
import Cardano.Ledger.Conway.Rules.Ratify (
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRepDistr (pulseDRepDistr)
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
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
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio)
import Data.Set (Set)
import Data.Word (Word64)
import Lens.Micro ((%~), (&), (.~), (^.))

newtype ConwayNewEpochPredFailure era
  = CorruptRewardUpdate
      (RewardUpdate (EraCrypto era)) -- The reward update which violates an invariant

deriving instance Eq (ConwayNewEpochPredFailure era)

deriving instance
  ( Show (PredicateFailure (EraRule "EPOCH" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  Show (ConwayNewEpochPredFailure era)

data ConwayNewEpochEvent era
  = DeltaRewardEvent !(Event (EraRule "RUPD" era))
  | RestrainedRewards
      !EpochNo
      !(Map.Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
      !(Set (Credential 'Staking (EraCrypto era)))
  | TotalRewardEvent !EpochNo !(Map.Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
  | EpochEvent !(Event (EraRule "EPOCH" era))
  | TotalAdaPotsEvent !AdaPots

instance
  ( EraTxOut era
  , ConwayEraGov era
  , Embed (EraRule "EPOCH" era) (ConwayNEWEPOCH era)
  , Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  , Environment (EraRule "EPOCH" era) ~ PoolDistr (EraCrypto era)
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
  ) =>
  STS (ConwayNEWEPOCH era)
  where
  type State (ConwayNEWEPOCH era) = NewEpochState era
  type Signal (ConwayNEWEPOCH era) = EpochNo
  type Environment (ConwayNEWEPOCH era) = ()
  type BaseM (ConwayNEWEPOCH era) = ShelleyBase
  type PredicateFailure (ConwayNEWEPOCH era) = ConwayNewEpochPredFailure era
  type Event (ConwayNEWEPOCH era) = ConwayNewEpochEvent era

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
  ( EraTxOut era
  , ConwayEraGov era
  , Embed (EraRule "EPOCH" era) (ConwayNEWEPOCH era)
  , Environment (EraRule "EPOCH" era) ~ PoolDistr (EraCrypto era)
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (StashedAVVMAddresses era)
  , Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  , Signal (EraRule "RATIFY" era) ~ RatifySignal era
  , State (EraRule "RATIFY" era) ~ RatifyState era
  , Environment (EraRule "RATIFY" era) ~ RatifyEnv era
  , GovState era ~ ConwayGovState era
  , Eq (PredicateFailure (EraRule "RATIFY" era))
  , Show (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  TransitionRule (ConwayNEWEPOCH era)
newEpochTransition = do
  TRC
    ( _
      , nes@(NewEpochState eL _ bcur es0 ru pd _)
      , eNo
      ) <-
    judgmentContext
  if eNo /= eL + 1
    then pure (nes & newEpochStateDRepDistrL %~ pulseDRepDistr)
    else do
      es1 <- case ru of
        SNothing -> pure es0
        SJust p@(Pulsing _ _) -> do
          (ans, event) <- liftSTS (completeRupd p)
          tellReward (DeltaRewardEvent (RupdEvent eNo event))
          updateRewards es0 eNo ans
        SJust (Complete ru') -> updateRewards es0 eNo ru'
      es2 <- trans @(EraRule "EPOCH" era) $ TRC (pd, es1, eNo)

      -- Initialize DRep pulser. We expect approximately 10*k-many blocks to be produced each epoch.
      -- Therefore to safely and evenly space out the DRep calculation, we divide
      -- the number of stake credentials by 8*k (8, rather than 10, to be sure we finish a bit early)
      k <- liftSTS $ asks securityParameter -- Maximum number of blocks we are allowed to roll back
      let stakeSize = Map.size (es2 ^. epochStateIncrStakeDistrL)
      let pulseSize = max 1 (ceiling ((fromIntegral stakeSize :: Ratio Word64) / (8 * (fromIntegral k :: Ratio Word64))))
      let es3 = es2 & epochStateDRepDistrL .~ freshDRepPulser pulseSize es2 -- Install a new (as yet unpulsed) DRepDistr pulser
      let adaPots = totalAdaPotsES es2
      tellEvent $ TotalAdaPotsEvent adaPots
      let pd' = ssStakeMarkPoolDistr (esSnapshots es0)
      -- See `ShelleyNEWEPOCH` for details on the implementation
      pure $
        nes
          { nesEL = eNo
          , nesBprev = bcur
          , nesBcur = BlocksMade mempty
          , nesEs = es3
          , nesRu = SNothing
          , nesPd = pd'
          }

-- | tell a RupdEvent as a DeltaRewardEvent only if the map is non-empty
tellReward ::
  Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era) =>
  ConwayNewEpochEvent era ->
  Rule (ConwayNEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

updateRewards ::
  EraGov era =>
  EpochState era ->
  EpochNo ->
  RewardUpdate (EraCrypto era) ->
  Rule (ConwayNEWEPOCH era) 'Transition (EpochState era)
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
  ( STS (ConwayNEWEPOCH era)
  , PredicateFailure (EraRule "NEWEPOCH" era) ~ ConwayNewEpochPredFailure era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  Embed (ConwayNEWEPOCH era) (ShelleyTICK era)
  where
  wrapFailed = NewEpochFailure
  wrapEvent = TickNewEpochEvent

instance
  ( STS (ConwayEPOCH era)
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  Embed (ConwayEPOCH era) (ConwayNEWEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = EpochEvent
