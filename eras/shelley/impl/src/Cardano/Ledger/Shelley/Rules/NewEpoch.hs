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

module Cardano.Ledger.Shelley.Rules.NewEpoch (
  NEWEPOCH,
  ShelleyNewEpochEvent (..),
  updateRewards,
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (BlocksMade),
  ShelleyBase,
  StrictMaybe (SJust, SNothing),
 )
import Cardano.Ledger.Coin (toDeltaCoin)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Rewards (Reward)
import Cardano.Ledger.Shelley.AdaPots (AdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (NEWEPOCH, ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules.Epoch
import Cardano.Ledger.Shelley.Rules.Mir (MIR, ShelleyMirEvent)
import Cardano.Ledger.Shelley.Rules.Rupd (RupdEvent (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.State
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.State.Transition
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data ShelleyNewEpochEvent era
  = DeltaRewardEvent (Event (EraRule "RUPD" era))
  | RestrainedRewards
      EpochNo
      (Map.Map (Credential Staking) (Set Reward))
      (Set (Credential Staking))
  | TotalRewardEvent
      EpochNo
      (Map.Map (Credential Staking) (Set Reward))
  | EpochEvent (Event (EraRule "EPOCH" era))
  | MirEvent (Event (EraRule "MIR" era))
  | TotalAdaPotsEvent AdaPots
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "EPOCH" era))
  , Eq (Event (EraRule "MIR" era))
  , Eq (Event (EraRule "RUPD" era))
  ) =>
  Eq (ShelleyNewEpochEvent era)

instance
  ( NFData (Event (EraRule "EPOCH" era))
  , NFData (Event (EraRule "MIR" era))
  , NFData (Event (EraRule "RUPD" era))
  ) =>
  NFData (ShelleyNewEpochEvent era)

type instance EraRuleEvent "NEWEPOCH" ShelleyEra = ShelleyNewEpochEvent ShelleyEra

instance
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "MIR" era) (NEWEPOCH era)
  , Embed (EraRule "EPOCH" era) (NEWEPOCH era)
  , Environment (EraRule "MIR" era) ~ ()
  , State (EraRule "MIR" era) ~ EpochState era
  , Signal (EraRule "MIR" era) ~ ()
  , Event (EraRule "RUPD" era) ~ RupdEvent
  , Environment (EraRule "EPOCH" era) ~ ()
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (EpochState era)
  , Default (State (EraRule "PPUP" era))
  , Default (PParams era)
  , Default (StashedAVVMAddresses era)
  ) =>
  STS (NEWEPOCH era)
  where
  type State (NEWEPOCH era) = NewEpochState era

  type Signal (NEWEPOCH era) = EpochNo

  type Environment (NEWEPOCH era) = ()

  type BaseM (NEWEPOCH era) = ShelleyBase
  type PredicateFailure (NEWEPOCH era) = Void
  type Event (NEWEPOCH era) = ShelleyNewEpochEvent era

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          def
          SNothing
          def
          def
    ]

  transitionRules = [newEpochTransition]

newEpochTransition ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "MIR" era) (NEWEPOCH era)
  , Embed (EraRule "EPOCH" era) (NEWEPOCH era)
  , Environment (EraRule "MIR" era) ~ ()
  , State (EraRule "MIR" era) ~ EpochState era
  , Signal (EraRule "MIR" era) ~ ()
  , Environment (EraRule "EPOCH" era) ~ ()
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Signal (EraRule "EPOCH" era) ~ EpochNo
  , Default (PParams era)
  , Default (StashedAVVMAddresses era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  , Default (State (EraRule "PPUP" era))
  ) =>
  TransitionRule (NEWEPOCH era)
newEpochTransition = do
  TRC
    ( _
      , src@(NewEpochState eNoL _ bcur es ru _pd _)
      , eNo
      ) <-
    judgmentContext
  if eNo /= succ eNoL
    then pure src
    else do
      es' <- case ru of
        SNothing -> pure es
        SJust p@(Pulsing _ _) -> do
          (ans, event) <- liftSTS (completeRupd p)
          tellReward (DeltaRewardEvent (RupdEvent eNo event))
          updateRewards es eNo ans
        SJust (Complete ru') -> updateRewards es eNo ru'
      es'' <- trans @(EraRule "MIR" era) $ TRC ((), es', ())
      es''' <- trans @(EraRule "EPOCH" era) $ TRC ((), es'', eNo)
      let adaPots = totalAdaPotsES es'''
      tellEvent $ TotalAdaPotsEvent adaPots
      let pd' = ssStakeMarkPoolDistr (esSnapshots es)
      -- The spec sets pd' with:
      -- pd' = calculatePoolDistr (ssStakeSet $ esSnapshots es'''),
      --
      -- This is equivalent to:
      -- pd' = ssStakeMarkPoolDistr (esSnapshots es)
      --
      -- since:
      --
      -- \* SNAP rotates `ssStakeMark` to `ssStakeSet`, so
      -- \* the `ssStakeSet` snapshot in es''' is `ssStakeMark` in es
      -- \* `ssStakeMarkPoolDistr` is computed by calling `calculatePoolDistr`
      --    on the `ssStakeMark` snapshot at the previous epoch boundary.
      -- \* RUPD does not alter `esSnaphots`
      -- \* MIR does not alter `esSnaphots`
      --
      -- This was done to memoize the per-pool stake distribution.
      -- See ADR-7.
      pure $
        src
          { nesEL = eNo
          , nesBprev = bcur
          , nesBcur = BlocksMade mempty
          , nesEs = es'''
          , nesRu = SNothing
          , nesPd = pd'
          }

-- | tell a RupdEvent as a DeltaRewardEvent only if the map is non-empty
tellReward ::
  Event (EraRule "RUPD" era) ~ RupdEvent =>
  ShelleyNewEpochEvent era ->
  Rule (NEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

instance
  ( STS (EPOCH era)
  , Event (EraRule "EPOCH" era) ~ ShelleyEpochEvent era
  ) =>
  Embed (EPOCH era) (NEWEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = EpochEvent

instance
  ( EraGov era
  , EraCertState era
  , Default (EpochState era)
  , Event (EraRule "MIR" era) ~ ShelleyMirEvent era
  ) =>
  Embed (MIR era) (NEWEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = MirEvent

-- ===========================================

updateRewards ::
  (EraGov era, EraCertState era) =>
  EpochState era ->
  EpochNo ->
  RewardUpdate ->
  Rule (NEWEPOCH era) 'Transition (EpochState era)
updateRewards es e ru'@(RewardUpdate dt dr rs_ df _) = do
  let totRs = sumRewards (es ^. prevPParamsEpochStateL . ppProtocolVersionL) rs_
   in assert (Val.isZero (dt <> (dr <> toDeltaCoin totRs <> df))) (pure ())
  let !(!es', filtered) = applyRUpdFiltered ru' es
  tellEvent $ RestrainedRewards e (frShelleyIgnored filtered) (frUnregistered filtered)
  -- This event (which is only generated once per epoch) must be generated even if the
  -- map is empty (db-sync depends on it).
  tellEvent $ TotalRewardEvent e (frRegistered filtered)
  pure es'
