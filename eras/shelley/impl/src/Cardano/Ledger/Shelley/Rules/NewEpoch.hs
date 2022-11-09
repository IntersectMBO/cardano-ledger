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
  ( BlocksMade (BlocksMade),
    ProtVer,
    ShelleyBase,
    StrictMaybe (SJust, SNothing),
  )
import Cardano.Ledger.Coin (toDeltaCoin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Shelley.AdaPots (AdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (Reward, sumRewards)
import Cardano.Ledger.Shelley.Rules.Epoch
import Cardano.Ledger.Shelley.Rules.Mir
import Cardano.Ledger.Shelley.Rules.Rupd (RupdEvent (..))
import Cardano.Ledger.Slot
import qualified Cardano.Ledger.Val as Val
import Control.Provenance (runProvM)
import Control.State.Transition
import Data.Default.Class (Default, def)
import qualified Data.Map.Strict as Map
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
  = DeltaRewardEvent (Event (Core.EraRule "RUPD" era))
  | RestrainedRewards EpochNo (Map.Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era)))) (Set (Credential 'Staking (Crypto era)))
  | TotalRewardEvent EpochNo (Map.Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))))
  | EpochEvent (Event (Core.EraRule "EPOCH" era))
  | MirEvent (Event (Core.EraRule "MIR" era))
  | TotalAdaPotsEvent AdaPots

instance
  ( UsesTxOut era,
    UsesValue era,
    Embed (Core.EraRule "MIR" era) (NEWEPOCH era),
    Embed (Core.EraRule "EPOCH" era) (NEWEPOCH era),
    Environment (Core.EraRule "MIR" era) ~ (),
    State (Core.EraRule "MIR" era) ~ EpochState era,
    Signal (Core.EraRule "MIR" era) ~ (),
    Event (Core.EraRule "RUPD" era) ~ RupdEvent (Crypto era),
    Environment (Core.EraRule "EPOCH" era) ~ (),
    State (Core.EraRule "EPOCH" era) ~ EpochState era,
    Signal (Core.EraRule "EPOCH" era) ~ EpochNo,
    Default (EpochState era),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    Default (State (Core.EraRule "PPUP" era)),
    Default (Core.PParams era),
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
  ( Embed (Core.EraRule "MIR" era) (NEWEPOCH era),
    Embed (Core.EraRule "EPOCH" era) (NEWEPOCH era),
    Event (Core.EraRule "RUPD" era) ~ RupdEvent (Crypto era),
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
    Default (Core.PParams era),
    Default (StashedAVVMAddresses era),
    Event (Core.EraRule "RUPD" era) ~ RupdEvent (Crypto era)
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
          (ans, event) <- liftSTS (runProvM $ completeRupd p)
          tellReward (DeltaRewardEvent (RupdEvent e event))
          updateRewards ans
        SJust (Complete ru') -> updateRewards ru'
      es'' <- trans @(Core.EraRule "MIR" era) $ TRC ((), es', ())
      es''' <- trans @(Core.EraRule "EPOCH" era) $ TRC ((), es'', e)
      let adaPots = totalAdaPotsES es'''
      tellEvent $ TotalAdaPotsEvent adaPots
      let pd' = _pstakeMarkPoolDistr (esSnapshots es)
      -- RUPD does not alter `esSnaphots`
      -- MIR does not alter `esSnaphots`
      -- SNAP rotates mark to set.
      --
      -- Thus: pd' = calcPoolDistr $ _pstakeSet $ esSnapshots es'''

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
tellReward :: (Event (Core.EraRule "RUPD" era) ~ RupdEvent (Crypto era)) => NewEpochEvent era -> Rule (NEWEPOCH era) rtype ()
tellReward (DeltaRewardEvent (RupdEvent _ m)) | Map.null m = pure ()
tellReward x = tellEvent x

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
