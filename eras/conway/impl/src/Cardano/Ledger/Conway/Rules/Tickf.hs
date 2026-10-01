{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Like TICK, called only by consensus. But, ticks ledger state to a __future__ slot.
module Cardano.Ledger.Conway.Rules.Tickf (
  TICKF,
  ConwayTickfEvent (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo, ShelleyBase, SlotNo)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core (Era, EraRule)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (SnapShots)
import Control.DeepSeq (NFData)
import Control.State.Transition
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))

newtype ConwayTickfEvent era
  = TickfSnapEvent (Event (EraRule "SNAP" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SNAP" era)) => Eq (ConwayTickfEvent era)

instance NFData (Event (EraRule "SNAP" era)) => NFData (ConwayTickfEvent era)

instance
  ( EraGov era
  , State (EraRule "SNAP" era) ~ SnapShots era
  , Environment (EraRule "SNAP" era) ~ Shelley.SnapEnv era
  , Signal (EraRule "SNAP" era) ~ EpochNo
  , Embed (EraRule "SNAP" era) (TICKF era)
  ) =>
  STS (TICKF era)
  where
  type State (TICKF era) = NewEpochState era
  type Signal (TICKF era) = SlotNo
  type Environment (TICKF era) = ()
  type BaseM (TICKF era) = ShelleyBase
  type PredicateFailure (TICKF era) = Void
  type Event (TICKF era) = ConwayTickfEvent era

  initialRules = []
  transitionRules = pure $ do
    TRC ((), nes0, slot) <- judgmentContext
    -- This whole function is a specialization of an inlined 'NEWEPOCH'.
    --
    -- The forecast is built entirely from the stake pool distribution in the set snapshot and
    -- the current protocol parameters, so the correctness of this rule only depends on getting
    -- these two correct.

    (curEpochNo, nes) <- liftSTS $ Shelley.solidifyNextEpochPParams nes0 slot

    if curEpochNo /= succ (nesEL nes)
      then pure nes
      else do
        let es = nesEs nes
            pp = es ^. curPParamsEpochStateL
            ls = esLState es
            ss = esSnapshots es
            govState = nes ^. newEpochStateGovStateL

        ss' <-
          trans @(EraRule "SNAP" era) $ TRC (Shelley.SnapEnv ls pp, ss, curEpochNo)

        pure $!
          nes
            & nesEsL . esSnapshotsL .~ ss'
            & newEpochStateGovStateL . curPParamsGovStateL .~ nextEpochPParams govState
            & newEpochStateGovStateL . prevPParamsGovStateL .~ (govState ^. curPParamsGovStateL)
            & newEpochStateGovStateL . futurePParamsGovStateL .~ NoPParamsUpdate

instance
  ( Era era
  , STS (Shelley.SNAP era)
  , Event (EraRule "SNAP" era) ~ Shelley.SnapEvent era
  ) =>
  Embed (Shelley.SNAP era) (TICKF era)
  where
  wrapFailed = \case {}
  wrapEvent = TickfSnapEvent
