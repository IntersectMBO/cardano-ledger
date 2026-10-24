{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Eb (EB) where

import Cardano.Ledger.BaseTypes (Mismatch, Relation (..))
import Cardano.Ledger.Core (EraIndependentBlockHeader, EraRule, SafeHash)
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Shelley.LedgerState (EraCertState, NewEpochState)
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersEnv)
import Control.State.Transition.Extended (Embed, STS (..), TRC (..), judgmentContext, trans)

data EB era

data EbPredFailure era
  = EbAnnouncementHashMismatch (Mismatch RelEQ (SafeHash EraIndependentBlockHeader))
  | EbLedgersFailure (PredicateFailure (EraRule "EBLEDGERS" era))

deriving instance Eq (PredicateFailure (EraRule "EBLEDGERS" era)) => Eq (EbPredFailure era)

deriving instance Ord (PredicateFailure (EraRule "EBLEDGERS" era)) => Ord (EbPredFailure era)

deriving instance Show (PredicateFailure (EraRule "EBLEDGERS" era)) => Show (EbPredFailure era)

instance
  ( EraCertState era
  , Eq (PredicateFailure (EraRule "EBLEDGERS" era))
  , Ord (PredicateFailure (EraRule "EBLEDGERS" era))
  , Show (PredicateFailure (EraRule "EBLEDGERS" era))
  , Embed (EraRule "EBLEDGERS" era) (EB era)
  , Environment (EraRule "EBLEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "EBLEDGERS" era) ~ NewEpochState era
  , Signal (EraRule "EBLEDGERS" era) ~ EndorserBlock era
  ) =>
  STS (EB era)
  where
  type State (EB era) = NewEpochState era
  type Signal (EB era) = EndorserBlock era
  type Environment (EB era) = ShelleyLedgersEnv era

  type PredicateFailure (EB era) = EbPredFailure era

  initialRules = []
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        trans @(EraRule "EBLEDGERS" era) $ TRC (env, st, sig)
    ]

type instance EraRule "EB" DijkstraEra = EB DijkstraEra
