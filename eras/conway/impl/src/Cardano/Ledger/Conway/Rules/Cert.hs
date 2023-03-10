{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Cert (
  ConwayCERT,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayCERT)
import Cardano.Ledger.Shelley.API (CertState, DelplEnv)
import Cardano.Ledger.Shelley.Rules (ShelleyDelplEvent, ShelleyDelplPredFailure)
import Control.State.Transition.Extended (STS (..))

instance
  ( Era era
  , Eq (PredicateFailure (EraRule "DELEG" era))
  , Show (PredicateFailure (EraRule "DELEG" era))
  , Eq (PredicateFailure (EraRule "POOL" era))
  , Show (PredicateFailure (EraRule "POOL" era))
  ) =>
  STS (ConwayCERT era)
  where
  type State (ConwayCERT era) = CertState era
  type Signal (ConwayCERT era) = DCert era
  type Environment (ConwayCERT era) = DelplEnv era
  type BaseM (ConwayCERT era) = ShelleyBase
  type PredicateFailure (ConwayCERT era) = ShelleyDelplPredFailure era
  type Event (ConwayCERT era) = ShelleyDelplEvent era

  transitionRules = undefined
