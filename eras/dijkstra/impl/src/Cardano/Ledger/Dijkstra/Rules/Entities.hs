{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Entities (
  EntitiesPredFailure (..),
  EntitiesEvent (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, ENTITIES)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.Sequence (Seq)
import GHC.Generics (Generic)

newtype EntitiesPredFailure era
  = CertsFailure (PredicateFailure (EraRule "CERTS" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "CERTS" era)) => Eq (EntitiesPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "CERTS" era)) => Show (EntitiesPredFailure era)

instance
  NFData (PredicateFailure (EraRule "CERTS" era)) =>
  NFData (EntitiesPredFailure era)

deriving newtype instance
  (Era era, EncCBOR (PredicateFailure (EraRule "CERTS" era))) =>
  EncCBOR (EntitiesPredFailure era)

deriving newtype instance
  (Era era, DecCBOR (PredicateFailure (EraRule "CERTS" era))) =>
  DecCBOR (EntitiesPredFailure era)

newtype EntitiesEvent era = CertsEvent (Event (EraRule "CERTS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "CERTS" era)) => Eq (EntitiesEvent era)

instance NFData (Event (EraRule "CERTS" era)) => NFData (EntitiesEvent era)

type instance EraRuleFailure "ENTITIES" DijkstraEra = EntitiesPredFailure DijkstraEra

type instance EraRuleEvent "ENTITIES" DijkstraEra = EntitiesEvent DijkstraEra

instance InjectRuleFailure "ENTITIES" EntitiesPredFailure DijkstraEra

instance InjectRuleFailure "ENTITIES" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = CertsFailure

instance
  ( EraTx era
  , Embed (EraRule "CERTS" era) (ENTITIES era)
  , State (EraRule "CERTS" era) ~ CertState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "CERTS" era) ~ Conway.CertsEnv era
  , EraRule "ENTITIES" era ~ ENTITIES era
  ) =>
  STS (ENTITIES era)
  where
  type State (ENTITIES era) = CertState era
  type Signal (ENTITIES era) = Seq (TxCert era)
  type Environment (ENTITIES era) = Conway.CertsEnv era
  type BaseM (ENTITIES era) = ShelleyBase
  type PredicateFailure (ENTITIES era) = EntitiesPredFailure era
  type Event (ENTITIES era) = EntitiesEvent era

  initialRules = []
  transitionRules = [dijkstraEntitiesTransition @era]

dijkstraEntitiesTransition ::
  forall era.
  ( Embed (EraRule "CERTS" era) (ENTITIES era)
  , State (EraRule "CERTS" era) ~ CertState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "CERTS" era) ~ Conway.CertsEnv era
  ) =>
  TransitionRule (ENTITIES era)
dijkstraEntitiesTransition = do
  TRC (env, certState, certificates) <- judgmentContext
  trans @(EraRule "CERTS" era) $ TRC (env, certState, certificates)

instance
  ( STS (Conway.CERTS era)
  , PredicateFailure (EraRule "CERTS" era) ~ Conway.ConwayCertsPredFailure era
  , Event (EraRule "CERTS" era) ~ Conway.ConwayCertsEvent era
  ) =>
  Embed (Conway.CERTS era) (ENTITIES era)
  where
  wrapFailed = CertsFailure
  wrapEvent = CertsEvent
