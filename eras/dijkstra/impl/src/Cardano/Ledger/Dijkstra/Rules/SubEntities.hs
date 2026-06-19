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

module Cardano.Ledger.Dijkstra.Rules.SubEntities (
  DijkstraSubEntitiesPredFailure (..),
  DijkstraSubEntitiesEvent (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, SUBCERTS, SUBENTITIES)
import Cardano.Ledger.Dijkstra.Rules.SubCerts (
  DijkstraSubCertsEvent,
  DijkstraSubCertsPredFailure,
  SubCertsEnv,
 )
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.Sequence (Seq)
import GHC.Generics (Generic)

newtype DijkstraSubEntitiesPredFailure era
  = SubCertsFailure (PredicateFailure (EraRule "SUBCERTS" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBCERTS" era)) => Eq (DijkstraSubEntitiesPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBCERTS" era)) => Show (DijkstraSubEntitiesPredFailure era)

instance
  NFData (PredicateFailure (EraRule "SUBCERTS" era)) =>
  NFData (DijkstraSubEntitiesPredFailure era)

deriving newtype instance
  (Era era, EncCBOR (PredicateFailure (EraRule "SUBCERTS" era))) =>
  EncCBOR (DijkstraSubEntitiesPredFailure era)

deriving newtype instance
  (Era era, DecCBOR (PredicateFailure (EraRule "SUBCERTS" era))) =>
  DecCBOR (DijkstraSubEntitiesPredFailure era)

newtype DijkstraSubEntitiesEvent era = SubCertsEvent (Event (EraRule "SUBCERTS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBCERTS" era)) => Eq (DijkstraSubEntitiesEvent era)

instance NFData (Event (EraRule "SUBCERTS" era)) => NFData (DijkstraSubEntitiesEvent era)

type instance EraRuleFailure "SUBENTITIES" DijkstraEra = DijkstraSubEntitiesPredFailure DijkstraEra

type instance EraRuleEvent "SUBENTITIES" DijkstraEra = DijkstraSubEntitiesEvent DijkstraEra

instance InjectRuleFailure "SUBENTITIES" DijkstraSubEntitiesPredFailure DijkstraEra

instance InjectRuleFailure "SUBENTITIES" DijkstraSubCertsPredFailure DijkstraEra where
  injectFailure = SubCertsFailure

instance
  ( EraTx era
  , Embed (EraRule "SUBCERTS" era) (SUBENTITIES era)
  , State (EraRule "SUBCERTS" era) ~ CertState era
  , Signal (EraRule "SUBCERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "SUBCERTS" era) ~ SubCertsEnv era
  , EraRule "SUBENTITIES" era ~ SUBENTITIES era
  ) =>
  STS (SUBENTITIES era)
  where
  type State (SUBENTITIES era) = CertState era
  type Signal (SUBENTITIES era) = Seq (TxCert era)
  type Environment (SUBENTITIES era) = SubCertsEnv era
  type BaseM (SUBENTITIES era) = ShelleyBase
  type PredicateFailure (SUBENTITIES era) = DijkstraSubEntitiesPredFailure era
  type Event (SUBENTITIES era) = DijkstraSubEntitiesEvent era

  initialRules = []
  transitionRules = [dijkstraSubEntitiesTransition @era]

dijkstraSubEntitiesTransition ::
  forall era.
  ( Embed (EraRule "SUBCERTS" era) (SUBENTITIES era)
  , State (EraRule "SUBCERTS" era) ~ CertState era
  , Signal (EraRule "SUBCERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "SUBCERTS" era) ~ SubCertsEnv era
  ) =>
  TransitionRule (SUBENTITIES era)
dijkstraSubEntitiesTransition = do
  TRC (env, certState, certificates) <- judgmentContext
  trans @(EraRule "SUBCERTS" era) $ TRC (env, certState, certificates)

instance
  ( STS (SUBCERTS era)
  , PredicateFailure (EraRule "SUBCERTS" era) ~ DijkstraSubCertsPredFailure era
  , Event (EraRule "SUBCERTS" era) ~ DijkstraSubCertsEvent era
  ) =>
  Embed (SUBCERTS era) (SUBENTITIES era)
  where
  wrapFailed = SubCertsFailure
  wrapEvent = SubCertsEvent
