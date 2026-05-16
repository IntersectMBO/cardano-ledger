{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubCerts (
  SUBCERTS,
  SubCertsEnv (..),
  DijkstraSubCertsPredFailure (..),
  DijkstraSubCertsEvent (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBCERT,
  SUBCERTS,
 )
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Dijkstra.Rules.SubCert (DijkstraSubCertEvent, DijkstraSubCertPredFailure)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import GHC.Generics (Generic)

newtype DijkstraSubCertsPredFailure era = SubCertFailure (PredicateFailure (EraRule "SUBCERT" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBCERT" era)) => Eq (DijkstraSubCertsPredFailure era)

deriving stock instance
  Ord (PredicateFailure (EraRule "SUBCERT" era)) => Ord (DijkstraSubCertsPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBCERT" era)) => Show (DijkstraSubCertsPredFailure era)

instance
  NFData (PredicateFailure (EraRule "SUBCERT" era)) =>
  NFData (DijkstraSubCertsPredFailure era)

deriving newtype instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBCERT" era))
  ) =>
  EncCBOR (DijkstraSubCertsPredFailure era)

deriving newtype instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBCERT" era))
  ) =>
  DecCBOR (DijkstraSubCertsPredFailure era)

type instance EraRuleFailure "SUBCERTS" DijkstraEra = DijkstraSubCertsPredFailure DijkstraEra

type instance EraRuleEvent "SUBCERTS" DijkstraEra = DijkstraSubCertsEvent DijkstraEra

instance InjectRuleFailure "SUBCERTS" DijkstraSubCertsPredFailure DijkstraEra

instance InjectRuleFailure "SUBCERTS" DijkstraSubCertPredFailure DijkstraEra where
  injectFailure = SubCertFailure

instance InjectRuleFailure "SUBCERTS" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraSubCertsPredFailure @DijkstraEra

instance InjectRuleEvent "SUBCERTS" DijkstraSubCertsEvent DijkstraEra

newtype DijkstraSubCertsEvent era = SubCertEvent (Event (EraRule "SUBCERT" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBCERT" era)) => Eq (DijkstraSubCertsEvent era)

instance NFData (Event (EraRule "SUBCERT" era)) => NFData (DijkstraSubCertsEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ SUBCERTS era
  , EraRule "SUBCERT" era ~ SUBCERT era
  , Embed (EraRule "SUBCERT" era) (SUBCERTS era)
  ) =>
  STS (SUBCERTS era)
  where
  type State (SUBCERTS era) = CertState era
  type Signal (SUBCERTS era) = Seq (TxCert era)
  type Environment (SUBCERTS era) = SubCertsEnv era
  type BaseM (SUBCERTS era) = ShelleyBase
  type PredicateFailure (SUBCERTS era) = DijkstraSubCertsPredFailure era
  type Event (SUBCERTS era) = DijkstraSubCertsEvent era

  transitionRules = [dijkstraSubCertsTransition @era]

dijkstraSubCertsTransition ::
  forall era.
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ SUBCERTS era
  , EraRule "SUBCERT" era ~ SUBCERT era
  , Embed (EraRule "SUBCERT" era) (SUBCERTS era)
  ) =>
  TransitionRule (EraRule "SUBCERTS" era)
dijkstraSubCertsTransition = do
  TRC
    ( env@(SubCertsEnv _tx pp currentEpoch committee committeeProposals)
      , certState
      , certificates
      ) <-
    judgmentContext
  case certificates of
    Empty -> pure certState
    gamma :|> txCert -> do
      certStateRest <-
        trans @(SUBCERTS era) $ TRC (env, certState, gamma)
      trans @(EraRule "SUBCERT" era) $
        TRC (Conway.CertEnv pp currentEpoch committee committeeProposals, certStateRest, txCert)

instance
  ( STS (SUBCERT era)
  , PredicateFailure (EraRule "SUBCERT" era) ~ DijkstraSubCertPredFailure era
  , Event (EraRule "SUBCERT" era) ~ DijkstraSubCertEvent era
  ) =>
  Embed (SUBCERT era) (SUBCERTS era)
  where
  wrapFailed = SubCertFailure
  wrapEvent = SubCertEvent

-- TODO: instead of duplicating this, parameterize on TxLevel
data SubCertsEnv era = SubCertsEnv
  { certsTx :: Tx SubTx era
  , certsPParams :: PParams era
  , certsCurrentEpoch :: EpochNo
  -- ^ Lazy on purpose, because not all certificates need to know the current EpochNo
  , certsCurrentCommittee :: StrictMaybe (Committee era)
  , certsCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose) (GovActionState era)
  }
  deriving (Generic)

instance EraTx era => EncCBOR (SubCertsEnv era) where
  encCBOR x@(SubCertsEnv _ _ _ _ _) =
    let SubCertsEnv {..} = x
     in encode $
          Rec SubCertsEnv
            !> To certsTx
            !> To certsPParams
            !> To certsCurrentEpoch
            !> To certsCurrentCommittee
            !> To certsCommitteeProposals

deriving instance (EraPParams era, Eq (Tx SubTx era)) => Eq (SubCertsEnv era)

deriving instance (EraPParams era, Show (Tx SubTx era)) => Show (SubCertsEnv era)

instance (EraPParams era, NFData (Tx SubTx era)) => NFData (SubCertsEnv era)

conwayToDijkstraSubCertsPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBCERT" Conway.ConwayCertPredFailure era
  , PredicateFailure (EraRule "CERT" era) ~ Conway.ConwayCertPredFailure era
  ) =>
  Conway.ConwayCertsPredFailure era -> DijkstraSubCertsPredFailure era
conwayToDijkstraSubCertsPredFailure = \case
  Conway.WithdrawalsNotInRewardsCERTS _ -> error "Impossible: `WithdrawalsNotInRewardsCERTS` for SUBCERTS"
  Conway.CertFailure f -> SubCertFailure (injectFailure @"SUBCERT" f)
