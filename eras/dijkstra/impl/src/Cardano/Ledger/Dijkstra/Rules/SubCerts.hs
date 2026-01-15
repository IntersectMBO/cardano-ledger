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
  DijkstraSUBCERTS,
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
import Cardano.Ledger.Conway.Rules (CertEnv (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBCERT,
  DijkstraSUBCERTS,
  DijkstraSUBDELEG,
  DijkstraSUBGOVCERT,
  DijkstraSUBPOOL,
 )
import Cardano.Ledger.Dijkstra.Rules.SubCert (DijkstraSubCertPredFailure)
import Cardano.Ledger.Dijkstra.TxCert
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubCertsPredFailure era = SubCertFailure (PredicateFailure (EraRule "SUBCERT" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBCERT" era)) => Eq (DijkstraSubCertsPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBCERT" era)) => Show (DijkstraSubCertsPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "SUBCERT" era)) =>
  NoThunks (DijkstraSubCertsPredFailure era)

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

instance InjectRuleEvent "SUBCERTS" DijkstraSubCertsEvent DijkstraEra

newtype DijkstraSubCertsEvent era = SubCertEvent (Event (EraRule "SUBCERT" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBCERT" era)) => Eq (DijkstraSubCertsEvent era)

instance NFData (Event (EraRule "SUBCERT" era)) => NFData (DijkstraSubCertsEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , Embed (EraRule "SUBCERT" era) (DijkstraSUBCERTS era)
  ) =>
  STS (DijkstraSUBCERTS era)
  where
  type State (DijkstraSUBCERTS era) = CertState era
  type Signal (DijkstraSUBCERTS era) = Seq (TxCert era)
  type Environment (DijkstraSUBCERTS era) = SubCertsEnv era
  type BaseM (DijkstraSUBCERTS era) = ShelleyBase
  type PredicateFailure (DijkstraSUBCERTS era) = DijkstraSubCertsPredFailure era
  type Event (DijkstraSUBCERTS era) = DijkstraSubCertsEvent era

  transitionRules = [dijkstraSubCertsTransition @era]

dijkstraSubCertsTransition ::
  forall era.
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , Embed (EraRule "SUBCERT" era) (DijkstraSUBCERTS era)
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
        trans @(DijkstraSUBCERTS era) $ TRC (env, certState, gamma)
      trans @(EraRule "SUBCERT" era) $
        TRC (CertEnv pp currentEpoch committee committeeProposals, certStateRest, txCert)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  Embed (DijkstraSUBCERT era) (DijkstraSUBCERTS era)
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
