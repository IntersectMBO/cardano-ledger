{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Entities (
  DijkstraEntitiesPredFailure (..),
  DijkstraEntitiesEvent (..),
  conwayToDijkstraEntitiesPredFailure,
) where

import Cardano.Ledger.BaseTypes (Globals (networkId), Mismatch (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, ENTITIES)
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Rules.ValidationMode (runTest)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Lens.Micro

data DijkstraEntitiesPredFailure era
  = CertsFailure (PredicateFailure (EraRule "CERTS" era))
  | WdrlNotDelegatedToDRep (NonEmpty (KeyHash Staking))
  | WithdrawalsMissingAccounts Withdrawals
  | IncompleteWithdrawals (NonEmptyMap AccountAddress (Mismatch RelEQ Coin))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "CERTS" era)) => Eq (DijkstraEntitiesPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "CERTS" era)) => Show (DijkstraEntitiesPredFailure era)

instance
  NFData (PredicateFailure (EraRule "CERTS" era)) =>
  NFData (DijkstraEntitiesPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  ) =>
  EncCBOR (DijkstraEntitiesPredFailure era)
  where
  encCBOR =
    encode . \case
      CertsFailure x -> Sum (CertsFailure @era) 0 !> To x
      WdrlNotDelegatedToDRep x -> Sum (WdrlNotDelegatedToDRep @era) 1 !> To x
      WithdrawalsMissingAccounts x -> Sum (WithdrawalsMissingAccounts @era) 2 !> To x
      IncompleteWithdrawals x -> Sum (IncompleteWithdrawals @era) 3 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  ) =>
  DecCBOR (DijkstraEntitiesPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraEntitiesPredFailure" $ \case
    0 -> SumD CertsFailure <! From
    1 -> SumD WdrlNotDelegatedToDRep <! From
    2 -> SumD WithdrawalsMissingAccounts <! From
    3 -> SumD IncompleteWithdrawals <! From
    n -> Invalid n

newtype DijkstraEntitiesEvent era = CertsEvent (Event (EraRule "CERTS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "CERTS" era)) => Eq (DijkstraEntitiesEvent era)

instance NFData (Event (EraRule "CERTS" era)) => NFData (DijkstraEntitiesEvent era)

type instance EraRuleFailure "ENTITIES" DijkstraEra = DijkstraEntitiesPredFailure DijkstraEra

type instance EraRuleEvent "ENTITIES" DijkstraEra = DijkstraEntitiesEvent DijkstraEra

instance InjectRuleFailure "ENTITIES" DijkstraEntitiesPredFailure DijkstraEra

instance InjectRuleFailure "ENTITIES" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = CertsFailure

instance InjectRuleFailure "ENTITIES" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = CertsFailure . injectFailure @"CERTS"

instance InjectRuleFailure "ENTITIES" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = CertsFailure . injectFailure @"CERTS"

instance InjectRuleFailure "ENTITIES" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = CertsFailure . injectFailure @"CERTS"

instance InjectRuleFailure "ENTITIES" Conway.ConwayGovCertPredFailure DijkstraEra where
  injectFailure = CertsFailure . injectFailure @"CERTS"

instance InjectRuleFailure "ENTITIES" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = CertsFailure . injectFailure @"CERTS"

instance InjectRuleFailure "ENTITIES" Conway.ConwayLedgerPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraEntitiesPredFailure

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , Embed (EraRule "CERTS" era) (ENTITIES era)
  , State (EraRule "CERTS" era) ~ CertState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "CERTS" era) ~ Conway.CertsEnv era
  , EraRule "ENTITIES" era ~ ENTITIES era
  , InjectRuleFailure "ENTITIES" DijkstraEntitiesPredFailure era
  , InjectRuleFailure "ENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  STS (ENTITIES era)
  where
  type State (ENTITIES era) = CertState era
  type Signal (ENTITIES era) = Seq (TxCert era)
  type Environment (ENTITIES era) = Conway.CertsEnv era
  type BaseM (ENTITIES era) = ShelleyBase
  type PredicateFailure (ENTITIES era) = DijkstraEntitiesPredFailure era
  type Event (ENTITIES era) = DijkstraEntitiesEvent era

  initialRules = []
  transitionRules = [dijkstraEntitiesTransition @era]

dijkstraEntitiesTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  , Embed (EraRule "CERTS" era) (ENTITIES era)
  , State (EraRule "CERTS" era) ~ CertState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "CERTS" era) ~ Conway.CertsEnv era
  , EraRule "ENTITIES" era ~ ENTITIES era
  , InjectRuleFailure "ENTITIES" DijkstraEntitiesPredFailure era
  , InjectRuleFailure "ENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  TransitionRule (ENTITIES era)
dijkstraEntitiesTransition = do
  TRC (env, certState, certificates) <- judgmentContext
  let Conway.CertsEnv tx pp curEpoch _committee _committeeProposals = env
      withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      accounts = certState ^. certDStateL . accountsL

  runTest $ Conway.validateWithdrawalsDelegated accounts tx

  network <- liftSTS $ asks networkId
  let (missingWithdrawals, incompleteWithdrawals) =
        case withdrawalsThatDoNotDrainAccounts withdrawals network accounts of
          Nothing -> (Map.empty, Map.empty)
          Just (missing, incomplete) -> (unWithdrawals missing, incomplete)
  failOnNonEmptyMap missingWithdrawals $
    injectFailure . WithdrawalsMissingAccounts . Withdrawals . NEM.toMap
  failOnNonEmptyMap incompleteWithdrawals $ injectFailure . IncompleteWithdrawals

  let certState' =
        certState
          & Conway.updateDormantDRepExpiries tx curEpoch
          & Conway.updateVotingDRepExpiries tx curEpoch (pp ^. ppDRepActivityL)
          & certDStateL . accountsL %~ drainAccounts withdrawals
  trans @(EraRule "CERTS" era) $ TRC (env, certState', certificates)

conwayToDijkstraEntitiesPredFailure ::
  forall era. Conway.ConwayLedgerPredFailure era -> DijkstraEntitiesPredFailure era
conwayToDijkstraEntitiesPredFailure = \case
  Conway.ConwayWdrlNotDelegatedToDRep khs -> WdrlNotDelegatedToDRep khs
  Conway.ConwayUtxowFailure _ -> impossible "ConwayUtxowFailure"
  Conway.ConwayCertsFailure _ -> impossible "ConwayCertsFailure"
  Conway.ConwayGovFailure _ -> impossible "ConwayGovFailure"
  Conway.ConwayTreasuryValueMismatch _ -> impossible "ConwayTreasuryValueMismatch"
  Conway.ConwayTxRefScriptsSizeTooBig _ -> impossible "ConwayTxRefScriptsSizeTooBig"
  Conway.ConwayMempoolFailure _ -> impossible "ConwayMempoolFailure"
  Conway.ConwayWithdrawalsMissingAccounts _ -> impossible "ConwayWithdrawalsMissingAccounts"
  Conway.ConwayIncompleteWithdrawals _ -> impossible "ConwayIncompleteWithdrawals"
  where
    impossible name = error $ "Impossible: `" <> name <> "` for ENTITIES"

instance
  ( STS (Conway.CERTS era)
  , PredicateFailure (EraRule "CERTS" era) ~ Conway.ConwayCertsPredFailure era
  , Event (EraRule "CERTS" era) ~ Conway.ConwayCertsEvent era
  ) =>
  Embed (Conway.CERTS era) (ENTITIES era)
  where
  wrapFailed = CertsFailure
  wrapEvent = CertsEvent
