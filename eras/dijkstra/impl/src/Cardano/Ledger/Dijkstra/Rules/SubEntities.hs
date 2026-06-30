{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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

module Cardano.Ledger.Dijkstra.Rules.SubEntities (
  SubEntitiesPredFailure (..),
  SubEntitiesEvent (..),
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.BaseTypes (Globals (networkId), Mismatch (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, SUBCERTS, SUBENTITIES)
import Cardano.Ledger.Dijkstra.Rules.SubCerts (
  DijkstraSubCertsEvent,
  DijkstraSubCertsPredFailure,
  SubCertsEnv (..),
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody, directDepositsTxBodyL)
import Cardano.Ledger.Rules.ValidationMode (runTest)
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

data SubEntitiesPredFailure era
  = SubCertsFailure (PredicateFailure (EraRule "SUBCERTS" era))
  | SubWdrlNotDelegatedToDRep (NonEmpty (KeyHash Staking))
  | SubWithdrawalsMissingAccounts Withdrawals
  | SubWithdrawalAmountsExceedAccountBalances (NonEmptyMap AccountAddress (Mismatch RelLTEQ Coin))
  | SubDirectDepositsToMissingAccounts DirectDeposits
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBCERTS" era)) => Eq (SubEntitiesPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBCERTS" era)) => Show (SubEntitiesPredFailure era)

instance
  NFData (PredicateFailure (EraRule "SUBCERTS" era)) =>
  NFData (SubEntitiesPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBCERTS" era))
  ) =>
  EncCBOR (SubEntitiesPredFailure era)
  where
  encCBOR =
    encode . \case
      SubCertsFailure x -> Sum (SubCertsFailure @era) 0 !> To x
      SubWdrlNotDelegatedToDRep x -> Sum (SubWdrlNotDelegatedToDRep @era) 1 !> To x
      SubWithdrawalsMissingAccounts x -> Sum (SubWithdrawalsMissingAccounts @era) 2 !> To x
      SubWithdrawalAmountsExceedAccountBalances x -> Sum (SubWithdrawalAmountsExceedAccountBalances @era) 3 !> To x
      SubDirectDepositsToMissingAccounts x -> Sum (SubDirectDepositsToMissingAccounts @era) 4 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBCERTS" era))
  ) =>
  DecCBOR (SubEntitiesPredFailure era)
  where
  decCBOR = decode . Summands "SubEntitiesPredFailure" $ \case
    0 -> SumD SubCertsFailure <! From
    1 -> SumD SubWdrlNotDelegatedToDRep <! From
    2 -> SumD SubWithdrawalsMissingAccounts <! From
    3 -> SumD SubWithdrawalAmountsExceedAccountBalances <! From
    4 -> SumD SubDirectDepositsToMissingAccounts <! From
    n -> Invalid n

newtype SubEntitiesEvent era = SubCertsEvent (Event (EraRule "SUBCERTS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBCERTS" era)) => Eq (SubEntitiesEvent era)

instance NFData (Event (EraRule "SUBCERTS" era)) => NFData (SubEntitiesEvent era)

type instance EraRuleFailure "SUBENTITIES" DijkstraEra = SubEntitiesPredFailure DijkstraEra

type instance EraRuleEvent "SUBENTITIES" DijkstraEra = SubEntitiesEvent DijkstraEra

instance InjectRuleFailure "SUBENTITIES" SubEntitiesPredFailure DijkstraEra

instance InjectRuleFailure "SUBENTITIES" DijkstraSubCertsPredFailure DijkstraEra where
  injectFailure = SubCertsFailure

instance InjectRuleFailure "SUBENTITIES" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = SubCertsFailure . injectFailure @"SUBCERTS"

instance InjectRuleFailure "SUBENTITIES" Conway.ConwayLedgerPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraSubEntitiesPredFailure

instance
  ( EraTx era
  , DijkstraEraTxBody era
  , ConwayEraCertState era
  , Embed (EraRule "SUBCERTS" era) (SUBENTITIES era)
  , State (EraRule "SUBCERTS" era) ~ CertState era
  , Signal (EraRule "SUBCERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "SUBCERTS" era) ~ SubCertsEnv era
  , EraRule "SUBENTITIES" era ~ SUBENTITIES era
  , InjectRuleFailure "SUBENTITIES" SubEntitiesPredFailure era
  , InjectRuleFailure "SUBENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  STS (SUBENTITIES era)
  where
  type State (SUBENTITIES era) = CertState era
  type Signal (SUBENTITIES era) = Seq (TxCert era)
  type Environment (SUBENTITIES era) = SubCertsEnv era
  type BaseM (SUBENTITIES era) = ShelleyBase
  type PredicateFailure (SUBENTITIES era) = SubEntitiesPredFailure era
  type Event (SUBENTITIES era) = SubEntitiesEvent era

  initialRules = []
  transitionRules = [dijkstraSubEntitiesTransition @era]

dijkstraSubEntitiesTransition ::
  forall era.
  ( EraTx era
  , DijkstraEraTxBody era
  , ConwayEraCertState era
  , Embed (EraRule "SUBCERTS" era) (SUBENTITIES era)
  , State (EraRule "SUBCERTS" era) ~ CertState era
  , Signal (EraRule "SUBCERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "SUBCERTS" era) ~ SubCertsEnv era
  , EraRule "SUBENTITIES" era ~ SUBENTITIES era
  , InjectRuleFailure "SUBENTITIES" SubEntitiesPredFailure era
  , InjectRuleFailure "SUBENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  TransitionRule (SUBENTITIES era)
dijkstraSubEntitiesTransition = do
  TRC (subCertsEnv, certState, certificates) <- judgmentContext
  let tx = certsTx subCertsEnv
      withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      accounts = certState ^. certDStateL . accountsL

  runTest $ Conway.validateWithdrawalsDelegated accounts tx

  network <- liftSTS $ asks networkId
  let (missingWithdrawals, exceededWithdrawals) =
        case withdrawalsThatExceedAccountBalance withdrawals network accounts of
          Nothing -> (Map.empty, Map.empty)
          Just (missing, exceeded) -> (unWithdrawals missing, exceeded)
  failOnNonEmptyMap missingWithdrawals $
    injectFailure . SubWithdrawalsMissingAccounts . Withdrawals . NEM.toMap
  failOnNonEmptyMap exceededWithdrawals $ injectFailure . SubWithdrawalAmountsExceedAccountBalances

  let certStateBeforeSubCerts =
        certState
          & certDStateL . accountsL %~ applyWithdrawals withdrawals
  certStateAfterSubCerts <-
    trans @(EraRule "SUBCERTS" era) $ TRC (subCertsEnv, certStateBeforeSubCerts, certificates)

  let directDeposits = tx ^. bodyTxL . directDepositsTxBodyL
      accountsAfterSubCerts = certStateAfterSubCerts ^. certDStateL . accountsL
  failOnJust (directDepositsMissingAccounts directDeposits accountsAfterSubCerts) $
    injectFailure . SubDirectDepositsToMissingAccounts

  pure $ certStateAfterSubCerts & certDStateL . accountsL %~ applyDirectDeposits directDeposits

conwayToDijkstraSubEntitiesPredFailure ::
  forall era. Conway.ConwayLedgerPredFailure era -> SubEntitiesPredFailure era
conwayToDijkstraSubEntitiesPredFailure = \case
  Conway.ConwayWdrlNotDelegatedToDRep khs -> SubWdrlNotDelegatedToDRep khs
  Conway.ConwayUtxowFailure _ -> impossible "ConwayUtxowFailure"
  Conway.ConwayCertsFailure _ -> impossible "ConwayCertsFailure"
  Conway.ConwayGovFailure _ -> impossible "ConwayGovFailure"
  Conway.ConwayTreasuryValueMismatch _ -> impossible "ConwayTreasuryValueMismatch"
  Conway.ConwayTxRefScriptsSizeTooBig _ -> impossible "ConwayTxRefScriptsSizeTooBig"
  Conway.ConwayMempoolFailure _ -> impossible "ConwayMempoolFailure"
  Conway.ConwayWithdrawalsMissingAccounts _ -> impossible "ConwayWithdrawalsMissingAccounts"
  Conway.ConwayIncompleteWithdrawals _ -> impossible "ConwayIncompleteWithdrawals"
  where
    impossible name = error $ "Impossible: `" <> name <> "` for SUBENTITIES"

instance
  ( STS (SUBCERTS era)
  , PredicateFailure (EraRule "SUBCERTS" era) ~ DijkstraSubCertsPredFailure era
  , Event (EraRule "SUBCERTS" era) ~ DijkstraSubCertsEvent era
  ) =>
  Embed (SUBCERTS era) (SUBENTITIES era)
  where
  wrapFailed = SubCertsFailure
  wrapEvent = SubCertsEvent
