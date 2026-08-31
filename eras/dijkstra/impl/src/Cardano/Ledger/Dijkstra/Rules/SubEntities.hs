{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Cardano.Ledger.Dijkstra.Rules.SubEntities (
  SubEntitiesEnv (..),
  SubEntitiesPredFailure (..),
  SubEntitiesEvent (..),
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Committee,
  GovActionPurpose (..),
  GovActionState,
  GovPurposeId,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, SUBCERTS, SUBENTITIES)
import Cardano.Ledger.Dijkstra.Rules.Entities (
  EntitiesPredFailure (..),
  validateAccountBalanceIntervals,
  validateMissingAccountsInDirectDeposits,
  validateWrongNetworkInDirectDeposit,
 )
import Cardano.Ledger.Dijkstra.Rules.SubCerts (
  DijkstraSubCertsEvent,
  DijkstraSubCertsPredFailure,
  SubCertsEnv (..),
 )
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval)
import Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody,
  directDepositsTxBodyL,
 )
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)
import Lens.Micro

data SubEntitiesEnv era = SubEntitiesEnv
  { seeCurrentEpoch :: EpochNo
  , seePParams :: PParams era
  , seeCurrentCommittee :: StrictMaybe (Committee era)
  , seeCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose) (GovActionState era)
  , seeOriginalAccounts :: Accounts era
  }
  deriving (Generic)

deriving instance
  (EraPParams era, Eq (Committee era), Eq (GovActionState era), Eq (Accounts era)) =>
  Eq (SubEntitiesEnv era)

deriving instance
  (EraPParams era, Show (Committee era), Show (GovActionState era), Show (Accounts era)) =>
  Show (SubEntitiesEnv era)

instance
  (EraPParams era, NFData (Committee era), NFData (GovActionState era), NFData (Accounts era)) =>
  NFData (SubEntitiesEnv era)

instance
  ( EraPParams era
  , EncCBOR (Committee era)
  , EncCBOR (GovActionState era)
  , EncCBOR (Accounts era)
  ) =>
  EncCBOR (SubEntitiesEnv era)
  where
  encCBOR x@(SubEntitiesEnv _ _ _ _ _) =
    let SubEntitiesEnv {..} = x
     in encode $
          Rec SubEntitiesEnv
            !> To seeCurrentEpoch
            !> To seePParams
            !> To seeCurrentCommittee
            !> To seeCommitteeProposals
            !> To seeOriginalAccounts

data SubEntitiesPredFailure era
  = SubCertsFailure (PredicateFailure (EraRule "SUBCERTS" era))
  | SubWithdrawalAddressesWithWrongNetwork
      -- | Expected network id
      Network
      -- | Withdrawal account addresses with wrong network id
      (NonEmptySet AccountAddress)
  | SubWithdrawalAccountsMissing Withdrawals
  | SubWithdrawalAccountsMissingPreBatch Withdrawals
  | SubDirectDepositAddressesWithWrongNetwork
      -- | Expected network id
      Network
      -- | Direct-deposit account addresses with wrong network id
      (NonEmptySet AccountAddress)
  | SubDirectDepositAccountsMissing DirectDeposits
  | SubBalancesOutsideAccountBalanceIntervals
      (NonEmptyMap AccountAddress (Coin, AccountBalanceInterval era))
  | SubMissingAccountsInAccountBalanceIntervals
      (NonEmptyMap AccountAddress (AccountBalanceInterval era))
  | SubWrongNetworkInAccountBalanceIntervals Network (NonEmptySet AccountAddress)
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBCERTS" era)) => Eq (SubEntitiesPredFailure era)

deriving stock instance
  Ord (PredicateFailure (EraRule "SUBCERTS" era)) => Ord (SubEntitiesPredFailure era)

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
      SubWithdrawalAddressesWithWrongNetwork expected wrongs -> Sum (SubWithdrawalAddressesWithWrongNetwork @era) 1 !> To expected !> To wrongs
      SubWithdrawalAccountsMissing x -> Sum (SubWithdrawalAccountsMissing @era) 2 !> To x
      SubWithdrawalAccountsMissingPreBatch x -> Sum (SubWithdrawalAccountsMissingPreBatch @era) 3 !> To x
      SubDirectDepositAddressesWithWrongNetwork expected wrongs -> Sum (SubDirectDepositAddressesWithWrongNetwork @era) 4 !> To expected !> To wrongs
      SubDirectDepositAccountsMissing x -> Sum (SubDirectDepositAccountsMissing @era) 5 !> To x
      SubBalancesOutsideAccountBalanceIntervals x -> Sum (SubBalancesOutsideAccountBalanceIntervals @era) 6 !> To x
      SubMissingAccountsInAccountBalanceIntervals x -> Sum (SubMissingAccountsInAccountBalanceIntervals @era) 7 !> To x
      SubWrongNetworkInAccountBalanceIntervals expected wrongs -> Sum (SubWrongNetworkInAccountBalanceIntervals @era) 8 !> To expected !> To wrongs

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBCERTS" era))
  ) =>
  DecCBOR (SubEntitiesPredFailure era)
  where
  decCBOR = decode . Summands "SubEntitiesPredFailure" $ \case
    0 -> SumD SubCertsFailure <! From
    1 -> SumD SubWithdrawalAddressesWithWrongNetwork <! From <! From
    2 -> SumD SubWithdrawalAccountsMissing <! From
    3 -> SumD SubWithdrawalAccountsMissingPreBatch <! From
    4 -> SumD SubDirectDepositAddressesWithWrongNetwork <! From <! From
    5 -> SumD SubDirectDepositAccountsMissing <! From
    6 -> SumD SubBalancesOutsideAccountBalanceIntervals <! From
    7 -> SumD SubMissingAccountsInAccountBalanceIntervals <! From
    8 -> SumD SubWrongNetworkInAccountBalanceIntervals <! From <! From
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

instance InjectRuleFailure "SUBENTITIES" EntitiesPredFailure DijkstraEra where
  injectFailure = entitiesToSubEntitiesPredFailure

instance InjectRuleFailure "SUBENTITIES" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = injectFailure @"SUBENTITIES" @EntitiesPredFailure . injectFailure @"ENTITIES"

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
  , InjectRuleFailure "SUBENTITIES" EntitiesPredFailure era
  , InjectRuleFailure "SUBENTITIES" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "SUBENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  STS (SUBENTITIES era)
  where
  type State (SUBENTITIES era) = CertState era
  type Signal (SUBENTITIES era) = Tx SubTx era
  type Environment (SUBENTITIES era) = SubEntitiesEnv era
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
  , InjectRuleFailure "SUBENTITIES" EntitiesPredFailure era
  , InjectRuleFailure "SUBENTITIES" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "SUBENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  TransitionRule (SUBENTITIES era)
dijkstraSubEntitiesTransition = do
  TRC (SubEntitiesEnv curEpoch pp committee committeeProposals originalAccounts, certState, tx) <-
    judgmentContext
  let withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      directDeposits = tx ^. bodyTxL . directDepositsTxBodyL
      accounts = certState ^. certDStateL . accountsL
      subCertsEnv = SubCertsEnv tx pp curEpoch committee committeeProposals

  network <- liftSTS $ asks networkId

  runTest $ Shelley.validateWrongNetworkWithdrawal network (tx ^. bodyTxL)
  runTest $ validateWrongNetworkInDirectDeposit network (tx ^. bodyTxL)
  runTest $ validateAccountBalanceIntervals network accounts (tx ^. bodyTxL)
  runTest $ validateMissingOriginalAccountsInWithdrawals withdrawals originalAccounts
  runTest $ validateMissingAccountsInWithdrawals withdrawals accounts

  let appliedWithdrawals = applyWithdrawals withdrawals accounts
  let certStateBeforeSubCerts =
        certState
          & Conway.updateDormantDRepExpiries tx curEpoch
          & Conway.updateVotingDRepExpiries tx curEpoch (pp ^. ppDRepActivityL)
          & certDStateL . accountsL .~ appliedWithdrawals
  certStateAfterSubCerts <-
    trans @(EraRule "SUBCERTS" era) $
      TRC (subCertsEnv, certStateBeforeSubCerts, StrictSeq.fromStrict $ tx ^. bodyTxL . certsTxBodyL)

  let accountsAfterSubCerts = certStateAfterSubCerts ^. certDStateL . accountsL
  runTest $ validateMissingAccountsInDirectDeposits directDeposits accountsAfterSubCerts

  let appliedDirectDeposits = applyDirectDeposits directDeposits accountsAfterSubCerts
  pure $ certStateAfterSubCerts & certDStateL . accountsL .~ appliedDirectDeposits

validateMissingOriginalAccountsInWithdrawals ::
  EraAccounts era =>
  Withdrawals ->
  Accounts era ->
  Test (SubEntitiesPredFailure era)
validateMissingOriginalAccountsInWithdrawals wdrls originalAccounts =
  failureOnJust
    (withdrawalsMissingAccounts wdrls originalAccounts)
    SubWithdrawalAccountsMissingPreBatch

validateMissingAccountsInWithdrawals ::
  EraAccounts era =>
  Withdrawals ->
  Accounts era ->
  Test (SubEntitiesPredFailure era)
validateMissingAccountsInWithdrawals wdrls accounts =
  failureOnJust
    (withdrawalsMissingAccounts wdrls accounts)
    SubWithdrawalAccountsMissing

conwayToDijkstraSubEntitiesPredFailure ::
  forall era. Conway.ConwayLedgerPredFailure era -> SubEntitiesPredFailure era
conwayToDijkstraSubEntitiesPredFailure = \case
  Conway.ConwayWdrlNotDelegatedToDRep _ -> impossible "ConwayWdrlNotDelegatedToDRep"
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

entitiesToSubEntitiesPredFailure ::
  EntitiesPredFailure era -> SubEntitiesPredFailure era
entitiesToSubEntitiesPredFailure = \case
  WithdrawalAddressesWithWrongNetwork net addrs -> SubWithdrawalAddressesWithWrongNetwork net addrs
  DirectDepositAddressesWithWrongNetwork net addrs -> SubDirectDepositAddressesWithWrongNetwork net addrs
  DirectDepositAccountsMissing dds -> SubDirectDepositAccountsMissing dds
  CertsFailure _ -> impossible "CertsFailure"
  WithdrawalAccountsMissing _ -> impossible "WithdrawalAccountsMissing"
  WithdrawalAmountsInexactInLegacyMode _ -> impossible "WithdrawalAmountsInexactInLegacyMode"
  WithdrawalAmountsExceedingOriginalBalance _ -> impossible "WithdrawalAmountsExceedingOriginalBalance"
  WrongNetworkInAccountBalanceIntervals net addrs -> SubWrongNetworkInAccountBalanceIntervals net addrs
  MissingAccountsInAccountBalanceIntervals x -> SubMissingAccountsInAccountBalanceIntervals x
  BalancesOutsideAccountBalanceIntervals x -> SubBalancesOutsideAccountBalanceIntervals x
  WrongNetworkInStartingAccountBalanceIntervals _ _ ->
    impossible "WrongNetworkInStartingAccountBalanceIntervals"
  MissingAccountsInStartingAccountBalanceIntervals _ ->
    impossible "MissingAccountsInStartingAccountBalanceIntervals"
  BalancesOutsideStartingAccountBalanceIntervals _ ->
    impossible "BalancesOutsideStartingAccountBalanceIntervals"
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
