{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.Dijkstra.Rules.Entities (
  EntitiesEnv (..),
  EntitiesPredFailure (..),
  EntitiesEvent (..),
  validateWrongNetworkInDirectDeposit,
  validateMissingAccountsInDirectDeposits,
  validateAccountBalanceIntervals,
  validateStartingAccountBalanceIntervals,
) where

import Cardano.Ledger.Address (DirectDeposits (..), accountAddressCredentialL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Committee,
  GovActionPurpose (..),
  GovActionState,
  GovPurposeId,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, ENTITIES)
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval (..), AccountBalanceIntervals (..))
import Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody,
  accountBalanceIntervalsTxBodyL,
  directDepositsTxBodyL,
  startingAccountBalanceIntervalsTxBodyL,
 )
import Cardano.Ledger.Dijkstra.UTxO (DijkstraEraUTxO (..))
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Foldable (sequenceA_)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)
import Lens.Micro

data EntitiesEnv era = EntitiesEnv
  { eeCurrentEpoch :: EpochNo
  , eePParams :: PParams era
  , eeCurrentCommittee :: StrictMaybe (Committee era)
  , eeCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose) (GovActionState era)
  , eeOriginalAccounts :: Accounts era
  }
  deriving (Generic)

deriving instance
  (EraPParams era, Eq (Committee era), Eq (GovActionState era), Eq (Accounts era)) =>
  Eq (EntitiesEnv era)

deriving instance
  (EraPParams era, Show (Committee era), Show (GovActionState era), Show (Accounts era)) =>
  Show (EntitiesEnv era)

instance
  (EraPParams era, NFData (Committee era), NFData (GovActionState era), NFData (Accounts era)) =>
  NFData (EntitiesEnv era)

instance
  ( EraPParams era
  , EncCBOR (Committee era)
  , EncCBOR (GovActionState era)
  , EncCBOR (Accounts era)
  ) =>
  EncCBOR (EntitiesEnv era)
  where
  encCBOR x@(EntitiesEnv _ _ _ _ _) =
    let EntitiesEnv {..} = x
     in encode $
          Rec EntitiesEnv
            !> To eeCurrentEpoch
            !> To eePParams
            !> To eeCurrentCommittee
            !> To eeCommitteeProposals
            !> To eeOriginalAccounts

data EntitiesPredFailure era
  = CertsFailure (PredicateFailure (EraRule "CERTS" era))
  | WithdrawalAddressesWithWrongNetwork
      -- | Expected network id
      Network
      -- | Withdrawal account addresses with wrong network id
      (NonEmptySet AccountAddress)
  | WithdrawalAccountsMissing Withdrawals
  | WithdrawalAmountsInexactInLegacyMode (NonEmptyMap AccountAddress (Mismatch RelEQ Coin))
  | WithdrawalAmountsExceedingOriginalBalance (NonEmptyMap AccountAddress (Mismatch RelLTEQ Coin))
  | DirectDepositAddressesWithWrongNetwork
      -- | Expected network id
      Network
      -- | Direct-deposit account addresses with wrong network id
      (NonEmptySet AccountAddress)
  | DirectDepositAccountsMissing DirectDeposits
  | WrongNetworkInAccountBalanceIntervals Network (NonEmptySet AccountAddress)
  | MissingAccountsInAccountBalanceIntervals (NonEmptyMap AccountAddress (AccountBalanceInterval era))
  | BalancesOutsideAccountBalanceIntervals
      (NonEmptyMap AccountAddress (Coin, AccountBalanceInterval era))
  | WrongNetworkInStartingAccountBalanceIntervals Network (NonEmptySet AccountAddress)
  | MissingAccountsInStartingAccountBalanceIntervals
      (NonEmptyMap AccountAddress (AccountBalanceInterval era))
  | BalancesOutsideStartingAccountBalanceIntervals
      (NonEmptyMap AccountAddress (Coin, AccountBalanceInterval era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "CERTS" era)) => Eq (EntitiesPredFailure era)

deriving stock instance
  Ord (PredicateFailure (EraRule "CERTS" era)) => Ord (EntitiesPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "CERTS" era)) => Show (EntitiesPredFailure era)

instance
  NFData (PredicateFailure (EraRule "CERTS" era)) =>
  NFData (EntitiesPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  ) =>
  EncCBOR (EntitiesPredFailure era)
  where
  encCBOR =
    encode . \case
      CertsFailure x -> Sum (CertsFailure @era) 0 !> To x
      WithdrawalAddressesWithWrongNetwork x y -> Sum (WithdrawalAddressesWithWrongNetwork @era) 1 !> To x !> To y
      WithdrawalAccountsMissing x -> Sum (WithdrawalAccountsMissing @era) 2 !> To x
      WithdrawalAmountsInexactInLegacyMode x -> Sum (WithdrawalAmountsInexactInLegacyMode @era) 3 !> To x
      WithdrawalAmountsExceedingOriginalBalance x -> Sum (WithdrawalAmountsExceedingOriginalBalance @era) 4 !> To x
      DirectDepositAddressesWithWrongNetwork x y -> Sum (DirectDepositAddressesWithWrongNetwork @era) 5 !> To x !> To y
      DirectDepositAccountsMissing x -> Sum (DirectDepositAccountsMissing @era) 6 !> To x
      WrongNetworkInAccountBalanceIntervals x y -> Sum (WrongNetworkInAccountBalanceIntervals @era) 7 !> To x !> To y
      MissingAccountsInAccountBalanceIntervals x -> Sum (MissingAccountsInAccountBalanceIntervals @era) 8 !> To x
      BalancesOutsideAccountBalanceIntervals x -> Sum (BalancesOutsideAccountBalanceIntervals @era) 9 !> To x
      WrongNetworkInStartingAccountBalanceIntervals x y -> Sum (WrongNetworkInStartingAccountBalanceIntervals @era) 10 !> To x !> To y
      MissingAccountsInStartingAccountBalanceIntervals x -> Sum (MissingAccountsInStartingAccountBalanceIntervals @era) 11 !> To x
      BalancesOutsideStartingAccountBalanceIntervals x -> Sum (BalancesOutsideStartingAccountBalanceIntervals @era) 12 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  ) =>
  DecCBOR (EntitiesPredFailure era)
  where
  decCBOR = decode . Summands "EntitiesPredFailure" $ \case
    0 -> SumD CertsFailure <! From
    1 -> SumD WithdrawalAddressesWithWrongNetwork <! From <! From
    2 -> SumD WithdrawalAccountsMissing <! From
    3 -> SumD WithdrawalAmountsInexactInLegacyMode <! From
    4 -> SumD WithdrawalAmountsExceedingOriginalBalance <! From
    5 -> SumD DirectDepositAddressesWithWrongNetwork <! From <! From
    6 -> SumD DirectDepositAccountsMissing <! From
    7 -> SumD WrongNetworkInAccountBalanceIntervals <! From <! From
    8 -> SumD MissingAccountsInAccountBalanceIntervals <! From
    9 -> SumD BalancesOutsideAccountBalanceIntervals <! From
    10 -> SumD WrongNetworkInStartingAccountBalanceIntervals <! From <! From
    11 -> SumD MissingAccountsInStartingAccountBalanceIntervals <! From
    12 -> SumD BalancesOutsideStartingAccountBalanceIntervals <! From
    n -> Invalid n

newtype EntitiesEvent era = CertsEvent (Event (EraRule "CERTS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "CERTS" era)) => Eq (EntitiesEvent era)

instance NFData (Event (EraRule "CERTS" era)) => NFData (EntitiesEvent era)

type instance EraRuleFailure "ENTITIES" DijkstraEra = EntitiesPredFailure DijkstraEra

type instance EraRuleEvent "ENTITIES" DijkstraEra = EntitiesEvent DijkstraEra

instance InjectRuleFailure "ENTITIES" EntitiesPredFailure DijkstraEra

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

instance InjectRuleFailure "ENTITIES" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = shelleyUtxoToDijkstraEntitiesPredFailure

instance
  ( EraTx era
  , DijkstraEraTxBody era
  , DijkstraEraUTxO era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , Embed (EraRule "CERTS" era) (ENTITIES era)
  , State (EraRule "CERTS" era) ~ CertState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "CERTS" era) ~ Conway.CertsEnv era
  , EraRule "ENTITIES" era ~ ENTITIES era
  , InjectRuleFailure "ENTITIES" EntitiesPredFailure era
  , InjectRuleFailure "ENTITIES" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "ENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  STS (ENTITIES era)
  where
  type State (ENTITIES era) = CertState era
  type Signal (ENTITIES era) = StAnnTx TopTx era
  type Environment (ENTITIES era) = EntitiesEnv era
  type BaseM (ENTITIES era) = ShelleyBase
  type PredicateFailure (ENTITIES era) = EntitiesPredFailure era
  type Event (ENTITIES era) = EntitiesEvent era

  initialRules = []
  transitionRules = [dijkstraEntitiesTransition @era]

dijkstraEntitiesTransition ::
  forall era.
  ( DijkstraEraTxBody era
  , DijkstraEraUTxO era
  , ConwayEraCertState era
  , Embed (EraRule "CERTS" era) (ENTITIES era)
  , State (EraRule "CERTS" era) ~ CertState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Environment (EraRule "CERTS" era) ~ Conway.CertsEnv era
  , EraRule "ENTITIES" era ~ ENTITIES era
  , InjectRuleFailure "ENTITIES" EntitiesPredFailure era
  , InjectRuleFailure "ENTITIES" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "ENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  TransitionRule (ENTITIES era)
dijkstraEntitiesTransition = do
  TRC (EntitiesEnv curEpoch pp committee committeeProposals originalAccounts, certState, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
      legacyMode = stAnnTx ^. plutusLegacyModeStAnnTxG
      withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      accounts = certState ^. certDStateL . accountsL
      certsEnv = Conway.CertsEnv pp curEpoch committee committeeProposals

  network <- liftSTS $ asks networkId

  runTest $ Shelley.validateWrongNetworkWithdrawal network (tx ^. bodyTxL)
  runTest $ validateWrongNetworkInDirectDeposit network (tx ^. bodyTxL)
  runTest $ validateAccountBalanceIntervals network accounts (tx ^. bodyTxL)
  runTest $ validateStartingAccountBalanceIntervals network originalAccounts (tx ^. bodyTxL)
  validateWithdrawals legacyMode network withdrawals accounts

  let certStateBeforeCerts =
        certState
          & Conway.updateDormantDRepExpiries tx curEpoch
          & Conway.updateVotingDRepExpiries tx curEpoch (pp ^. ppDRepActivityL)
          & certDStateL . accountsL %~ applyWithdrawals withdrawals
  certStateAfterCerts <-
    trans @(EraRule "CERTS" era) $
      TRC (certsEnv, certStateBeforeCerts, StrictSeq.fromStrict $ tx ^. bodyTxL . certsTxBodyL)

  let directDeposits = tx ^. bodyTxL . directDepositsTxBodyL
      accountsAfterCerts = certStateAfterCerts ^. certDStateL . accountsL
  runTest $ validateMissingAccountsInDirectDeposits directDeposits accountsAfterCerts

  pure $ certStateAfterCerts & certDStateL . accountsL %~ applyDirectDeposits directDeposits

validateWrongNetworkInDirectDeposit ::
  DijkstraEraTxBody era =>
  Network ->
  TxBody t era ->
  Test (EntitiesPredFailure era)
validateWrongNetworkInDirectDeposit netId txb =
  failureOnNonEmptySet depositsWrongNetwork (DirectDepositAddressesWithWrongNetwork netId)
  where
    depositsWrongNetwork =
      Map.keysSet $
        Map.filterWithKey
          (\a _ -> aaNetworkId a /= netId)
          (unDirectDeposits $ txb ^. directDepositsTxBodyL)

validateMissingAccountsInDirectDeposits ::
  EraAccounts era =>
  DirectDeposits ->
  Accounts era ->
  Test (EntitiesPredFailure era)
validateMissingAccountsInDirectDeposits dds accounts =
  failureOnJust
    (directDepositsMissingAccounts dds accounts)
    DirectDepositAccountsMissing

validateWithdrawals ::
  EraAccounts era =>
  Bool ->
  Network ->
  Withdrawals ->
  Accounts era ->
  Rule (ENTITIES era) ctx ()
validateWithdrawals legacyMode network withdrawals accounts = do
  missingWithdrawals <-
    if legacyMode
      then do
        let (missingWithdrawals, incompleteWithdrawals) =
              case withdrawalsThatDoNotDrainAccounts withdrawals network accounts of
                Nothing -> (Map.empty, Map.empty)
                Just (missing, incomplete) -> (unWithdrawals missing, incomplete)
        failOnNonEmptyMap incompleteWithdrawals WithdrawalAmountsInexactInLegacyMode
        pure missingWithdrawals
      else do
        let (missingWithdrawals, exceededWithdrawals) =
              case withdrawalsThatExceedAccountBalance withdrawals network accounts of
                Nothing -> (Map.empty, Map.empty)
                Just (missing, exceeded) -> (unWithdrawals missing, exceeded)
        failOnNonEmptyMap exceededWithdrawals WithdrawalAmountsExceedingOriginalBalance
        pure missingWithdrawals
  failOnNonEmptyMap missingWithdrawals $
    WithdrawalAccountsMissing . Withdrawals . NEM.toMap

conwayToDijkstraEntitiesPredFailure ::
  forall era. Conway.ConwayLedgerPredFailure era -> EntitiesPredFailure era
conwayToDijkstraEntitiesPredFailure = \case
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
    impossible name = error $ "Impossible: `" <> name <> "` for ENTITIES"

shelleyUtxoToDijkstraEntitiesPredFailure ::
  Shelley.ShelleyUtxoPredFailure era -> EntitiesPredFailure era
shelleyUtxoToDijkstraEntitiesPredFailure = \case
  Shelley.WrongNetworkWithdrawal net addrs -> WithdrawalAddressesWithWrongNetwork net addrs
  Shelley.BadInputsUTxO _ -> impossible "BadInputsUTxO"
  Shelley.ExpiredUTxO _ -> impossible "ExpiredUTxO"
  Shelley.MaxTxSizeUTxO _ -> impossible "MaxTxSizeUTxO"
  Shelley.InputSetEmptyUTxO -> impossible "InputSetEmptyUTxO"
  Shelley.FeeTooSmallUTxO _ -> impossible "FeeTooSmallUTxO"
  Shelley.ValueNotConservedUTxO _ -> impossible "ValueNotConservedUTxO"
  Shelley.WrongNetwork _ _ -> impossible "WrongNetwork"
  Shelley.OutputTooSmallUTxO _ -> impossible "OutputTooSmallUTxO"
  Shelley.UpdateFailure _ -> impossible "UpdateFailure"
  Shelley.OutputBootAddrAttrsTooBig _ -> impossible "OutputBootAddrAttrsTooBig"
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

accountBalanceIntervalContains :: Coin -> AccountBalanceInterval era -> Bool
accountBalanceIntervalContains bal = \case
  AccountBalanceLowerBound (Inclusive lo) -> lo <= bal
  AccountBalanceUpperBound (Exclusive hi) -> bal < hi
  AccountBalanceBothBounds (Inclusive lo) (Exclusive hi) -> lo <= bal && bal < hi
  AccountBalanceExact n -> bal == n

categorizeAccountBalanceIntervals ::
  EraAccounts era =>
  Network ->
  Accounts era ->
  AccountBalanceIntervals era ->
  ( Set AccountAddress
  , Map.Map AccountAddress (AccountBalanceInterval era)
  , Map.Map AccountAddress (Coin, AccountBalanceInterval era)
  )
categorizeAccountBalanceIntervals network accounts (AccountBalanceIntervals intervals) =
  Map.foldlWithKey' categorize (Set.empty, Map.empty, Map.empty) intervals
  where
    categorize (!wrongNetwork, !missing, !outside) acct interval
      | aaNetworkId acct /= network = (Set.insert acct wrongNetwork, missing, outside)
      | otherwise =
          case lookupAccountState (acct ^. accountAddressCredentialL) accounts of
            Nothing -> (wrongNetwork, Map.insert acct interval missing, outside)
            Just accountState ->
              let balance = fromCompact (accountState ^. balanceAccountStateL)
               in if accountBalanceIntervalContains balance interval
                    then (wrongNetwork, missing, outside)
                    else (wrongNetwork, missing, Map.insert acct (balance, interval) outside)

validateAccountBalanceIntervals ::
  (EraAccounts era, DijkstraEraTxBody era) =>
  Network ->
  Accounts era ->
  TxBody l era ->
  Test (EntitiesPredFailure era)
validateAccountBalanceIntervals network accounts txBody =
  sequenceA_
    [ failureOnNonEmptySet wrongNetwork (WrongNetworkInAccountBalanceIntervals network)
    , failureOnNonEmptyMap missing MissingAccountsInAccountBalanceIntervals
    , failureOnNonEmptyMap outside BalancesOutsideAccountBalanceIntervals
    ]
  where
    (wrongNetwork, missing, outside) =
      categorizeAccountBalanceIntervals network accounts $
        txBody ^. accountBalanceIntervalsTxBodyL

validateStartingAccountBalanceIntervals ::
  (EraAccounts era, DijkstraEraTxBody era) =>
  Network ->
  Accounts era ->
  TxBody TopTx era ->
  Test (EntitiesPredFailure era)
validateStartingAccountBalanceIntervals network accounts txBody =
  sequenceA_
    [ failureOnNonEmptySet wrongNetwork (WrongNetworkInStartingAccountBalanceIntervals network)
    , failureOnNonEmptyMap missing MissingAccountsInStartingAccountBalanceIntervals
    , failureOnNonEmptyMap outside BalancesOutsideStartingAccountBalanceIntervals
    ]
  where
    (wrongNetwork, missing, outside) =
      categorizeAccountBalanceIntervals network accounts $
        txBody ^. startingAccountBalanceIntervalsTxBodyL
