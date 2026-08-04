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
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, ENTITIES)
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody, directDepositsTxBodyL)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)
import Lens.Micro

data EntitiesEnv era = EntitiesEnv
  { eePlutusLegacyMode :: Bool
  , eeCertsEnv :: Conway.CertsEnv era
  }
  deriving (Generic)

deriving instance (EraPParams era, Eq (Tx TopTx era)) => Eq (EntitiesEnv era)

deriving instance (EraPParams era, Show (Tx TopTx era)) => Show (EntitiesEnv era)

instance (EraPParams era, NFData (Tx TopTx era)) => NFData (EntitiesEnv era)

instance EraTx era => EncCBOR (EntitiesEnv era) where
  encCBOR x@(EntitiesEnv _ _) =
    let EntitiesEnv {..} = x
     in encode $
          Rec EntitiesEnv
            !> To eePlutusLegacyMode
            !> To eeCertsEnv

data EntitiesPredFailure era
  = CertsFailure (PredicateFailure (EraRule "CERTS" era))
  | MissingAccountsInWithdrawals Withdrawals
  | IncompleteWithdrawals (NonEmptyMap AccountAddress (Mismatch RelEQ Coin))
  | ExceededBalancesInWithdrawals (NonEmptyMap AccountAddress (Mismatch RelLTEQ Coin))
  | MissingAccountsInDirectDeposits DirectDeposits
  | WrongNetworkInWithdrawals
      -- | Expected network id
      Network
      -- | Withdrawal accounts with wrong network id
      (NonEmptySet AccountAddress)
  | WrongNetworkInDirectDeposits
      -- | Expected network id
      Network
      -- | Direct-deposit accounts with wrong network id
      (NonEmptySet AccountAddress)
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
      MissingAccountsInWithdrawals x -> Sum (MissingAccountsInWithdrawals @era) 1 !> To x
      IncompleteWithdrawals x -> Sum (IncompleteWithdrawals @era) 2 !> To x
      ExceededBalancesInWithdrawals x -> Sum (ExceededBalancesInWithdrawals @era) 3 !> To x
      MissingAccountsInDirectDeposits x -> Sum (MissingAccountsInDirectDeposits @era) 4 !> To x
      WrongNetworkInWithdrawals x y -> Sum (WrongNetworkInWithdrawals @era) 5 !> To x !> To y
      WrongNetworkInDirectDeposits x y -> Sum (WrongNetworkInDirectDeposits @era) 6 !> To x !> To y

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  ) =>
  DecCBOR (EntitiesPredFailure era)
  where
  decCBOR = decode . Summands "EntitiesPredFailure" $ \case
    0 -> SumD CertsFailure <! From
    1 -> SumD MissingAccountsInWithdrawals <! From
    2 -> SumD IncompleteWithdrawals <! From
    3 -> SumD ExceededBalancesInWithdrawals <! From
    4 -> SumD MissingAccountsInDirectDeposits <! From
    5 -> SumD WrongNetworkInWithdrawals <! From <! From
    6 -> SumD WrongNetworkInDirectDeposits <! From <! From
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
  type Signal (ENTITIES era) = Seq (TxCert era)
  type Environment (ENTITIES era) = EntitiesEnv era
  type BaseM (ENTITIES era) = ShelleyBase
  type PredicateFailure (ENTITIES era) = EntitiesPredFailure era
  type Event (ENTITIES era) = EntitiesEvent era

  initialRules = []
  transitionRules = [dijkstraEntitiesTransition @era]

dijkstraEntitiesTransition ::
  forall era.
  ( EraTx era
  , DijkstraEraTxBody era
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
  TRC (EntitiesEnv legacyMode certsEnv, certState, certificates) <- judgmentContext
  let Conway.CertsEnv tx pp curEpoch _committee _committeeProposals = certsEnv
      withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      accounts = certState ^. certDStateL . accountsL

  network <- liftSTS $ asks networkId

  runTest $ Shelley.validateWrongNetworkWithdrawal network (tx ^. bodyTxL)
  runTest $ validateWrongNetworkInDirectDeposit network (tx ^. bodyTxL)

  validateWithdrawals legacyMode network withdrawals accounts

  let certStateBeforeCerts =
        certState
          & Conway.updateDormantDRepExpiries tx curEpoch
          & Conway.updateVotingDRepExpiries tx curEpoch (pp ^. ppDRepActivityL)
          & certDStateL . accountsL %~ applyWithdrawals withdrawals
  certStateAfterCerts <-
    trans @(EraRule "CERTS" era) $ TRC (certsEnv, certStateBeforeCerts, certificates)

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
  failureOnNonEmptySet depositsWrongNetwork (WrongNetworkInDirectDeposits netId)
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
    MissingAccountsInDirectDeposits

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
        failOnNonEmptyMap incompleteWithdrawals IncompleteWithdrawals
        pure missingWithdrawals
      else do
        let (missingWithdrawals, exceededWithdrawals) =
              case withdrawalsThatExceedAccountBalance withdrawals network accounts of
                Nothing -> (Map.empty, Map.empty)
                Just (missing, exceeded) -> (unWithdrawals missing, exceeded)
        failOnNonEmptyMap exceededWithdrawals ExceededBalancesInWithdrawals
        pure missingWithdrawals
  failOnNonEmptyMap missingWithdrawals $
    MissingAccountsInWithdrawals . Withdrawals . NEM.toMap

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
  Shelley.WrongNetworkWithdrawal net addrs -> WrongNetworkInWithdrawals net addrs
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
