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

module Cardano.Ledger.Dijkstra.Rules.Entities (
  EntitiesEnv (..),
  EntitiesPredFailure (..),
  EntitiesEvent (..),
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, ENTITIES)
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody,
  directDepositsTxBodyL,
  subTransactionsTxBodyL,
 )
import Cardano.Ledger.Rules.ValidationMode (runTest)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Lens.Micro

data EntitiesEnv era = EntitiesEnv
  { eePlutusLegacyMode :: Bool
  , eeCertsEnv :: Conway.CertsEnv era
  , eeOriginalAccounts :: Accounts era
  }
  deriving (Generic)

deriving instance (EraPParams era, Eq (Tx TopTx era), Eq (Accounts era)) => Eq (EntitiesEnv era)

deriving instance
  (EraPParams era, Show (Tx TopTx era), Show (Accounts era)) => Show (EntitiesEnv era)

instance (EraPParams era, NFData (Tx TopTx era), NFData (Accounts era)) => NFData (EntitiesEnv era)

instance (EraTx era, EncCBOR (Accounts era)) => EncCBOR (EntitiesEnv era) where
  encCBOR x@(EntitiesEnv _ _ _) =
    let EntitiesEnv {..} = x
     in encode $
          Rec EntitiesEnv
            !> To eePlutusLegacyMode
            !> To eeCertsEnv
            !> To eeOriginalAccounts

data EntitiesPredFailure era
  = CertsFailure (PredicateFailure (EraRule "CERTS" era))
  | WdrlNotDelegatedToDRep (NonEmpty (KeyHash Staking))
  | WithdrawalsMissingAccounts Withdrawals
  | IncompleteWithdrawals (NonEmptyMap AccountAddress (Mismatch RelEQ Coin))
  | WithdrawalAmountsExceedAccountBalances (NonEmptyMap AccountAddress (Mismatch RelLTEQ Coin))
  | DirectDepositsToMissingAccounts DirectDeposits
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "CERTS" era)) => Eq (EntitiesPredFailure era)

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
      WdrlNotDelegatedToDRep x -> Sum (WdrlNotDelegatedToDRep @era) 1 !> To x
      WithdrawalsMissingAccounts x -> Sum (WithdrawalsMissingAccounts @era) 2 !> To x
      IncompleteWithdrawals x -> Sum (IncompleteWithdrawals @era) 3 !> To x
      WithdrawalAmountsExceedAccountBalances x -> Sum (WithdrawalAmountsExceedAccountBalances @era) 4 !> To x
      DirectDepositsToMissingAccounts x -> Sum (DirectDepositsToMissingAccounts @era) 5 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  ) =>
  DecCBOR (EntitiesPredFailure era)
  where
  decCBOR = decode . Summands "EntitiesPredFailure" $ \case
    0 -> SumD CertsFailure <! From
    1 -> SumD WdrlNotDelegatedToDRep <! From
    2 -> SumD WithdrawalsMissingAccounts <! From
    3 -> SumD IncompleteWithdrawals <! From
    4 -> SumD WithdrawalAmountsExceedAccountBalances <! From
    5 -> SumD DirectDepositsToMissingAccounts <! From
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
  , InjectRuleFailure "ENTITIES" Conway.ConwayLedgerPredFailure era
  ) =>
  TransitionRule (ENTITIES era)
dijkstraEntitiesTransition = do
  TRC (EntitiesEnv legacyMode certsEnv originalAccounts, certState, certificates) <- judgmentContext
  let Conway.CertsEnv tx pp curEpoch _committee _committeeProposals = certsEnv
      withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      accounts = certState ^. certDStateL . accountsL

  {- Aggregated withdrawals across the batch must not exceed each account's
     pre-batch balance, and every withdrawn account must exist pre-batch.
     In legacy mode, top-tx withdrawals are excluded (CIP-118 escape hatch). -}
  validateBatchWithdrawals legacyMode originalAccounts tx

  runTest $ Conway.validateWithdrawalsDelegated accounts tx

  network <- liftSTS $ asks networkId

  when legacyMode $ validateLegacyDrain network withdrawals accounts

  let certStateBeforeCerts =
        certState
          & Conway.updateDormantDRepExpiries tx curEpoch
          & Conway.updateVotingDRepExpiries tx curEpoch (pp ^. ppDRepActivityL)
          & certDStateL . accountsL %~ applyWithdrawals withdrawals
  certStateAfterCerts <-
    trans @(EraRule "CERTS" era) $ TRC (certsEnv, certStateBeforeCerts, certificates)

  let directDeposits = tx ^. bodyTxL . directDepositsTxBodyL
      accountsAfterCerts = certStateAfterCerts ^. certDStateL . accountsL
  failOnJust (directDepositsMissingAccounts directDeposits accountsAfterCerts) $
    injectFailure . DirectDepositsToMissingAccounts

  pure $ certStateAfterCerts & certDStateL . accountsL %~ applyDirectDeposits directDeposits

-- | Aggregate withdrawals across the top tx and all its subtransactions. For each
-- account, the total withdrawn must not exceed its pre-batch balance, and every
-- account referenced by a withdrawal must exist pre-batch.
--
-- In legacy mode, top-tx's own withdrawals are excluded from the batch sum
-- (CIP-118 escape hatch): top's withdrawal is validated separately by
-- 'validateLegacyDrain' against post-sub-ledger accounts, allowing a top-tx
-- to withdraw from an account funded mid-batch by a sub-tx's direct deposit.
validateBatchWithdrawals ::
  ( EraTx era
  , EraAccounts era
  , DijkstraEraTxBody era
  ) =>
  Bool ->
  Accounts era ->
  Tx TopTx era ->
  Rule (ENTITIES era) ctx ()
validateBatchWithdrawals legacyMode originalAccounts tx = do
  let topWithdrawals
        | legacyMode = Map.empty
        | otherwise = unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)
      allWithdrawals =
        Map.unionsWith (<>) $
          topWithdrawals
            : [ unWithdrawals $ subTx ^. bodyTxL . withdrawalsTxBodyL
              | subTx <- OMap.elems $ tx ^. bodyTxL . subTransactionsTxBodyL
              ]
      categorize acctAddr@(AccountAddress _ (AccountId cred)) withdrawn (missing, exceeded) =
        case lookupAccountState cred originalAccounts of
          Nothing -> (Map.insert acctAddr withdrawn missing, exceeded)
          Just accountState ->
            let balance = fromCompact (accountState ^. balanceAccountStateL)
             in if withdrawn > balance
                  then
                    ( missing
                    , Map.insert
                        acctAddr
                        Mismatch {mismatchSupplied = withdrawn, mismatchExpected = balance}
                        exceeded
                    )
                  else (missing, exceeded)
      (missingWithdrawals, exceededWithdrawals) =
        Map.foldrWithKey categorize (Map.empty, Map.empty) allWithdrawals
  failOnNonEmptyMap missingWithdrawals $
    WithdrawalsMissingAccounts . Withdrawals . NEM.toMap
  failOnNonEmptyMap exceededWithdrawals WithdrawalAmountsExceedAccountBalances

validateLegacyDrain ::
  EraAccounts era =>
  Network ->
  Withdrawals ->
  Accounts era ->
  Rule (ENTITIES era) ctx ()
validateLegacyDrain network withdrawals accounts = do
  let incompleteWithdrawals =
        case withdrawalsThatDoNotDrainAccounts withdrawals network accounts of
          Nothing -> Map.empty
          Just (_missing, incomplete) -> incomplete
  failOnNonEmptyMap incompleteWithdrawals IncompleteWithdrawals

conwayToDijkstraEntitiesPredFailure ::
  forall era. Conway.ConwayLedgerPredFailure era -> EntitiesPredFailure era
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
