{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Arbitrary () where

import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  UpgradeConwayPParams (..),
 )
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Language (Language (..))
import Control.State.Transition.Extended (STS (Event))
import Data.Functor.Identity (Identity)
import Test.Cardano.Data (genNonEmptyMap)
import Test.Cardano.Ledger.Alonzo.Arbitrary (genAlonzoScript)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common

instance Crypto c => Arbitrary (ConwayGenesis c) where
  arbitrary = ConwayGenesis <$> arbitrary

instance Arbitrary (UpgradeConwayPParams Identity) where
  arbitrary =
    UpgradeConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (Delegatee c) where
  arbitrary =
    oneof
      [ DelegStake <$> arbitrary
      , DelegVote <$> arbitrary
      , DelegStakeVote <$> arbitrary <*> arbitrary
      ]

instance Crypto c => Arbitrary (ConwayDelegCert c) where
  arbitrary =
    oneof
      [ ConwayRegCert <$> arbitrary <*> arbitrary
      , ConwayUnRegCert <$> arbitrary <*> arbitrary
      , ConwayDelegCert <$> arbitrary <*> arbitrary
      , ConwayRegDelegCert <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Era era => Arbitrary (ConwayTxCert era) where
  arbitrary =
    oneof
      [ ConwayTxCertDeleg <$> arbitrary
      , ConwayTxCertPool <$> arbitrary
      , ConwayTxCertGov <$> arbitrary
      ]

instance Crypto c => Arbitrary (ConwayGovCert c) where
  arbitrary =
    oneof
      [ ConwayRegDRep <$> arbitrary <*> arbitrary <*> arbitrary
      , ConwayUnRegDRep <$> arbitrary <*> arbitrary
      , ConwayAuthCommitteeHotKey <$> arbitrary <*> arbitrary
      , ConwayResignCommitteeColdKey <$> arbitrary
      ]

instance Crypto c => Arbitrary (AlonzoScript (ConwayEra c)) where
  arbitrary = genAlonzoScript [PlutusV1, PlutusV2, PlutusV3]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Conway.Goverance  ------------------------------------------------------
------------------------------------------------------------------------------------------

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (ConwayGovState era)
  where
  arbitrary =
    ConwayGovState
      <$> arbitrary
      <*> arbitrary

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (RatifyState era)
  where
  arbitrary =
    RatifyState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (RatifyEnv era)
  where
  arbitrary =
    RatifyEnv
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto (EraCrypto era) => Arbitrary (Constitution era) where
  arbitrary = Constitution <$> arbitrary <*> arbitrary

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (EnactState era)
  where
  arbitrary =
    EnactState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovActionsState era)

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovActionState era) where
  arbitrary =
    GovActionState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovAction era) where
  arbitrary =
    oneof
      [ ParameterChange <$> arbitrary <*> arbitrary
      , HardForkInitiation <$> arbitrary <*> arbitrary
      , TreasuryWithdrawals <$> arbitrary
      , NoConfidence <$> arbitrary
      , NewCommittee <$> arbitrary <*> arbitrary <*> arbitrary
      , NewConstitution <$> arbitrary <*> arbitrary
      , pure InfoAction
      ]

instance Era era => Arbitrary (Committee era) where
  arbitrary = Committee <$> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (GovActionId c) where
  arbitrary =
    GovActionId
      <$> arbitrary
      <*> arbitrary

deriving instance Arbitrary GovActionIx

deriving instance Crypto c => Arbitrary (PrevGovActionId r c)

instance Crypto c => Arbitrary (Voter c) where
  arbitrary =
    oneof
      [ CommitteeVoter <$> arbitrary
      , DRepVoter <$> arbitrary
      , StakePoolVoter <$> arbitrary
      ]

instance Arbitrary Vote where
  arbitrary = arbitraryBoundedEnum

instance
  ( ConwayEraTxBody era
  , Arbitrary (Sized (TxOut era))
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , Arbitrary (PParamsUpdate era)
  ) =>
  Arbitrary (ConwayTxBody era)
  where
  arbitrary =
    ConwayTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Conway.Rules -----------------------------------------------------------
------------------------------------------------------------------------------------------

-- GOV

instance (Era era, Arbitrary (PParamsHKD Identity era)) => Arbitrary (GovEnv era) where
  arbitrary =
    GovEnv
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Era era => Arbitrary (VotingProcedure era) where
  arbitrary = VotingProcedure <$> arbitrary <*> arbitrary

instance Era era => Arbitrary (VotingProcedures era) where
  arbitrary = VotingProcedures <$> liftArbitrary (genNonEmptyMap arbitrary arbitrary)

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (ProposalProcedure era) where
  arbitrary =
    ProposalProcedure
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovProcedures era) where
  arbitrary =
    GovProcedures <$> arbitrary <*> arbitrary

instance Era era => Arbitrary (ConwayGovPredFailure era) where
  arbitrary = GovActionsDoNotExist <$> arbitrary

instance
  ( Arbitrary (PredicateFailure (EraRule "UTXOW" era))
  , Arbitrary (PredicateFailure (EraRule "CERTS" era))
  , Arbitrary (PredicateFailure (EraRule "GOV" era))
  ) =>
  Arbitrary (ConwayLedgerPredFailure era)
  where
  arbitrary =
    oneof
      [ ConwayUtxowFailure <$> arbitrary
      , ConwayCertsFailure <$> arbitrary
      , ConwayGovFailure <$> arbitrary
      ]

-- EPOCH

instance
  ( Era era
  , Arbitrary (Event (EraRule "POOLREAP" era))
  , Arbitrary (Event (EraRule "SNAP" era))
  ) =>
  Arbitrary (ConwayEpochEvent era)
  where
  arbitrary =
    oneof
      [ PoolReapEvent <$> arbitrary
      , SnapEvent <$> arbitrary
      ]

-- NEWEPOCH

instance
  ( Era era
  , Arbitrary (Event (EraRule "RUPD" era))
  ) =>
  Arbitrary (ConwayNewEpochEvent era)
  where
  arbitrary =
    oneof
      [ DeltaRewardEvent <$> arbitrary
      , RestrainedRewards <$> arbitrary <*> arbitrary <*> arbitrary
      ]

-- CERTS

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "CERT" era))
  ) =>
  Arbitrary (ConwayCertsPredFailure era)
  where
  arbitrary =
    oneof
      [ DelegateeNotRegisteredDELEG <$> arbitrary
      , WithdrawalsNotInRewardsCERTS <$> arbitrary
      , CertFailure <$> arbitrary
      ]

-- CERT

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "DELEG" era))
  , Arbitrary (PredicateFailure (EraRule "POOL" era))
  , Arbitrary (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  Arbitrary (ConwayCertPredFailure era)
  where
  arbitrary =
    oneof
      [ DelegFailure <$> arbitrary
      , PoolFailure <$> arbitrary
      , GovCertFailure <$> arbitrary
      ]

-- DELEG

instance
  Era era =>
  Arbitrary (ConwayDelegPredFailure era)
  where
  arbitrary =
    oneof
      [ IncorrectDepositDELEG <$> arbitrary
      , StakeKeyRegisteredDELEG <$> arbitrary
      , StakeKeyNotRegisteredDELEG <$> arbitrary
      , StakeKeyHasNonZeroRewardAccountBalanceDELEG <$> arbitrary
      , DRepAlreadyRegisteredForStakeKeyDELEG <$> arbitrary
      , pure WrongCertificateTypeDELEG
      ]

-- GOVCERT

instance Era era => Arbitrary (ConwayGovCertPredFailure era) where
  arbitrary =
    oneof
      [ ConwayDRepAlreadyRegistered <$> arbitrary
      , ConwayDRepNotRegistered <$> arbitrary
      , ConwayDRepIncorrectDeposit <$> arbitrary <*> arbitrary
      , ConwayCommitteeHasPreviouslyResigned <$> arbitrary
      ]

instance Era era => Arbitrary (ConwayPParams Identity era) where
  arbitrary =
    ConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Era era => Arbitrary (ConwayPParams StrictMaybe era) where
  arbitrary =
    ConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure SNothing
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PoolVotingThresholds where
  arbitrary =
    PoolVotingThresholds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary DRepVotingThresholds where
  arbitrary =
    DRepVotingThresholds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
