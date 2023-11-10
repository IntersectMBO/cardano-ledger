{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Arbitrary (
  genUpdateCommittee,
  genNoConfidence,
  genTreasuryWithdrawals,
  genHardForkInitiation,
  genParameterChange,
  genNewConstitution,
  genGovActionStateFromAction,
  govActionGenerators,
  uniqueIdGovActions,
) where

import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayGovState (..),
  DRepPulsingState (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovProcedures (..),
  PrevGovActionId (..),
  PrevGovActionIds (..),
  ProposalProcedure (..),
  Proposals,
  PulsingSnapshot (..),
  RatifyEnv (..),
  Vote,
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  fromGovActionStateSeq,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
  UpgradeConwayPParams (..),
 )
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.HKD (HKD, NoUpdate (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Control.State.Transition.Extended (STS (Event))
import Data.Functor.Identity (Identity)
import Data.List (nubBy)
import qualified Data.Sequence.Strict as Seq
import Test.Cardano.Data (genNonEmptyMap)
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary (genAlonzoScript, unFlexibleCostModels)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (PulsingSnapshot era) where
  arbitrary = PulsingSnapshot <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary (PParams era), Arbitrary (PParamsUpdate era), Era era) => Arbitrary (DRepPulsingState era) where
  arbitrary = DRComplete <$> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (ConwayGenesis c) where
  arbitrary = ConwayGenesis <$> arbitrary <*> arbitrary <*> arbitrary

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
      , ConwayResignCommitteeColdKey <$> arbitrary <*> arbitrary
      ]

instance Crypto c => Arbitrary (AlonzoScript (ConwayEra c)) where
  arbitrary = genAlonzoScript [PlutusV1, PlutusV2, PlutusV3]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Conway.Goverance  ------------------------------------------------------
------------------------------------------------------------------------------------------

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (ConwayGovState era)
  where
  arbitrary =
    ConwayGovState
      <$> arbitrary
      <*> arbitrary
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
      <*> arbitrary

instance Era era => Arbitrary (PrevGovActionIds era) where
  arbitrary =
    PrevGovActionIds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

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

uniqueIdGovActions ::
  ( Era era
  , Arbitrary (PParamsUpdate era)
  ) =>
  Gen (Seq.StrictSeq (GovActionState era))
uniqueIdGovActions = Seq.fromList . nubBy (\x y -> gasId x == gasId y) <$> arbitrary

instance
  ( Era era
  , Arbitrary (PParamsUpdate era)
  ) =>
  Arbitrary (Proposals era)
  where
  arbitrary = fromGovActionStateSeq <$> uniqueIdGovActions

genGovActionStateFromAction :: Era era => GovAction era -> Gen (GovActionState era)
genGovActionStateFromAction act =
  GovActionState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure act
    <*> arbitrary
    <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovActionState era) where
  arbitrary = genGovActionStateFromAction =<< arbitrary

genParameterChange :: (Era era, Arbitrary (PParamsUpdate era)) => Gen (GovAction era)
genParameterChange = ParameterChange <$> arbitrary <*> arbitrary

genHardForkInitiation :: Era era => Gen (GovAction era)
genHardForkInitiation = HardForkInitiation <$> arbitrary <*> arbitrary

genTreasuryWithdrawals :: Era era => Gen (GovAction era)
genTreasuryWithdrawals = TreasuryWithdrawals <$> arbitrary

genNoConfidence :: Era era => Gen (GovAction era)
genNoConfidence = NoConfidence <$> arbitrary

genUpdateCommittee :: Era era => Gen (GovAction era)
genUpdateCommittee =
  UpdateCommittee
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genNewConstitution :: Era era => Gen (GovAction era)
genNewConstitution = NewConstitution <$> arbitrary <*> arbitrary

govActionGenerators ::
  ( Era era
  , Arbitrary (PParamsUpdate era)
  ) =>
  [Gen (GovAction era)]
govActionGenerators =
  [ genParameterChange
  , genHardForkInitiation
  , genTreasuryWithdrawals
  , genNoConfidence
  , genUpdateCommittee
  , genNewConstitution
  , pure InfoAction
  ]

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovAction era) where
  arbitrary = oneof govActionGenerators

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

instance (EraPParams era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovProcedures era) where
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

instance Arbitrary (HKD f a) => Arbitrary (THKD t f a) where
  arbitrary = THKD <$> arbitrary

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
      <*> (THKD . unFlexibleCostModels <$> arbitrary)
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
      <*> pure NoUpdate
      <*> arbitrary
      <*> arbitrary
      <*> (THKD . fmap unFlexibleCostModels <$> arbitrary)
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
