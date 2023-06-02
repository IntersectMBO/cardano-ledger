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
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Language (Language (..))
import Control.State.Transition.Extended (STS (Event))
import Test.Cardano.Ledger.Alonzo.Arbitrary (genAlonzoScript)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common

instance Crypto c => Arbitrary (ConwayGenesis c) where
  arbitrary = ConwayGenesis <$> arbitrary

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
      , ConwayTxCertCommittee <$> arbitrary
      ]

instance Crypto c => Arbitrary (ConwayCommitteeCert c) where
  arbitrary =
    oneof
      [ ConwayDRepReg <$> arbitrary
      , ConwayDRepUnReg <$> arbitrary
      , ConwayAuthCommitteeHotKey <$> arbitrary <*> arbitrary
      , ConwayResignCommitteeColdKey <$> arbitrary
      ]

instance Crypto c => Arbitrary (AlonzoScript (ConwayEra c)) where
  arbitrary = genAlonzoScript [PlutusV1, PlutusV2, PlutusV3]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Conway.Governance ------------------------------------------------------
------------------------------------------------------------------------------------------

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (ConwayGovernance era)
  where
  arbitrary =
    ConwayGovernance
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

deriving instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (ConwayTallyState era)

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovernanceActionState era) where
  arbitrary =
    GovernanceActionState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovernanceAction era) where
  arbitrary =
    oneof
      [ ParameterChange <$> arbitrary
      , HardForkInitiation <$> arbitrary
      , TreasuryWithdrawals <$> arbitrary
      ]

instance Crypto c => Arbitrary (GovernanceActionId c) where
  arbitrary =
    GovernanceActionId
      <$> arbitrary
      <*> arbitrary

deriving instance Arbitrary GovernanceActionIx

instance Arbitrary VoterRole where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Vote where
  arbitrary = arbitraryBoundedEnum

instance
  ( Era era
  , ConwayEraTxBody era
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

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Conway.Rules -----------------------------------------------------------
------------------------------------------------------------------------------------------

-- TALLY

instance Era era => Arbitrary (TallyEnv era) where
  arbitrary =
    TallyEnv
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (Anchor c) where
  arbitrary =
    Anchor
      <$> arbitrary
      <*> arbitrary

instance Era era => Arbitrary (VotingProcedure era) where
  arbitrary = VotingProcedure <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (ProposalProcedure era) where
  arbitrary = ProposalProcedure <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovernanceProcedure era) where
  arbitrary =
    oneof
      [ GovernanceVotingProcedure <$> arbitrary
      , GovernanceProposalProcedure <$> arbitrary
      ]

instance Era era => Arbitrary (ConwayTallyPredFailure era) where
  arbitrary =
    oneof
      [ VoterDoesNotHaveRole <$> arbitrary <*> arbitrary
      , GovernanceActionDoesNotExist <$> arbitrary
      ]

instance
  ( Arbitrary (PredicateFailure (EraRule "UTXOW" era))
  , Arbitrary (PredicateFailure (EraRule "CERTS" era))
  , Arbitrary (PredicateFailure (EraRule "TALLY" era))
  ) =>
  Arbitrary (ConwayLedgerPredFailure era)
  where
  arbitrary =
    oneof
      [ ConwayUtxowFailure <$> arbitrary
      , ConwayCertsFailure <$> arbitrary
      , ConwayTallyFailure <$> arbitrary
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

-- TICKF

instance
  ( Era era
  ) =>
  Arbitrary (ConwayTickfPredFailure era)
  where
  arbitrary = undefined

instance
  ( Era era
  ) =>
  Arbitrary (ConwayTickfEvent era)
  where
  arbitrary = undefined

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
