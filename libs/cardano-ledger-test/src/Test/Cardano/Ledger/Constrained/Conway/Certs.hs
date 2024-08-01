{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERTS rule
module Test.Cardano.Ledger.Constrained.Conway.Certs where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (CertState (..), DState (..), PState (..), VState (..), drepDeposit)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.UMap as UMap
import Constrained
import Control.DeepSeq (NFData)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict as OMap
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (DepositPurpose (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances (IsConwayUniv)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

-- ==============================================================

data CertsExecEnv era = CertsExecEnv
  { ceeCertEnv :: !(CertsEnv era)
  , ceeDeposits :: !(Map (DepositPurpose (EraCrypto era)) Coin)
  , ceeWithdrawals :: !(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  , ceeVotes :: !(VotingProcedures era)
  }
  deriving (Generic)

deriving instance (EraPParams era, Eq (Tx era)) => Eq (CertsExecEnv era)
deriving instance (EraPParams era, Show (Tx era)) => Show (CertsExecEnv era)

instance
  ( Era era
  , ToExpr (Tx era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (PParamsHKD Identity era)
  ) =>
  ToExpr (CertsExecEnv era)
instance (EraPParams era, NFData (Tx era)) => NFData (CertsExecEnv era)
instance HasSimpleRep (CertsExecEnv era)
instance
  ( Eq (Tx era)
  , Show (Tx era)
  , IsConwayUniv fn
  , EraPParams era
  , HasSpec fn (CertsEnv era)
  , HasSpec fn (CertEnv era)
  ) =>
  HasSpec fn (CertsExecEnv era)

instance Inject (CertsExecEnv era) (CertsEnv era) where
  inject = ceeCertEnv

instance Inject (CertsExecEnv era) (CertEnv era) where
  inject CertsExecEnv {..} =
    CertEnv
      (certsSlotNo ceeCertEnv)
      (certsPParams ceeCertEnv)
      (certsCurrentEpoch ceeCertEnv)
      (certsCurrentCommittee ceeCertEnv)
      (certsCommitteeProposals ceeCertEnv)

instance Inject (CertsExecEnv era) (ConwayGovCertEnv era) where
  inject CertsExecEnv {..} =
    ConwayGovCertEnv
      (certsPParams ceeCertEnv)
      (certsCurrentEpoch ceeCertEnv)
      (certsCurrentCommittee ceeCertEnv)
      (certsCommitteeProposals ceeCertEnv)

toDeposits ::
  ConwayEraGov era =>
  CertState era ->
  GovState era ->
  Map (DepositPurpose (EraCrypto era)) Coin
toDeposits CertState {..} govState =
  Map.unions
    [ Map.mapKeys CredentialDeposit credDeposits
    , Map.mapKeys PoolDeposit poolDeposits
    , Map.mapKeys DRepDeposit drepDeposits
    , Map.mapKeys GovActionDeposit proposalDeposits
    ]
  where
    credDeposits = depositMap (dsUnified certDState)
    poolDeposits = psDeposits certPState
    drepDeposits = drepDeposit <$> vsDReps certVState
    proposalDeposits =
      Map.map
        (^. gasProposalProcedureL . pProcDepositL)
        (OMap.toMap (govState ^. proposalsGovStateL . pPropsL))

-- ===================================================

certsExecEnvSpec ::
  Specification fn (CertsExecEnv (ConwayEra StandardCrypto))
certsExecEnvSpec = TrueSpec

txCertsSpec ::
  Specification fn (Seq (ConwayTxCert (ConwayEra StandardCrypto)))
txCertsSpec = TrueSpec
