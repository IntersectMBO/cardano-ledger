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
-- for the GOVCERT rule
module Test.Cardano.Ledger.Constrained.Conway.GovCert where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
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
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.OMap.Strict as OMap
import GHC.Generics (Generic)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit (DepositPurpose (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

-- =========================================================

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

vStateSpec :: Specification fn (VState (ConwayEra StandardCrypto))
vStateSpec = TrueSpec

govCertSpec ::
  IsConwayUniv fn =>
  ConwayGovCertEnv (ConwayEra StandardCrypto) ->
  VState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayGovCert StandardCrypto)
govCertSpec ConwayGovCertEnv {..} vs =
  let reps = lit $ Map.keysSet $ vsDReps vs
      deposits = Map.map drepDeposit (vsDReps vs)
      getNewMembers = \case
        UpdateCommittee _ _ newMembers _ -> Map.keysSet newMembers
        _ -> mempty
      knownColdCreds =
        Map.keysSet (foldMap committeeMembers cgceCurrentCommittee)
          <> foldMap (getNewMembers . pProcGovAction . gasProposalProcedure) cgceCommitteeProposals
      ccCertSpec coldCred =
        assert . member_ coldCred $ lit knownColdCreds
      commiteeStatus = csCommitteeCreds (vsCommitteeState vs)
   in constrained $ \ [var|gc|] ->
        caseOn
          gc
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency
          -- ConwayRegDRep

          ( branchW 1 $ \ [var|keyreg|] [var|coinreg|] _ ->
              [ assert $ not_ $ member_ keyreg (dom_ (lit deposits))
              , assert $ coinreg ==. lit (cgcePParams ^. ppDRepDepositL)
              ]
          )
          -- ConwayUnRegDRep
          ( branchW 3 $ \ [var|credUnreg|] [var|coinUnreg|] ->
              assert $ elem_ (pair_ credUnreg coinUnreg) (lit (Map.toList deposits))
          )
          -- ConwayUpdateDRep
          ( branchW 1 $ \keyupdate _ ->
              member_ keyupdate reps
          )
          -- ConwayAuthCommitteeHotKey
          ( branchW 1 $ \ [var|coldCredAuth|] _ -> [ccCertSpec coldCredAuth, notYetResigned commiteeStatus coldCredAuth]
          )
          -- ConwayResignCommitteeColdKey
          ( branchW 1 $ \ [var|coldCredResign|] _ -> [ccCertSpec coldCredResign, notYetResigned commiteeStatus coldCredResign]
          )

-- | Operations for authenticating a HotKey, or resigning a ColdKey are illegal
--   if that key has already resigned.
notYetResigned ::
  (HasSpec fn x, Ord x, IsConwayUniv fn) =>
  Map.Map x (CommitteeAuthorization StandardCrypto) ->
  Term fn x ->
  Pred fn
notYetResigned committeeStatus coldcred =
  caseOn
    (lookup_ coldcred (lit committeeStatus))
    -- SNothing
    (branch $ \_ -> True)
    -- SJust
    ( branch $ \x ->
        [ caseOn
            x
            --  CommitteeHotCredential
            (branch $ \_ -> True)
            -- CommitteeMemberResigned
            (branch $ \_ -> False)
        ]
    )

govCertEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (ConwayGovCertEnv (ConwayEra StandardCrypto))
govCertEnvSpec =
  constrained $ \gce ->
    match gce $ \pp _ _ _ ->
      satisfies pp pparamsSpec
