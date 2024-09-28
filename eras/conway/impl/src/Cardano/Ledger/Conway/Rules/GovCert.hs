{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.GovCert (
  ConwayGOVCERT,
  ConwayGovCertPredFailure (..),
  ConwayGovCertEnv (..),
  computeDRepExpiry,
)
where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  ShelleyBase,
  StrictMaybe,
  addEpochInterval,
  strictMaybe,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  CommitteeState (..),
  VState (..),
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayGOVCERT)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  GovAction (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovPurposeId,
  ProposalProcedure (..),
 )
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRepState (..), drepAnchorL, drepDepositL, drepExpiryL)
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole, DRepRole))
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Slotting.Slot (EpochInterval, binOpEpochNo)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  failBecause,
  failOnJust,
  judgmentContext,
  transitionRules,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ConwayGovCertEnv era = ConwayGovCertEnv
  { cgcePParams :: !(PParams era)
  , cgceCurrentEpoch :: !EpochNo
  , cgceCurrentCommittee :: StrictMaybe (Committee era)
  , cgceCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose era) (GovActionState era)
  -- ^ All of the `UpdateCommittee` proposals
  }
  deriving (Generic)

instance EraPParams era => EncCBOR (ConwayGovCertEnv era) where
  encCBOR x@(ConwayGovCertEnv _ _ _ _) =
    let ConwayGovCertEnv {..} = x
     in encode $
          Rec ConwayGovCertEnv
            !> To cgcePParams
            !> To cgceCurrentEpoch
            !> To cgceCurrentCommittee
            !> To cgceCommitteeProposals

instance EraPParams era => NFData (ConwayGovCertEnv era)

deriving instance EraPParams era => Show (ConwayGovCertEnv era)

deriving instance EraPParams era => Eq (ConwayGovCertEnv era)

data ConwayGovCertPredFailure era
  = ConwayDRepAlreadyRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepNotRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepIncorrectDeposit !Coin !Coin -- The first is the given and the second is the expected deposit
  | ConwayCommitteeHasPreviouslyResigned !(Credential 'ColdCommitteeRole (EraCrypto era))
  | ConwayDRepIncorrectRefund !Coin !Coin -- The first is the given and the second is the expected refund
  | -- | Predicate failure whenever an update to an unknown committee member is
    -- attempted. Current Constitutional Committee and all available proposals will be
    -- searched before reporting this predicate failure.
    ConwayCommitteeIsUnknown !(Credential 'ColdCommitteeRole (EraCrypto era))
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "GOVCERT" (ConwayEra c) = ConwayGovCertPredFailure (ConwayEra c)

type instance EraRuleEvent "GOVCERT" (ConwayEra c) = VoidEraRule "GOVCERT" (ConwayEra c)

instance InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure (ConwayEra c)

instance NoThunks (ConwayGovCertPredFailure era)

instance NFData (ConwayGovCertPredFailure era)

instance
  (Typeable era, Crypto (EraCrypto era)) =>
  EncCBOR (ConwayGovCertPredFailure era)
  where
  encCBOR = \case
    ConwayDRepAlreadyRegistered cred ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR cred
    ConwayDRepNotRegistered cred ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR cred
    ConwayDRepIncorrectDeposit deposit expectedDeposit ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR deposit
        <> encCBOR expectedDeposit
    ConwayCommitteeHasPreviouslyResigned coldCred ->
      encodeListLen 2
        <> encCBOR (3 :: Word8)
        <> encCBOR coldCred
    ConwayDRepIncorrectRefund refund expectedRefund ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR refund
        <> encCBOR expectedRefund
    ConwayCommitteeIsUnknown coldCred ->
      encodeListLen 2
        <> encCBOR (5 :: Word8)
        <> encCBOR coldCred

instance
  (Typeable era, Crypto (EraCrypto era)) =>
  DecCBOR (ConwayGovCertPredFailure era)
  where
  decCBOR = decodeRecordSum "ConwayGovCertPredFailure" $
    \case
      0 -> do
        cred <- decCBOR
        pure (2, ConwayDRepAlreadyRegistered cred)
      1 -> do
        cred <- decCBOR
        pure (2, ConwayDRepNotRegistered cred)
      2 -> do
        deposit <- decCBOR
        expectedDeposit <- decCBOR
        pure (3, ConwayDRepIncorrectDeposit deposit expectedDeposit)
      3 -> do
        coldCred <- decCBOR
        pure (2, ConwayCommitteeHasPreviouslyResigned coldCred)
      4 -> do
        refund <- decCBOR
        expectedRefund <- decCBOR
        pure (3, ConwayDRepIncorrectRefund refund expectedRefund)
      5 -> do
        coldCred <- decCBOR
        pure (2, ConwayCommitteeIsUnknown coldCred)
      k -> invalidKey k

instance
  ( ConwayEraPParams era
  , State (EraRule "GOVCERT" era) ~ VState era
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert (EraCrypto era)
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
  , EraRule "GOVCERT" era ~ ConwayGOVCERT era
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  STS (ConwayGOVCERT era)
  where
  type State (ConwayGOVCERT era) = VState era
  type Signal (ConwayGOVCERT era) = ConwayGovCert (EraCrypto era)
  type Environment (ConwayGOVCERT era) = ConwayGovCertEnv era
  type BaseM (ConwayGOVCERT era) = ShelleyBase
  type PredicateFailure (ConwayGOVCERT era) = ConwayGovCertPredFailure era
  type Event (ConwayGOVCERT era) = Void

  transitionRules = [conwayGovCertTransition @era]

conwayGovCertTransition ::
  ConwayEraPParams era => TransitionRule (ConwayGOVCERT era)
conwayGovCertTransition = do
  TRC
    ( ConwayGovCertEnv {cgcePParams, cgceCurrentEpoch, cgceCurrentCommittee, cgceCommitteeProposals}
      , vState@VState {vsDReps}
      , cert
      ) <-
    judgmentContext
  let ppDRepDeposit = cgcePParams ^. ppDRepDepositL
      ppDRepActivity = cgcePParams ^. ppDRepActivityL
      checkAndOverwriteCommitteeMemberState coldCred newMemberState = do
        let VState {vsCommitteeState = CommitteeState csCommitteeCreds} = vState
            coldCredResigned =
              Map.lookup coldCred csCommitteeCreds >>= \case
                CommitteeMemberResigned {} -> Just coldCred
                CommitteeHotCredential {} -> Nothing
        failOnJust coldCredResigned ConwayCommitteeHasPreviouslyResigned
        let isCurrentMember =
              strictMaybe False (Map.member coldCred . committeeMembers) cgceCurrentCommittee
            committeeUpdateContainsColdCred GovActionState {gasProposalProcedure} =
              case pProcGovAction gasProposalProcedure of
                UpdateCommittee _ _ newMembers _ -> Map.member coldCred newMembers
                _ -> False
            isPotentialFutureMember =
              any committeeUpdateContainsColdCred cgceCommitteeProposals
        isCurrentMember || isPotentialFutureMember ?! ConwayCommitteeIsUnknown coldCred
        pure
          vState
            { vsCommitteeState =
                CommitteeState
                  { csCommitteeCreds = Map.insert coldCred newMemberState csCommitteeCreds
                  }
            }
  case cert of
    ConwayRegDRep cred deposit mAnchor -> do
      Map.notMember cred vsDReps ?! ConwayDRepAlreadyRegistered cred
      deposit == ppDRepDeposit ?! ConwayDRepIncorrectDeposit deposit ppDRepDeposit
      pure
        vState
          { vsDReps =
              Map.insert
                cred
                ( DRepState
                    ( computeDRepExpiryVersioned
                        cgcePParams
                        cgceCurrentEpoch
                        (vState ^. vsNumDormantEpochsL)
                    )
                    mAnchor
                    ppDRepDeposit
                    mempty
                )
                vsDReps
          }
    ConwayUnRegDRep cred refund -> do
      case Map.lookup cred vsDReps of
        Nothing -> failBecause $ ConwayDRepNotRegistered cred
        Just drepState ->
          let paidDeposit = drepState ^. drepDepositL
           in refund == paidDeposit ?! ConwayDRepIncorrectRefund refund paidDeposit
      pure vState {vsDReps = Map.delete cred vsDReps}
    -- Update a DRep expiry along with its anchor.
    ConwayUpdateDRep cred mAnchor -> do
      Map.member cred vsDReps ?! ConwayDRepNotRegistered cred
      pure
        vState
          { vsDReps =
              Map.adjust
                ( \drepState ->
                    drepState
                      & drepExpiryL
                        .~ computeDRepExpiry
                          ppDRepActivity
                          cgceCurrentEpoch
                          (vState ^. vsNumDormantEpochsL)
                      & drepAnchorL .~ mAnchor
                )
                cred
                vsDReps
          }
    ConwayAuthCommitteeHotKey coldCred hotCred ->
      checkAndOverwriteCommitteeMemberState coldCred $ CommitteeHotCredential hotCred
    ConwayResignCommitteeColdKey coldCred anchor ->
      checkAndOverwriteCommitteeMemberState coldCred $ CommitteeMemberResigned anchor

computeDRepExpiryVersioned ::
  ConwayEraPParams era =>
  PParams era ->
  -- | Current epoch
  EpochNo ->
  -- | The count of the dormant epochs
  EpochNo ->
  EpochNo
computeDRepExpiryVersioned pp currentEpoch numDormantEpochs
  -- Starting with version 10, we correctly take into account the number of dormant epochs
  -- when registering a drep
  | HF.bootstrapPhase (pp ^. ppProtocolVersionL) =
      addEpochInterval currentEpoch (pp ^. ppDRepActivityL)
  | otherwise =
      computeDRepExpiry (pp ^. ppDRepActivityL) currentEpoch numDormantEpochs

computeDRepExpiry ::
  -- | DRepActivity PParam
  EpochInterval ->
  -- | Current epoch
  EpochNo ->
  -- | The count of the dormant epochs
  EpochNo ->
  -- | Computed expiry
  EpochNo
computeDRepExpiry ppDRepActivity currentEpoch =
  binOpEpochNo
    (-)
    (addEpochInterval currentEpoch ppDRepActivity)
