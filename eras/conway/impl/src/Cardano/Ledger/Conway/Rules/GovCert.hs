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
) where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  Mismatch (..),
  Relation (..),
  ShelleyBase,
  StrictMaybe,
  addEpochInterval,
  strictMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayGOVCERT, hardforkConwayBootstrapPhase)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  GovAction (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovPurposeId,
  ProposalProcedure (..),
 )
import Cardano.Ledger.Conway.PParams (ppDRepDepositCompactL)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (drepAnchorL, drepDepositL, drepExpiryL)
import Cardano.Slotting.Slot (EpochInterval, binOpEpochNo)
import Control.DeepSeq (NFData)
import Control.Monad (guard)
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
  failOnJust,
  judgmentContext,
  transitionRules,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ConwayGovCertEnv era = ConwayGovCertEnv
  { cgcePParams :: PParams era
  , cgceCurrentEpoch :: EpochNo
  -- ^ Lazy on purpose, because not all certificates need to know the current EpochNo
  , cgceCurrentCommittee :: StrictMaybe (Committee era)
  , cgceCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose) (GovActionState era)
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
  = ConwayDRepAlreadyRegistered (Credential DRepRole)
  | ConwayDRepNotRegistered (Credential DRepRole)
  | ConwayDRepIncorrectDeposit (Mismatch 'RelEQ Coin)
  | ConwayCommitteeHasPreviouslyResigned (Credential ColdCommitteeRole)
  | ConwayDRepIncorrectRefund (Mismatch 'RelEQ Coin)
  | -- | Predicate failure whenever an update to an unknown committee member is
    -- attempted. Current Constitutional Committee and all available proposals will be
    -- searched before reporting this predicate failure.
    ConwayCommitteeIsUnknown (Credential ColdCommitteeRole)
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "GOVCERT" ConwayEra = ConwayGovCertPredFailure ConwayEra

type instance EraRuleEvent "GOVCERT" ConwayEra = VoidEraRule "GOVCERT" ConwayEra

instance InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure ConwayEra

instance NoThunks (ConwayGovCertPredFailure era)

instance NFData (ConwayGovCertPredFailure era)

instance Era era => EncCBOR (ConwayGovCertPredFailure era) where
  encCBOR =
    encode @_ @(ConwayGovCertPredFailure era) . \case
      ConwayDRepAlreadyRegistered cred -> Sum ConwayDRepAlreadyRegistered 0 !> To cred
      ConwayDRepNotRegistered cred -> Sum ConwayDRepNotRegistered 1 !> To cred
      ConwayDRepIncorrectDeposit mm -> Sum ConwayDRepIncorrectDeposit 2 !> ToGroup mm
      ConwayCommitteeHasPreviouslyResigned coldCred -> Sum ConwayCommitteeHasPreviouslyResigned 3 !> To coldCred
      ConwayDRepIncorrectRefund mm -> Sum ConwayDRepIncorrectRefund 4 !> ToGroup mm
      ConwayCommitteeIsUnknown coldCred -> Sum ConwayCommitteeIsUnknown 5 !> To coldCred

instance Typeable era => DecCBOR (ConwayGovCertPredFailure era) where
  decCBOR = decode . Summands "ConwayGovCertPredFailure" $ \case
    0 -> SumD ConwayDRepAlreadyRegistered <! From
    1 -> SumD ConwayDRepNotRegistered <! From
    2 -> SumD ConwayDRepIncorrectDeposit <! FromGroup
    3 -> SumD ConwayCommitteeHasPreviouslyResigned <! From
    4 -> SumD ConwayDRepIncorrectRefund <! FromGroup
    5 -> SumD ConwayCommitteeIsUnknown <! From
    n -> Invalid n

instance
  ( ConwayEraPParams era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
  , EraRule "GOVCERT" era ~ ConwayGOVCERT era
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  , ConwayEraCertState era
  ) =>
  STS (ConwayGOVCERT era)
  where
  type State (ConwayGOVCERT era) = CertState era
  type Signal (ConwayGOVCERT era) = ConwayGovCert
  type Environment (ConwayGOVCERT era) = ConwayGovCertEnv era
  type BaseM (ConwayGOVCERT era) = ShelleyBase
  type PredicateFailure (ConwayGOVCERT era) = ConwayGovCertPredFailure era
  type Event (ConwayGOVCERT era) = Void

  transitionRules = [conwayGovCertTransition @era]

conwayGovCertTransition ::
  ( ConwayEraPParams era
  , ConwayEraCertState era
  ) =>
  TransitionRule (ConwayGOVCERT era)
conwayGovCertTransition = do
  TRC
    ( ConwayGovCertEnv {cgcePParams, cgceCurrentEpoch, cgceCurrentCommittee, cgceCommitteeProposals}
      , certState
      , cert
      ) <-
    judgmentContext
  let ppDRepDepositCompact = cgcePParams ^. ppDRepDepositCompactL
      ppDRepDeposit = fromCompact ppDRepDepositCompact
      ppDRepActivity = cgcePParams ^. ppDRepActivityL
      checkAndOverwriteCommitteeMemberState coldCred newMemberState = do
        let VState {vsCommitteeState = CommitteeState csCommitteeCreds} = certState ^. certVStateL
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
        pure $
          certState
            & certVStateL . vsCommitteeStateL . csCommitteeCredsL %~ Map.insert coldCred newMemberState
  case cert of
    ConwayRegDRep cred deposit mAnchor -> do
      Map.notMember cred (certState ^. certVStateL . vsDRepsL) ?! ConwayDRepAlreadyRegistered cred
      deposit
        == ppDRepDeposit
          ?! ConwayDRepIncorrectDeposit
            Mismatch
              { mismatchSupplied = deposit
              , mismatchExpected = ppDRepDeposit
              }
      let drepState =
            DRepState
              { drepExpiry =
                  computeDRepExpiryVersioned
                    cgcePParams
                    cgceCurrentEpoch
                    (certState ^. certVStateL . vsNumDormantEpochsL)
              , drepAnchor = mAnchor
              , drepDeposit = ppDRepDepositCompact
              , drepDelegs = mempty
              }
      pure $
        certState
          & certVStateL . vsDRepsL %~ Map.insert cred drepState
    ConwayUnRegDRep cred refund -> do
      let mDRepState = Map.lookup cred (certState ^. certVStateL . vsDRepsL)
          drepRefundMismatch = do
            drepState <- mDRepState
            let paidDeposit = drepState ^. drepDepositL
            guard (refund /= paidDeposit)
            pure paidDeposit
      isJust mDRepState ?! ConwayDRepNotRegistered cred
      failOnJust drepRefundMismatch $ ConwayDRepIncorrectRefund . Mismatch refund
      let
        certState' =
          certState & certVStateL . vsDRepsL %~ Map.delete cred
        clearDRepDelegations delegs accountsMap =
          foldr (Map.adjust (dRepDelegationAccountStateL .~ Nothing)) accountsMap delegs
      pure $
        case mDRepState of
          Nothing -> certState'
          Just dRepState ->
            certState'
              & certDStateL . accountsL . accountsMapL
                %~ clearDRepDelegations (drepDelegs dRepState)
    -- Update a DRep expiry along with its anchor.
    ConwayUpdateDRep cred mAnchor -> do
      Map.member cred (certState ^. certVStateL . vsDRepsL) ?! ConwayDRepNotRegistered cred
      pure $
        certState
          & certVStateL . vsDRepsL
            %~ Map.adjust
              ( \drepState ->
                  drepState
                    & drepExpiryL
                      .~ computeDRepExpiry
                        ppDRepActivity
                        cgceCurrentEpoch
                        (certState ^. certVStateL . vsNumDormantEpochsL)
                    & drepAnchorL .~ mAnchor
              )
              cred
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
  | hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL) =
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
