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

module Cardano.Ledger.Dijkstra.Rules.GovCert (
  DijkstraGOVCERT,
  DijkstraGovCertPredFailure (..),
  DijkstraGovCertEnv (..),
  computeDRepExpiry,
)
where

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
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraGOVCERT)
import Cardano.Ledger.Dijkstra.Governance (
  Committee (..),
  GovAction (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovPurposeId,
  ProposalProcedure (..),
 )
import Cardano.Ledger.Dijkstra.State (
  DijkstraEraCertState (..),
  VState (..),
  csCommitteeCredsL,
  vsCommitteeStateL,
  vsDRepsL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Dijkstra.TxCert (DijkstraGovCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRepState (..), drepAnchorL, drepDepositL, drepExpiryL)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.State (
  CommitteeAuthorization (..),
  CommitteeState (..),
  EraCertState (..),
  dsUnifiedL,
 )
import qualified Cardano.Ledger.UMap as UM
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

data DijkstraGovCertEnv era = DijkstraGovCertEnv
  { cgcePParams :: PParams era
  , cgceCurrentEpoch :: EpochNo
  -- ^ Lazy on purpose, because not all certificates need to know the current EpochNo
  , cgceCurrentCommittee :: StrictMaybe (Committee era)
  , cgceCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose era) (GovActionState era)
  -- ^ All of the `UpdateCommittee` proposals
  }
  deriving (Generic)

instance EraPParams era => EncCBOR (DijkstraGovCertEnv era) where
  encCBOR x@(DijkstraGovCertEnv _ _ _ _) =
    let DijkstraGovCertEnv {..} = x
     in encode $
          Rec DijkstraGovCertEnv
            !> To cgcePParams
            !> To cgceCurrentEpoch
            !> To cgceCurrentCommittee
            !> To cgceCommitteeProposals

instance EraPParams era => NFData (DijkstraGovCertEnv era)

deriving instance EraPParams era => Show (DijkstraGovCertEnv era)

deriving instance EraPParams era => Eq (DijkstraGovCertEnv era)

data DijkstraGovCertPredFailure era
  = DijkstraDRepAlreadyRegistered (Credential 'DRepRole)
  | DijkstraDRepNotRegistered (Credential 'DRepRole)
  | DijkstraDRepIncorrectDeposit (Mismatch 'RelEQ Coin)
  | DijkstraCommitteeHasPreviouslyResigned (Credential 'ColdCommitteeRole)
  | DijkstraDRepIncorrectRefund (Mismatch 'RelEQ Coin)
  | -- | Predicate failure whenever an update to an unknown committee member is
    -- attempted. Current Constitutional Committee and all available proposals will be
    -- searched before reporting this predicate failure.
    DijkstraCommitteeIsUnknown (Credential 'ColdCommitteeRole)
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "GOVCERT" DijkstraEra = DijkstraGovCertPredFailure DijkstraEra

type instance EraRuleEvent "GOVCERT" DijkstraEra = VoidEraRule "GOVCERT" DijkstraEra

instance InjectRuleFailure "GOVCERT" DijkstraGovCertPredFailure DijkstraEra

instance NoThunks (DijkstraGovCertPredFailure era)

instance NFData (DijkstraGovCertPredFailure era)

instance Era era => EncCBOR (DijkstraGovCertPredFailure era) where
  encCBOR =
    encode @_ @(DijkstraGovCertPredFailure era) . \case
      DijkstraDRepAlreadyRegistered cred -> Sum DijkstraDRepAlreadyRegistered 0 !> To cred
      DijkstraDRepNotRegistered cred -> Sum DijkstraDRepNotRegistered 1 !> To cred
      DijkstraDRepIncorrectDeposit mm -> Sum DijkstraDRepIncorrectDeposit 2 !> ToGroup mm
      DijkstraCommitteeHasPreviouslyResigned coldCred -> Sum DijkstraCommitteeHasPreviouslyResigned 3 !> To coldCred
      DijkstraDRepIncorrectRefund mm -> Sum DijkstraDRepIncorrectRefund 4 !> ToGroup mm
      DijkstraCommitteeIsUnknown coldCred -> Sum DijkstraCommitteeIsUnknown 5 !> To coldCred

instance Typeable era => DecCBOR (DijkstraGovCertPredFailure era) where
  decCBOR = decode . Summands "DijkstraGovCertPredFailure" $ \case
    0 -> SumD DijkstraDRepAlreadyRegistered <! From
    1 -> SumD DijkstraDRepNotRegistered <! From
    2 -> SumD DijkstraDRepIncorrectDeposit <! FromGroup
    3 -> SumD DijkstraCommitteeHasPreviouslyResigned <! From
    4 -> SumD DijkstraDRepIncorrectRefund <! FromGroup
    5 -> SumD DijkstraCommitteeIsUnknown <! From
    n -> Invalid n

instance
  ( DijkstraEraPParams era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Signal (EraRule "GOVCERT" era) ~ DijkstraGovCert
  , Environment (EraRule "GOVCERT" era) ~ DijkstraGovCertEnv era
  , EraRule "GOVCERT" era ~ DijkstraGOVCERT era
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  , DijkstraEraCertState era
  ) =>
  STS (DijkstraGOVCERT era)
  where
  type State (DijkstraGOVCERT era) = CertState era
  type Signal (DijkstraGOVCERT era) = DijkstraGovCert
  type Environment (DijkstraGOVCERT era) = DijkstraGovCertEnv era
  type BaseM (DijkstraGOVCERT era) = ShelleyBase
  type PredicateFailure (DijkstraGOVCERT era) = DijkstraGovCertPredFailure era
  type Event (DijkstraGOVCERT era) = Void

  transitionRules = [conwayGovCertTransition @era]

conwayGovCertTransition ::
  ( DijkstraEraPParams era
  , DijkstraEraCertState era
  ) =>
  TransitionRule (DijkstraGOVCERT era)
conwayGovCertTransition = do
  TRC
    ( DijkstraGovCertEnv {cgcePParams, cgceCurrentEpoch, cgceCurrentCommittee, cgceCommitteeProposals}
      , certState
      , cert
      ) <-
    judgmentContext
  let ppDRepDeposit = cgcePParams ^. ppDRepDepositL
      ppDRepActivity = cgcePParams ^. ppDRepActivityL
      checkAndOverwriteCommitteeMemberState coldCred newMemberState = do
        let VState {vsCommitteeState = CommitteeState csCommitteeCreds} = certState ^. certVStateL
            coldCredResigned =
              Map.lookup coldCred csCommitteeCreds >>= \case
                CommitteeMemberResigned {} -> Just coldCred
                CommitteeHotCredential {} -> Nothing
        failOnJust coldCredResigned DijkstraCommitteeHasPreviouslyResigned
        let isCurrentMember =
              strictMaybe False (Map.member coldCred . committeeMembers) cgceCurrentCommittee
            committeeUpdateContainsColdCred GovActionState {gasProposalProcedure} =
              case pProcGovAction gasProposalProcedure of
                UpdateCommittee _ _ newMembers _ -> Map.member coldCred newMembers
                _ -> False
            isPotentialFutureMember =
              any committeeUpdateContainsColdCred cgceCommitteeProposals
        isCurrentMember || isPotentialFutureMember ?! DijkstraCommitteeIsUnknown coldCred
        pure $
          certState
            & certVStateL . vsCommitteeStateL . csCommitteeCredsL %~ Map.insert coldCred newMemberState
  case cert of
    DijkstraRegDRep cred deposit mAnchor -> do
      Map.notMember cred (certState ^. certVStateL . vsDRepsL) ?! DijkstraDRepAlreadyRegistered cred
      deposit
        == ppDRepDeposit
          ?! DijkstraDRepIncorrectDeposit
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
              , drepDeposit = ppDRepDeposit
              , drepDelegs = mempty
              }
      pure $
        certState
          & certVStateL . vsDRepsL %~ Map.insert cred drepState
    DijkstraUnRegDRep cred refund -> do
      let mDRepState = Map.lookup cred (certState ^. certVStateL . vsDRepsL)
          drepRefundMismatch = do
            drepState <- mDRepState
            let paidDeposit = drepState ^. drepDepositL
            guard (refund /= paidDeposit)
            pure paidDeposit
      isJust mDRepState ?! DijkstraDRepNotRegistered cred
      failOnJust drepRefundMismatch $ DijkstraDRepIncorrectRefund . Mismatch refund
      let
        certState' =
          certState & certVStateL . vsDRepsL %~ Map.delete cred
      pure $
        case mDRepState of
          Nothing -> certState'
          Just dRepState ->
            certState'
              & certDStateL . dsUnifiedL
                .~ drepDelegs dRepState UM.⋪ UM.DRepUView (certState ^. certDStateL . dsUnifiedL)
    -- Update a DRep expiry along with its anchor.
    DijkstraUpdateDRep cred mAnchor -> do
      Map.member cred (certState ^. certVStateL . vsDRepsL) ?! DijkstraDRepNotRegistered cred
      pure $
        certState
          & certVStateL . vsDRepsL
            %~ ( Map.adjust
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
               )
    DijkstraAuthCommitteeHotKey coldCred hotCred ->
      checkAndOverwriteCommitteeMemberState coldCred $ CommitteeHotCredential hotCred
    DijkstraResignCommitteeColdKey coldCred anchor ->
      checkAndOverwriteCommitteeMemberState coldCred $ CommitteeMemberResigned anchor

computeDRepExpiryVersioned ::
  DijkstraEraPParams era =>
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
