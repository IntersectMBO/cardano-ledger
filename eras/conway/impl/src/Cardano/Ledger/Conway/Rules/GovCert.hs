{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  updateDRepExpiry,
)
where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  ShelleyBase,
  addEpochInterval,
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
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRepState (..), drepAnchorL, drepDepositL, drepExpiryL)
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole, DRepRole))
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
  }
  deriving (Generic)

instance (NFData (PParams era), Era era) => NFData (ConwayGovCertEnv era)

deriving instance Show (PParams era) => Show (ConwayGovCertEnv era)

deriving instance Eq (PParams era) => Eq (ConwayGovCertEnv era)

data ConwayGovCertPredFailure era
  = ConwayDRepAlreadyRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepNotRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepIncorrectDeposit !Coin !Coin -- The first is the given and the second is the expected deposit
  | ConwayCommitteeHasPreviouslyResigned !(Credential 'ColdCommitteeRole (EraCrypto era))
  | ConwayDRepIncorrectRefund !Coin !Coin -- The first is the given and the second is the expected refund
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
    ConwayCommitteeHasPreviouslyResigned keyH ->
      encodeListLen 2
        <> encCBOR (3 :: Word8)
        <> encCBOR keyH
    ConwayDRepIncorrectRefund refund expectedRefund ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR refund
        <> encCBOR expectedRefund

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
        keyH <- decCBOR
        pure (2, ConwayCommitteeHasPreviouslyResigned keyH)
      4 -> do
        refund <- decCBOR
        expectedRefund <- decCBOR
        pure (3, ConwayDRepIncorrectRefund refund expectedRefund)
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
    ( ConwayGovCertEnv {cgcePParams, cgceCurrentEpoch}
      , vState@VState {vsDReps}
      , c
      ) <-
    judgmentContext
  let ppDRepDeposit = cgcePParams ^. ppDRepDepositL
      ppDRepActivity = cgcePParams ^. ppDRepActivityL
  case c of
    ConwayRegDRep cred deposit mAnchor -> do
      Map.notMember cred vsDReps ?! ConwayDRepAlreadyRegistered cred
      deposit == ppDRepDeposit ?! ConwayDRepIncorrectDeposit deposit ppDRepDeposit
      pure
        vState
          { vsDReps =
              Map.insert
                cred
                (DRepState (addEpochInterval cgceCurrentEpoch ppDRepActivity) mAnchor ppDRepDeposit)
                vsDReps
          }
    ConwayUnRegDRep cred refund -> do
      case Map.lookup cred vsDReps of
        Nothing -> failBecause $ ConwayDRepNotRegistered cred
        Just drepState ->
          let paidDeposit = drepState ^. drepDepositL
           in refund == paidDeposit ?! ConwayDRepIncorrectRefund refund paidDeposit
      pure vState {vsDReps = Map.delete cred vsDReps}
    ConwayAuthCommitteeHotKey coldCred hotCred ->
      checkAndOverwriteCommitteeHotCred vState coldCred $ CommitteeHotCredential hotCred
    ConwayResignCommitteeColdKey coldCred anchor ->
      checkAndOverwriteCommitteeHotCred vState coldCred $ CommitteeMemberResigned anchor
    -- Update a DRep expiry too along with its anchor.
    ConwayUpdateDRep cred mAnchor -> do
      Map.member cred vsDReps ?! ConwayDRepNotRegistered cred
      pure
        vState
          { vsDReps =
              updateDRepExpiry ppDRepActivity cgceCurrentEpoch (vState ^. vsNumDormantEpochsL) cred $
                Map.adjust (\drepState -> drepState & drepAnchorL .~ mAnchor) cred vsDReps
          }
  where
    checkColdCredHasNotResigned coldCred csCommitteeCreds =
      case Map.lookup coldCred csCommitteeCreds of
        Just (CommitteeMemberResigned _) -> failBecause $ ConwayCommitteeHasPreviouslyResigned coldCred
        _ -> pure ()
    checkAndOverwriteCommitteeHotCred vState@VState {vsCommitteeState = CommitteeState csCommitteeCreds} coldCred hotCred = do
      checkColdCredHasNotResigned coldCred csCommitteeCreds
      pure
        vState
          { vsCommitteeState =
              CommitteeState
                { csCommitteeCreds = Map.insert coldCred hotCred csCommitteeCreds
                }
          }

updateDRepExpiry ::
  -- | DRepActivity PParam
  EpochInterval ->
  -- | Current epoch
  EpochNo ->
  -- | The count of the dormant epochs
  EpochNo ->
  -- | DRep credential
  Credential 'DRepRole c ->
  -- | DRep map from DRepState
  Map.Map (Credential 'DRepRole c) (DRepState c) ->
  Map.Map (Credential 'DRepRole c) (DRepState c)
updateDRepExpiry ppDRepActivity currentEpoch numDormantEpochs =
  Map.adjust $
    drepExpiryL
      .~ binOpEpochNo
        (-)
        (addEpochInterval currentEpoch ppDRepActivity)
        numDormantEpochs
