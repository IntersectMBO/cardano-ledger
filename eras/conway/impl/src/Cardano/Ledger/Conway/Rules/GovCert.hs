{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  ConwayGovCertEvent (..),
  ConwayGovCertPredFailure (..),
  ConwayGovCertEnv (..),
)
where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  ShelleyBase,
  addEpochInterval,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (CommitteeState (..), VState (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Era (ConwayGOVCERT)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepActivityL, ppDRepDepositL)
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraRule, PParams)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRepState (..), drepAnchorL, drepDepositL, drepExpiryL)
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole, DRepRole, HotCommitteeRole))
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
  tellEvent,
  transitionRules,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ConwayGovCertEnv era = ConwayGovCertEnv
  { cgcePParams :: !(PParams era)
  , cgceCurrentEpoch :: !EpochNo
  }

deriving instance Show (PParams era) => Show (ConwayGovCertEnv era)

deriving instance Eq (PParams era) => Eq (ConwayGovCertEnv era)

data ConwayGovCertPredFailure era
  = ConwayDRepAlreadyRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepNotRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepIncorrectDeposit !Coin !Coin -- The first is the given and the second is the expected deposit
  | ConwayCommitteeHasPreviouslyResigned !(Credential 'ColdCommitteeRole (EraCrypto era))
  deriving (Show, Eq, Generic)

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
      k -> invalidKey k

data ConwayGovCertEvent c
  = -- | Event indicating that a DRep is registrating
    DRepRegistration !(Credential 'DRepRole c)
  | -- | Event indicating that a DRep is unregistrating
    DRepUnregistration !(Credential 'DRepRole c)
  | -- | Event indicating that the hot key (corresponding to the given cold key) is being overwritten
    OverwriteCommitteeHotKey !(Credential 'ColdCommitteeRole c) !(Credential 'HotCommitteeRole c)
  | -- | Event indicating that a cold key is being resigned
    ResignCommitteeColdKey !(Credential 'ColdCommitteeRole c)

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
  type Event (ConwayGOVCERT era) = ConwayGovCertEvent (EraCrypto era)

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
      tellEvent $ DRepRegistration cred
      pure
        vState
          { vsDReps =
              Map.insert cred (DRepState (addEpochInterval cgceCurrentEpoch ppDRepActivity) mAnchor ppDRepDeposit) vsDReps
          }
    ConwayUnRegDRep cred deposit -> do
      checkRegistrationAndDepositAgainstPaidDeposit vsDReps cred deposit
      tellEvent $ DRepUnregistration cred
      pure vState {vsDReps = Map.delete cred vsDReps}
    ConwayAuthCommitteeHotKey coldCred hotCred -> do
      vState' <- checkAndOverwriteCommitteeHotCred vState coldCred $ Just hotCred
      tellEvent $ OverwriteCommitteeHotKey coldCred hotCred
      return vState'
    ConwayResignCommitteeColdKey coldCred _ -> do
      vState' <- checkAndOverwriteCommitteeHotCred vState coldCred Nothing
      tellEvent $ ResignCommitteeColdKey coldCred
      return vState'
    -- Update a DRep expiry too along with its anchor.
    ConwayUpdateDRep cred mAnchor -> do
      Map.member cred vsDReps ?! ConwayDRepNotRegistered cred
      pure
        vState
          { vsDReps =
              Map.adjust
                ( \drepState ->
                    drepState
                      & drepExpiryL .~ addEpochInterval cgceCurrentEpoch ppDRepActivity
                      & drepAnchorL .~ mAnchor
                )
                cred
                vsDReps
          }
  where
    checkColdCredHasNotResigned coldCred csCommitteeCreds =
      ((isNothing <$> Map.lookup coldCred csCommitteeCreds) /= Just True)
        ?! ConwayCommitteeHasPreviouslyResigned coldCred
    checkRegistrationAndDepositAgainstPaidDeposit vsDReps cred deposit =
      case Map.lookup cred vsDReps of
        Nothing -> failBecause $ ConwayDRepNotRegistered cred
        Just drepState ->
          let paidDeposit = drepState ^. drepDepositL
           in deposit == paidDeposit ?! ConwayDRepIncorrectDeposit deposit paidDeposit
    checkAndOverwriteCommitteeHotCred vState@VState {vsCommitteeState = CommitteeState csCommitteeCreds} coldCred hotCred = do
      checkColdCredHasNotResigned coldCred csCommitteeCreds
      pure
        vState
          { vsCommitteeState =
              CommitteeState
                { csCommitteeCreds = Map.insert coldCred hotCred csCommitteeCreds
                }
          }
