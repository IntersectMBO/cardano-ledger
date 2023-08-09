{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  SlotNo,
  epochInfoPure,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (DRepState (..), VState (..), vsDRepsL)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (ConwayEraPParams, ppDRepActivityL)
import Cardano.Ledger.Conway.Era (ConwayGOVCERT)
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraRule, PParams)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRepDistr (drepAnchorL, drepExpiryL)
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole, DRepRole))
import Cardano.Ledger.Slot (epochInfoEpoch)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  Rule,
  STS,
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  liftSTS,
  transitionRules,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ConwayGovCertEnv era = ConwayGovCertEnv
  { cgcePParams :: !(PParams era)
  , cgceCurrentSlot :: !SlotNo
  }

data ConwayGovCertPredFailure era
  = ConwayDRepAlreadyRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepNotRegistered !(Credential 'DRepRole (EraCrypto era))
  | ConwayDRepIncorrectDeposit !Coin
  | ConwayCommitteeHasResigned !(Credential 'ColdCommitteeRole (EraCrypto era))
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
    ConwayDRepIncorrectDeposit deposit ->
      encodeListLen 2
        <> encCBOR (2 :: Word8)
        <> encCBOR deposit
    ConwayCommitteeHasResigned keyH ->
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
        pure (2, ConwayDRepIncorrectDeposit deposit)
      3 -> do
        keyH <- decCBOR
        pure (2, ConwayCommitteeHasResigned keyH)
      k -> invalidKey k

newtype ConwayGovCertEvent era = GovCertEvent (Event (EraRule "GOVCERT" era))

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
  type Event (ConwayGOVCERT era) = ConwayGovCertEvent era

  transitionRules = [conwayGovCertTransition @era]

calcDRepExpiryEpoch ::
  ( ConwayEraPParams era
  , Eq (PredicateFailure (EraRule "GOVCERT" era))
  , Show (PredicateFailure (EraRule "GOVCERT" era))
  , EraRule "GOVCERT" era ~ ConwayGOVCERT era
  ) =>
  SlotNo ->
  EpochNo ->
  Rule (ConwayGOVCERT era) ctx EpochNo
calcDRepExpiryEpoch slot drepActivity = do
  curEpoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot
  pure $ curEpoch + drepActivity

conwayGovCertTransition ::
  (ConwayEraPParams era, EraRule "GOVCERT" era ~ ConwayGOVCERT era) => TransitionRule (ConwayGOVCERT era)
conwayGovCertTransition = do
  TRC
    ( ConwayGovCertEnv {..}
      , vState@VState {vsDReps, vsCommitteeHotKeys}
      , c
      ) <-
    judgmentContext
  case c of
    ConwayRegDRep cred _deposit mAnchor -> do
      Map.notMember cred vsDReps ?! ConwayDRepAlreadyRegistered cred
      -- TODO: check against a new PParam `drepDeposit`, once PParams are updated. -- someCheck ?! ConwayDRepIncorrectDeposit deposit
      drepState <- do
        expiryEpoch <-
          calcDRepExpiryEpoch
            cgceCurrentSlot
            (cgcePParams ^. ppDRepActivityL)
        pure
          DRepState
            { drepExpiry = expiryEpoch
            , drepAnchor = mAnchor
            }
      pure $ vState {vsDReps = Map.insert cred drepState vsDReps}
    ConwayUnRegDRep cred _deposit -> do
      -- TODO: check against a new PParam `drepDeposit`, once PParams are updated. -- someCheck ?! ConwayDRepIncorrectDeposit deposit
      Map.member cred vsDReps ?! ConwayDRepNotRegistered cred
      pure $ vState {vsDReps = Map.delete cred vsDReps}
    ConwayAuthCommitteeHotKey coldK hotK -> do
      checkColdKeyHasNotResigned coldK vsCommitteeHotKeys
      pure $ vState {vsCommitteeHotKeys = Map.insert coldK (Just hotK) vsCommitteeHotKeys}
    ConwayResignCommitteeColdKey coldK -> do
      checkColdKeyHasNotResigned coldK vsCommitteeHotKeys
      pure $ vState {vsCommitteeHotKeys = Map.insert coldK Nothing vsCommitteeHotKeys}
    ConwayUpdateDRep cred mAnchor -> do
      Map.notMember cred vsDReps ?! ConwayDRepNotRegistered cred
      expiryEpoch <-
        calcDRepExpiryEpoch
          cgceCurrentSlot
          (cgcePParams ^. ppDRepActivityL)
      let updateDRepState drepState =
            drepState
              & drepExpiryL .~ expiryEpoch
              & drepAnchorL .~ mAnchor
      pure $ vState & vsDRepsL %~ Map.update (Just . updateDRepState) cred
  where
    checkColdKeyHasNotResigned coldK vsCommitteeHotKeys =
      ((isNothing <$> Map.lookup coldK vsCommitteeHotKeys) /= Just True)
        ?! ConwayCommitteeHasResigned coldK
