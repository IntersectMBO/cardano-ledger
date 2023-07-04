{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.GovCert (
  ConwayVDEL,
  ConwayGovCertEvent (..),
  ConwayGovCertPredFailure (..),
)
where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (VState (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Era (ConwayVDEL)
import Cardano.Ledger.Conway.TxCert (ConwayCommitteeCert (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraPParams, EraRule, PParams)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (CommitteeColdKey, Voting))
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
  judgmentContext,
  transitionRules,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data ConwayGovCertPredFailure era
  = ConwayDRepAlreadyRegisteredVDEL !(Credential 'Voting (EraCrypto era))
  | ConwayDRepNotRegisteredVDEL !(Credential 'Voting (EraCrypto era))
  | ConwayDRepIncorrectDepositVDEL !Coin
  | ConwayCommitteeHasResignedVDEL !(KeyHash 'CommitteeColdKey (EraCrypto era))
  deriving (Show, Eq, Generic)

instance NoThunks (ConwayGovCertPredFailure era)

instance NFData (ConwayGovCertPredFailure era)

instance
  (Typeable era, Crypto (EraCrypto era)) =>
  EncCBOR (ConwayGovCertPredFailure era)
  where
  encCBOR = \case
    ConwayDRepAlreadyRegisteredVDEL cred ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR cred
    ConwayDRepNotRegisteredVDEL cred ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR cred
    ConwayDRepIncorrectDepositVDEL deposit ->
      encodeListLen 2
        <> encCBOR (2 :: Word8)
        <> encCBOR deposit
    ConwayCommitteeHasResignedVDEL keyH ->
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
        pure (2, ConwayDRepAlreadyRegisteredVDEL cred)
      1 -> do
        cred <- decCBOR
        pure (2, ConwayDRepNotRegisteredVDEL cred)
      2 -> do
        deposit <- decCBOR
        pure (2, ConwayDRepIncorrectDepositVDEL deposit)
      3 -> do
        keyH <- decCBOR
        pure (2, ConwayCommitteeHasResignedVDEL keyH)
      k -> invalidKey k

newtype ConwayGovCertEvent era = GovCertEvent (Event (EraRule "VDEL" era))

instance
  ( EraPParams era
  , State (EraRule "VDEL" era) ~ VState era
  , Signal (EraRule "VDEL" era) ~ ConwayCommitteeCert (EraCrypto era)
  , Environment (EraRule "VDEL" era) ~ PParams era
  , EraRule "VDEL" era ~ ConwayVDEL era
  , Eq (PredicateFailure (EraRule "VDEL" era))
  , Show (PredicateFailure (EraRule "VDEL" era))
  ) =>
  STS (ConwayVDEL era)
  where
  type State (ConwayVDEL era) = VState era
  type Signal (ConwayVDEL era) = ConwayCommitteeCert (EraCrypto era)
  type Environment (ConwayVDEL era) = PParams era
  type BaseM (ConwayVDEL era) = ShelleyBase
  type PredicateFailure (ConwayVDEL era) = ConwayGovCertPredFailure era
  type Event (ConwayVDEL era) = ConwayGovCertEvent era

  transitionRules = [conwayGovCertTransition @era]

conwayGovCertTransition :: TransitionRule (ConwayVDEL era)
conwayGovCertTransition = do
  TRC
    ( _pp
      , vState@VState {vsDReps, vsCommitteeHotKeys}
      , c
      ) <-
    judgmentContext
  case c of
    ConwayRegDRep cred _deposit -> do
      Set.notMember cred vsDReps ?! ConwayDRepAlreadyRegisteredVDEL cred
      -- TODO: check against a new PParam `drepDeposit`, once PParams are updated. -- someCheck ?! ConwayDRepIncorrectDeposit deposit
      pure $ vState {vsDReps = Set.insert cred vsDReps}
    ConwayUnRegDRep cred _deposit -> do
      -- TODO: check against a new PParam `drepDeposit`, once PParams are updated. -- someCheck ?! ConwayDRepIncorrectDeposit deposit
      Set.member cred vsDReps ?! ConwayDRepNotRegisteredVDEL cred
      pure $ vState {vsDReps = Set.delete cred vsDReps}
    ConwayAuthCommitteeHotKey coldK hotK -> do
      checkColdKeyHasNotResigned coldK vsCommitteeHotKeys
      pure $ vState {vsCommitteeHotKeys = Map.insert coldK (Just hotK) vsCommitteeHotKeys}
    ConwayResignCommitteeColdKey coldK -> do
      checkColdKeyHasNotResigned coldK vsCommitteeHotKeys
      pure $ vState {vsCommitteeHotKeys = Map.insert coldK Nothing vsCommitteeHotKeys}
  where
    checkColdKeyHasNotResigned coldK vsCommitteeHotKeys =
      (isNothing <$> Map.lookup coldK vsCommitteeHotKeys)
        /= Just True
        ?! ConwayCommitteeHasResignedVDEL coldK
