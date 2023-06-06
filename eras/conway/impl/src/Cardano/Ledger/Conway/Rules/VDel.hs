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

module Cardano.Ledger.Conway.Rules.VDel (
  ConwayVDEL,
  ConwayVDelEvent (..),
  ConwayVDelPredFailure (..),
)
where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (VState (..))
import Cardano.Ledger.Conway.Era (ConwayVDEL)
import Cardano.Ledger.Conway.TxCert (ConwayCommitteeCert (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraPParams, EraRule, PParams)
import Cardano.Ledger.Credential (Credential)
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data ConwayVDelPredFailure era
  = ConwayDRepAlreadyRegisteredVDEL !(Credential 'Voting (EraCrypto era))
  | ConwayDRepNotRegisteredVDEL !(Credential 'Voting (EraCrypto era))
  | ConwayCommitteeAlreadyRegisteredVDEL !(KeyHash 'CommitteeColdKey (EraCrypto era))
  | ConwayCommitteeNotRegisteredVDEL !(KeyHash 'CommitteeColdKey (EraCrypto era))
  deriving (Show, Eq, Generic)

instance NoThunks (ConwayVDelPredFailure era)

instance NFData (ConwayVDelPredFailure era)

instance Typeable era => EncCBOR (ConwayVDelPredFailure era) where
  encCBOR = \case
    ConwayVDelPredFailure -> encodeListLen 1 <> encCBOR (0 :: Word8)

instance Typeable era => DecCBOR (ConwayVDelPredFailure era) where
  decCBOR = decodeRecordSum "ConwayVDelPredFailure" $
    \case
      0 -> pure (1, ConwayVDelPredFailure)
      k -> invalidKey k

newtype ConwayVDelEvent era = VDelEvent (Event (EraRule "VDEL" era))

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
  type PredicateFailure (ConwayVDEL era) = ConwayVDelPredFailure era
  type Event (ConwayVDEL era) = ConwayVDelEvent era

  transitionRules = [conwayVDelTransition @era]

conwayVDelTransition :: TransitionRule (ConwayVDEL era)
conwayVDelTransition = do
  TRC
    ( _pp
      , vState@VState {vsDReps, vsCCHotKeys}
      , c
      ) <-
    judgmentContext
  case c of
    ConwayDRepReg cred -> do
      Set.notMember cred vsDReps ?! ConwayDRepAlreadyRegisteredVDEL cred
      -- TODO: check against a new PParam `drepDeposit`, once PParams are updated.
      pure $
        vState
          { vsDReps = Set.insert cred vsDReps
          }
    ConwayDRepUnReg cred -> do
      Set.member cred vsDReps ?! ConwayDRepNotRegisteredVDEL cred
      pure $
        vState
          { vsDReps = Set.delete cred vsDReps
          }
    ConwayAuthCommitteeHotKey coldK hotK -> do
      -- TODO: implement after the spec is to be updated w.r.t. checking if the coldK belongs to CC.
      pure $
        vState
          { vsCCHotKeys = Map.insert coldK hotK vsCCHotKeys
          }
    ConwayResignCommitteeColdKey coldK -> do
      Map.member coldK vsCCHotKeys ?! ConwayCommitteeNotRegisteredVDEL coldK
      pure $
        vState
          { vsCCHotKeys = Map.delete coldK vsCCHotKeys
          }
