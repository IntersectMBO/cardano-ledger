{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Mempool (
  ConwayMEMPOOL,
  ConwayMempoolEvent (..),
  ConwayMempoolPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR, ToCBOR)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayMEMPOOL)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  Voter (..),
  authorizedElectedHotCommitteeCredentials,
  unVotingProcedures,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Control.DeepSeq (NFData)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  failOnNonEmpty,
  judgmentContext,
  tellEvent,
  transitionRules,
 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text as T (Text, pack)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

newtype ConwayMempoolPredFailure era = ConwayMempoolPredFailure Text
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, NFData, ToCBOR, FromCBOR, EncCBOR, DecCBOR)

type instance EraRuleFailure "MEMPOOL" (ConwayEra c) = ConwayMempoolPredFailure (ConwayEra c)
instance InjectRuleFailure "MEMPOOL" ConwayMempoolPredFailure (ConwayEra c)

newtype ConwayMempoolEvent era = ConwayMempoolEvent Text
  deriving (Generic, Eq)
  deriving newtype (NFData)

type instance EraRuleEvent "MEMPOOL" (ConwayEra c) = ConwayMempoolEvent (ConwayEra c)

instance
  (EraTx era, ConwayEraTxBody era, ConwayEraGov era) =>
  STS (ConwayMEMPOOL era)
  where
  type State (ConwayMEMPOOL era) = LedgerState era
  type Signal (ConwayMEMPOOL era) = Tx era
  type Environment (ConwayMEMPOOL era) = LedgerEnv era
  type BaseM (ConwayMEMPOOL era) = ShelleyBase
  type PredicateFailure (ConwayMEMPOOL era) = ConwayMempoolPredFailure era
  type Event (ConwayMEMPOOL era) = ConwayMempoolEvent era

  transitionRules = [mempoolTransition @era]

mempoolTransition ::
  (EraTx era, ConwayEraTxBody era, ConwayEraGov era) => TransitionRule (ConwayMEMPOOL era)
mempoolTransition = do
  TRC (_ledgerEnv, ledgerState, tx) <-
    judgmentContext
  -- This rule only gets invoked on transactions within the mempool.
  -- Add checks here that sanitize undesired transactions.
  tellEvent . ConwayMempoolEvent . ("Mempool rule for tx " <>) . T.pack . show $ txIdTx tx
  let
    authorizedElectedHotCreds = authorizedElectedHotCommitteeCredentials ledgerState
    collectUnelectedCommitteeVotes !unelectedHotCreds voter _ =
      case voter of
        CommitteeVoter hotCred
          | hotCred `Set.notMember` authorizedElectedHotCreds ->
              Set.insert hotCred unelectedHotCreds
        _ -> unelectedHotCreds
    unelectedCommitteeVoters =
      Map.foldlWithKey' collectUnelectedCommitteeVotes Set.empty $
        unVotingProcedures (tx ^. bodyTxL . votingProceduresTxBodyL)
    addPrefix =
      ("Unelected committee members are not allowed to cast votes: " <>)
  failOnNonEmpty unelectedCommitteeVoters $
    ConwayMempoolPredFailure . addPrefix . T.pack . show . NE.toList
  pure ledgerState
