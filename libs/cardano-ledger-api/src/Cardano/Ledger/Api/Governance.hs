{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.Api.Governance (
  EraGov (GovState),
  emptyGovState,
  getProposedPPUpdates,

  -- * Shelley
  ShelleyGovState (..),
  ProposedPPUpdates (..),
  emptyPPPUpdates,

  -- * Conway

  -- ** Governance Procedures
  VotingProcedure (..),
  VotingProcedures (..),
  ProposalProcedure (..),

  -- ** Constitution
  Constitution (..),
  constitutionHashL,
  constitutionScriptL,

  -- ** Governance State
  ConwayGovState (..),
  cgRatifyL,
  cgGovL,
  GovActionsState (..),
  RatifyState (..),
  EnactState (..),
  Voter (..),
  Vote (..),

  -- ** Governance Action
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovActionPurpose (..),
  PrevGovActionId (..),
  govActionIdToText,

  -- *** Anchor
  Anchor (..),
  AnchorData (..),
  AnchorDataHash,
  hashAnchorData,
) where

-- Lenses

import Cardano.Ledger.Allegra.Core (Constitution (..))
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
  AnchorDataHash,
  ConwayGovState (..),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovActionsState (..),
  PrevGovActionId (..),
  ProposalProcedure (..),
  RatifyState (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  cgGovL,
  cgRatifyL,
  constitutionHashL,
  constitutionScriptL,
  govActionIdToText,
 )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeHash, SafeToHash, hashAnnotated)
import Cardano.Ledger.Shelley.Governance (
  EraGov (GovState),
  ShelleyGovState (..),
  emptyGovState,
  getProposedPPUpdates,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), emptyPPPUpdates)
import Data.ByteString (ByteString)

newtype AnchorData c = AnchorData ByteString
  deriving (Eq, SafeToHash)

instance HashAnnotated (AnchorData c) AnchorDataHash c

-- | Hash `AnchorData`
hashAnchorData :: Crypto c => AnchorData c -> SafeHash c AnchorDataHash
hashAnchorData = hashAnnotated
