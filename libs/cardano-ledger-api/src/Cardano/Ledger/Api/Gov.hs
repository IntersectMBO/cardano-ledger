{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.Api.Gov (
  EraGov (GovState),
  emptyGovState,
  getProposedPPUpdates,

  -- * Shelley
  ShelleyGovState (..),
  ProposedPPUpdates (..),
  emptyPPPUpdates,

  -- * Conway

  -- ** Conway Governance
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

  -- *** Anchor
  Anchor (..),
  AnchorData (..),
  AnchorDataHash,
  hashAnchorData,
) where

import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Conway.Gov (
  Anchor (..),
  AnchorDataHash,
  ConwayGovState (..),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovActionsState (..),
  RatifyState (..),
  Vote (..),
  Voter (..),
  -- Lenses

  cgGovL,
  cgRatifyL,
 )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeHash, SafeToHash, hashAnnotated)
import Cardano.Ledger.Shelley.Gov (
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
