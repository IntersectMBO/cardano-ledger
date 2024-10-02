{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Newpp (
  ShelleyNEWPP,
  ShelleyNewppState (..),
  NewppEnv (..),
  PredicateFailure,
) where

import Cardano.Ledger.BaseTypes (Globals (quorum), ShelleyBase)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyNEWPP)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  UTxOState,
 )
import Cardano.Ledger.Shelley.PParams (
  ProposedPPUpdates (ProposedPPUpdates),
  emptyPPPUpdates,
  hasLegalProtVerUpdate,
 )
import Cardano.Ledger.Shelley.Rules.Ppup (votedFuturePParams)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
 )
import Data.Default.Class (Default, def)
import Data.Void (Void)
import Data.Word (Word64)

data ShelleyNewppState era
  = NewppState (PParams era) (ShelleyGovState era)

data NewppEnv era = NewppEnv
  { neCertState :: !(CertState era)
  , neUTxOState :: !(UTxOState era)
  }

instance
  ( EraGov era
  , GovState era ~ ShelleyGovState era
  , ProtVerAtMost era 8
  ) =>
  STS (ShelleyNEWPP era)
  where
  type State (ShelleyNEWPP era) = ShelleyNewppState era
  type Signal (ShelleyNEWPP era) = PParams era
  type Environment (ShelleyNEWPP era) = NewppEnv era
  type BaseM (ShelleyNEWPP era) = ShelleyBase
  type PredicateFailure (ShelleyNEWPP era) = Void
  transitionRules = [newPpTransition]

instance EraPParams era => Default (ShelleyNewppState era) where
  def = NewppState def def

newPpTransition ::
  forall era.
  ( GovState era ~ ShelleyGovState era
  , EraGov era
  , ProtVerAtMost era 8
  ) =>
  TransitionRule (ShelleyNEWPP era)
newPpTransition = do
  TRC
    ( NewppEnv _certState _utxoState
      , NewppState _pp ppupState
      , ppNew
      ) <-
    judgmentContext
  coreNodeQuorum <- liftSTS $ asks quorum
  pure $ updatePpup coreNodeQuorum ppupState ppNew

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  ( EraPParams era
  , GovState era ~ ShelleyGovState era
  , ProtVerAtMost era 8
  ) =>
  Word64 ->
  GovState era ->
  PParams era ->
  ShelleyNewppState era
updatePpup !coreNodeQuorum ppupState pp =
  NewppState pp $
    ppupState
      { sgsCurProposals = curProposals
      , sgsFutureProposals = emptyPPPUpdates
      , sgsFuturePParams =
          PotentialPParamsUpdate $ votedFuturePParams curProposals pp coreNodeQuorum
      }
  where
    ProposedPPUpdates newProposals = sgsFutureProposals ppupState
    curProposals =
      if all (hasLegalProtVerUpdate pp) newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
