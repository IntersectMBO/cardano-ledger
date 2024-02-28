{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
  ShelleyNewppPredFailure (..),
  PredicateFailure,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyNEWPP)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  UTxOState (utxosDeposited),
  totalObligation,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.PParams (
  ProposedPPUpdates (ProposedPPUpdates),
  emptyPPPUpdates,
  hasLegalProtVerUpdate,
 )
import Control.DeepSeq (NFData)
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  (?!),
 )
import Data.Default.Class (Default, def)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ShelleyNewppState era
  = NewppState (PParams era) (ShelleyGovState era)

data NewppEnv era = NewppEnv
  { neCertState :: !(CertState era)
  , neUTxOState :: !(UTxOState era)
  }

data ShelleyNewppPredFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyNewppPredFailure era)

instance NFData (ShelleyNewppPredFailure era)

instance
  ( EraGov era
  , GovState era ~ ShelleyGovState era
  , ProtVerAtMost era 8
  ) =>
  STS (ShelleyNEWPP era)
  where
  type State (ShelleyNEWPP era) = ShelleyNewppState era
  type Signal (ShelleyNEWPP era) = Maybe (PParams era)
  type Environment (ShelleyNEWPP era) = NewppEnv era
  type BaseM (ShelleyNEWPP era) = ShelleyBase
  type PredicateFailure (ShelleyNEWPP era) = ShelleyNewppPredFailure era
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
    ( NewppEnv certState utxoState
      , NewppState pp ppupState
      , mppNew
      ) <-
    judgmentContext
  let obligationCurr =
        totalObligation
          certState
          (utxoState ^. utxosGovStateL)

  -- TODO: remove this predicate check. See #4158
  obligationCurr
    == utxosDeposited utxoState
      ?! UnexpectedDepositPot obligationCurr (utxosDeposited utxoState)

  case mppNew of
    Just ppNew
      | toInteger (ppNew ^. ppMaxTxSizeL)
          + toInteger (ppNew ^. ppMaxBHSizeL)
          < toInteger (ppNew ^. ppMaxBBSizeL) ->
          pure $ NewppState ppNew $ updatePpup ppupState ppNew
    _ -> pure $ NewppState pp $ updatePpup ppupState pp

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  ( EraPParams era
  , GovState era ~ ShelleyGovState era
  , ProtVerAtMost era 8
  ) =>
  GovState era ->
  PParams era ->
  ShelleyGovState era
updatePpup ppupState pp =
  ppupState
    & proposalsL .~ ps
    & futureProposalsL .~ emptyPPPUpdates
  where
    ProposedPPUpdates newProposals = futureProposals ppupState
    ps =
      if all (hasLegalProtVerUpdate pp) newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
