{-# LANGUAGE BangPatterns #-}
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

import Cardano.Ledger.BaseTypes (Globals (quorum), ShelleyBase)
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
import Cardano.Ledger.Shelley.Rules.Ppup (votedFuturePParams)
import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  (?!),
 )
import Data.Default.Class (Default, def)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
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
  forM_ mppNew $ \_ ->
    obligationCurr
      == utxosDeposited utxoState
        ?! UnexpectedDepositPot obligationCurr (utxosDeposited utxoState)

  coreNodeQuorum <- liftSTS $ asks quorum
  case mppNew of
    Just ppNew
      | toInteger (ppNew ^. ppMaxTxSizeL)
          + toInteger (ppNew ^. ppMaxBHSizeL)
          < toInteger (ppNew ^. ppMaxBBSizeL) ->
          pure $ NewppState ppNew $ updatePpup coreNodeQuorum ppupState ppNew
    _ -> pure $ NewppState pp $ updatePpup coreNodeQuorum ppupState pp

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
  ShelleyGovState era
updatePpup !coreNodeQuorum ppupState pp =
  ppupState
    { sgsCurProposals = curProposals
    , sgsFutureProposals = emptyPPPUpdates
    , sgsFuturePParams = votedFuturePParams curProposals pp coreNodeQuorum
    }
  where
    ProposedPPUpdates newProposals = sgsFutureProposals ppupState
    curProposals =
      if all (hasLegalProtVerUpdate pp) newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
