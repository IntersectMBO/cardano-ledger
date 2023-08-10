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
  DState (..),
  PState (..),
  UTxOState (utxosDeposited),
  totalObligation,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.PParams (
  ProposedPPUpdates (ProposedPPUpdates),
  emptyPPPUpdates,
  pvCanFollow,
 )
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

data NewppEnv era
  = NewppEnv
      (DState era)
      (PState era)
      (UTxOState era)

data ShelleyNewppPredFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyNewppPredFailure era)

instance
  ( EraGov era
  , GovState era ~ ShelleyGovState era
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
  ) =>
  TransitionRule (ShelleyNEWPP era)
newPpTransition = do
  TRC
    ( NewppEnv dstate pstate utxoSt
      , NewppState pp ppupSt
      , ppNew
      ) <-
    judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr =
            totalObligation
              (CertState def pstate dstate)
              (utxoSt ^. utxosGovStateL)
      Coin oblgCurr
        == utxosDeposited utxoSt
          ?! UnexpectedDepositPot (Coin oblgCurr) (utxosDeposited utxoSt)

      if (ppNew' ^. ppMaxTxSizeL + ppNew' ^. ppMaxBHSizeL) < (ppNew' ^. ppMaxBBSizeL)
        then pure $ NewppState ppNew' (updatePpup ppupSt ppNew')
        else pure $ NewppState pp (updatePpup ppupSt pp)
    Nothing -> pure $ NewppState pp (updatePpup ppupSt pp)

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  ( EraPParams era
  , GovState era ~ ShelleyGovState era
  ) =>
  GovState era ->
  PParams era ->
  ShelleyGovState era
updatePpup ppupSt pp =
  ppupSt
    & proposalsL .~ ps
    & futureProposalsL .~ emptyPPPUpdates
  where
    ProposedPPUpdates newProposals = futureProposals ppupSt
    goodPV ppu =
      pvCanFollow (pp ^. ppProtocolVersionL) $ ppu ^. ppuProtocolVersionL
    ps =
      if all goodPV newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
