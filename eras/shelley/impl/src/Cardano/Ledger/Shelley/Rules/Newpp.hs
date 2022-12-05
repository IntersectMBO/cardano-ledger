{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Newpp
  ( ShelleyNEWPP,
    ShelleyNewppState (..),
    NewppEnv (..),
    ShelleyNewppPredFailure (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase, StrictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyNEWPP)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    PPUPState (..),
    PState (..),
    UTxOState (utxosDeposited),
    obligationDPState,
    pvCanFollow,
  )
import Cardano.Ledger.Shelley.PParams
  ( ProposedPPUpdates (..),
    emptyPPPUpdates,
  )
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Cardano.Ledger.UMapCompact (rewView)
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    (?!),
  )
import Data.Default.Class (Default, def)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))

data ShelleyNewppState era
  = NewppState (PParams era) (PPUPState era)

data NewppEnv era
  = NewppEnv
      (DState (EraCrypto era))
      (PState (EraCrypto era))
      (UTxOState era)

data ShelleyNewppPredFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyNewppPredFailure era)

instance
  ( Default (PParams era),
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxBHSize" (PParams era) Natural,
    HasField "_maxBBSize" (PParams era) Natural,
    HasField "_protocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer),
    Typeable era
  ) =>
  STS (ShelleyNEWPP era)
  where
  type State (ShelleyNEWPP era) = ShelleyNewppState era
  type Signal (ShelleyNEWPP era) = Maybe (PParams era)
  type Environment (ShelleyNEWPP era) = NewppEnv era
  type BaseM (ShelleyNEWPP era) = ShelleyBase
  type PredicateFailure (ShelleyNEWPP era) = ShelleyNewppPredFailure era
  transitionRules = [newPpTransition]

instance Default (PParams era) => Default (ShelleyNewppState era) where
  def = NewppState def def

newPpTransition ::
  forall era.
  ( HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxBHSize" (PParams era) Natural,
    HasField "_maxBBSize" (PParams era) Natural,
    HasField "_protocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (ShelleyNEWPP era)
newPpTransition = do
  TRC
    ( NewppEnv dstate pstate utxoSt,
      NewppState pp ppupSt,
      ppNew
      ) <-
    judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligationDPState (DPState dstate pstate)
      Coin oblgCurr
        == utxosDeposited utxoSt
        ?! UnexpectedDepositPot (Coin oblgCurr) (utxosDeposited utxoSt)

      if (getField @"_maxTxSize" ppNew' + getField @"_maxBHSize" ppNew')
        < getField @"_maxBBSize" ppNew'
        then pure $ NewppState ppNew' (updatePpup ppupSt ppNew')
        else pure $ NewppState pp (updatePpup ppupSt pp)
    Nothing -> pure $ NewppState pp (updatePpup ppupSt pp)

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  ( HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  PPUPState era ->
  PParams era ->
  PPUPState era
updatePpup ppupSt pp = PPUPState ps emptyPPPUpdates
  where
    ProposedPPUpdates newProposals = futureProposals ppupSt
    goodPV =
      pvCanFollow (getField @"_protocolVersion" pp)
        . getField @"_protocolVersion"
    ps =
      if all goodPV newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
