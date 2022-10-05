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
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.Era (ShelleyNEWPP)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DState (..),
    PPUPState (..),
    PState (..),
    UTxOState,
    availableAfterMIR,
    pvCanFollow,
    utxosDeposited,
    dsIrwd,
  )
import Cardano.Ledger.Shelley.PParams
  ( ProposedPPUpdates (..),
    emptyPPPUpdates,
  )
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    (?!),
  )
import Data.Default.Class (Default, def)
import Data.Typeable (Typeable)
import Data.UMap (rewView)
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
      AccountState

data ShelleyNewppPredFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyNewppPredFailure era)

instance
  ( Default (PParams era),
    HasField "sppKeyDeposit" (PParams era) Coin,
    HasField "sppPoolDeposit" (PParams era) Coin,
    HasField "sppProtocolVersion" (PParams era) ProtVer,
    HasField "sppMaxTxSize" (PParams era) Natural,
    HasField "sppMaxBHSize" (PParams era) Natural,
    HasField "sppMaxBBSize" (PParams era) Natural,
    HasField "sppProtocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer),
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
  ( HasField "sppKeyDeposit" (PParams era) Coin,
    HasField "sppPoolDeposit" (PParams era) Coin,
    HasField "sppProtocolVersion" (PParams era) ProtVer,
    HasField "sppMaxTxSize" (PParams era) Natural,
    HasField "sppMaxBHSize" (PParams era) Natural,
    HasField "sppMaxBBSize" (PParams era) Natural,
    HasField "sppProtocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (ShelleyNEWPP era)
newPpTransition = do
  TRC
    ( NewppEnv dstate pstate utxoSt acnt,
      NewppState pp ppupSt,
      ppNew
      ) <-
    judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligation pp (rewView (dsUnified dstate)) (psPParams pstate)
          Coin oblgNew = obligation ppNew' (rewView (dsUnified dstate)) (psPParams pstate)
          diff = oblgCurr - oblgNew
          Coin availableReserves = availableAfterMIR ReservesMIR acnt (dsIrwd dstate)

      Coin oblgCurr == utxosDeposited utxoSt
        ?! UnexpectedDepositPot (Coin oblgCurr) (utxosDeposited utxoSt)

      if availableReserves + diff >= 0
        -- Note that instantaneous rewards from the treasury are irrelevant
        -- here, since changes in the protocol parameters do not change how much
        -- is needed from the treasury
        && (getField @"sppMaxTxSize" ppNew' + getField @"sppMaxBHSize" ppNew')
          < getField @"sppMaxBBSize" ppNew'
        then pure $ NewppState ppNew' (updatePpup ppupSt ppNew')
        else pure $ NewppState pp (updatePpup ppupSt pp)
    Nothing -> pure $ NewppState pp (updatePpup ppupSt pp)

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  ( HasField "sppProtocolVersion" (PParams era) ProtVer,
    HasField "sppProtocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  PPUPState era ->
  PParams era ->
  PPUPState era
updatePpup ppupSt pp = PPUPState ps emptyPPPUpdates
  where
    ProposedPPUpdates newProposals = futureProposals ppupSt
    goodPV =
      pvCanFollow (getField @"sppProtocolVersion" pp)
        . getField @"sppProtocolVersion"
    ps =
      if all goodPV newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
