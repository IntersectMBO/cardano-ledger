{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Newpp
  ( NEWPP,
    NewppState (..),
    NewppEnv (..),
    NewppPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase, StrictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DState (..),
    PPUPState (..),
    PState (..),
    UTxOState,
    availableAfterMIR,
    pvCanFollow,
    _deposited,
    _irwd,
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
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Records
import NoThunks.Class (NoThunks (..))

data NEWPP era

data NewppState era
  = NewppState (Core.PParams era) (PPUPState era)

data NewppEnv era
  = NewppEnv
      (DState (Crypto era))
      (PState (Crypto era))
      (UTxOState era)
      AccountState

data NewppPredicateFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (NewppPredicateFailure era)

instance
  ( Default (Core.PParams era),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer),
    Typeable era
  ) =>
  STS (NEWPP era)
  where
  type State (NEWPP era) = NewppState era
  type Signal (NEWPP era) = Maybe (Core.PParams era)
  type Environment (NEWPP era) = NewppEnv era
  type BaseM (NEWPP era) = ShelleyBase
  type PredicateFailure (NEWPP era) = NewppPredicateFailure era
  transitionRules = [newPpTransition]

instance Default (Core.PParams era) => Default (NewppState era) where
  def = NewppState def def

newPpTransition ::
  forall era.
  ( HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (NEWPP era)
newPpTransition = do
  TRC
    ( NewppEnv dstate pstate utxoSt acnt,
      NewppState pp ppupSt,
      ppNew
      ) <-
    judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligation pp (_rewards dstate) (_pParams pstate)
          Coin oblgNew = obligation ppNew' (_rewards dstate) (_pParams pstate)
          diff = oblgCurr - oblgNew
          Coin availableReserves = availableAfterMIR ReservesMIR acnt (_irwd dstate)

      Coin oblgCurr == _deposited utxoSt
        ?! UnexpectedDepositPot (Coin oblgCurr) (_deposited utxoSt)

      if availableReserves + diff >= 0
        -- Note that instantaneous rewards from the treasury are irrelevant
        -- here, since changes in the protocol parameters do not change how much
        -- is needed from the treasury
        && (getField @"_maxTxSize" ppNew' + getField @"_maxBHSize" ppNew')
          < getField @"_maxBBSize" ppNew'
        then pure $ NewppState ppNew' (updatePpup ppupSt ppNew')
        else pure $ NewppState pp (updatePpup ppupSt pp)
    Nothing -> pure $ NewppState pp (updatePpup ppupSt pp)

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  ( HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
  ) =>
  PPUPState era ->
  Core.PParams era ->
  PPUPState era
updatePpup ppupSt pp = PPUPState ps emptyPPPUpdates
  where
    (ProposedPPUpdates newProposals) = futureProposals ppupSt
    goodPV =
      pvCanFollow (getField @"_protocolVersion" pp)
        . getField @"_protocolVersion"
    ps =
      if all goodPV newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
