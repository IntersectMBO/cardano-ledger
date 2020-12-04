{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Newpp
  ( NEWPP,
    NewppState (..),
    NewppEnv (..),
    NewppPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Control.State.Transition
  ( InitialRule,
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    (?!),
  )
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (obligation)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DState (..),
    PState (..),
    UTxOState,
    emptyAccount,
    totalInstantaneousReservesRewards,
    updatePpup,
    _deposited,
    _irwd,
    _reserves,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPParams)
import Shelley.Spec.Ledger.UTxO (UTxO (..))

data NEWPP era

data NewppState era
  = NewppState (UTxOState era) AccountState (PParams era)

data NewppEnv era
  = NewppEnv (DState (Crypto era)) (PState (Crypto era))

data NewppPredicateFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (NewppPredicateFailure era)

instance (Typeable era, ShelleyBased era) => STS (NEWPP era) where
  type State (NEWPP era) = NewppState era
  type Signal (NEWPP era) = Maybe (PParams era)
  type Environment (NEWPP era) = NewppEnv era
  type BaseM (NEWPP era) = ShelleyBase
  type PredicateFailure (NEWPP era) = NewppPredicateFailure era
  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: forall era. ShelleyBased era => InitialRule (NEWPP era)
initialNewPp =
  pure $
    NewppState
      (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) (initialUpdateState @era))
      emptyAccount
      emptyPParams

newPpTransition ::
  Core.HasUpdateLogic era => TransitionRule (NEWPP era)
newPpTransition = do
  TRC (NewppEnv dstate pstate, NewppState utxoSt acnt pp, ppNew) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligation pp (_rewards dstate) (_pParams pstate)
          Coin oblgNew = obligation ppNew' (_rewards dstate) (_pParams pstate)
          diff = oblgCurr - oblgNew
          Coin reserves = _reserves acnt
          Coin requiredInstantaneousRewards = totalInstantaneousReservesRewards (_irwd dstate)

      (Coin oblgCurr) == (_deposited utxoSt) ?! UnexpectedDepositPot (Coin oblgCurr) (_deposited utxoSt)

      if reserves + diff >= requiredInstantaneousRewards
        -- Note that instantaneous rewards from the treasury are irrelevant here,
        -- since changes in the protocol parameters do not change how much is needed from the treasury
        && (_maxTxSize ppNew' + _maxBHSize ppNew') < _maxBBSize ppNew'
        then
          let utxoSt' = utxoSt {_deposited = Coin oblgNew}
           in let acnt' = acnt {_reserves = Coin $ reserves + diff}
               in pure $ NewppState (updatePpup utxoSt' ppNew') acnt' ppNew'
        else pure $ NewppState (updatePpup utxoSt pp) acnt pp
    Nothing -> pure $ NewppState (updatePpup utxoSt pp) acnt pp
