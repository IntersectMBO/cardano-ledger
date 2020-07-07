{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Newpp
  ( NEWPP,
    NewppState (..),
    NewppEnv (..),
    PredicateFailure (..),
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
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
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (obligation)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DState (..),
    PState (..),
    UTxOState,
    emptyAccount,
    emptyPPUPState,
    totalInstantaneousReservesRewards,
    updatePpup,
    _deposited,
    _irwd,
    _reserves,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPParams)
import Shelley.Spec.Ledger.UTxO (UTxO (..))

data NEWPP crypto

data NewppState crypto
  = NewppState (UTxOState crypto) AccountState PParams

data NewppEnv crypto
  = NewppEnv (DState crypto) (PState crypto)

instance Typeable crypto => STS (NEWPP crypto) where
  type State (NEWPP crypto) = NewppState crypto
  type Signal (NEWPP crypto) = Maybe PParams
  type Environment (NEWPP crypto) = NewppEnv crypto
  type BaseM (NEWPP crypto) = ShelleyBase
  data PredicateFailure (NEWPP crypto)
    = UnexpectedDepositPot
        !Coin -- The total outstanding deposits
        !Coin -- The deposit pot
    deriving (Show, Eq, Generic)

  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

instance NoUnexpectedThunks (PredicateFailure (NEWPP crypto))

initialNewPp :: InitialRule (NEWPP crypto)
initialNewPp =
  pure $
    NewppState
      (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPUPState)
      emptyAccount
      emptyPParams

newPpTransition :: TransitionRule (NEWPP crypto)
newPpTransition = do
  TRC (NewppEnv dstate pstate, NewppState utxoSt acnt pp, ppNew) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligation pp (_stkCreds dstate) (_stPools pstate)
          Coin oblgNew = obligation ppNew' (_stkCreds dstate) (_stPools pstate)
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
