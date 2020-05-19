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
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DState (..),
    PState (..),
    UTxOState,
    _deposited,
    _irwd,
    _reserves,
    clearPpup,
    emptyAccount,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.UTxO

data NEWPP crypto

data NewppState crypto
  = NewppState (UTxOState crypto) AccountState PParams

data NewppEnv crypto
  = NewppEnv (Maybe PParams) (DState crypto) (PState crypto)

instance STS (NEWPP crypto) where
  type State (NEWPP crypto) = NewppState crypto
  type Signal (NEWPP crypto) = EpochNo
  type Environment (NEWPP crypto) = NewppEnv crypto
  type BaseM (NEWPP crypto) = ShelleyBase
  data PredicateFailure (NEWPP crypto)
    = UnexpectedDepositPot Coin Coin
    deriving (Show, Eq, Generic)

  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

instance NoUnexpectedThunks (PredicateFailure (NEWPP crypto))

initialNewPp :: InitialRule (NEWPP crypto)
initialNewPp =
  pure $
    NewppState
      (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPPUpdates)
      emptyAccount
      emptyPParams

newPpTransition :: TransitionRule (NEWPP crypto)
newPpTransition = do
  TRC (NewppEnv ppNew dstate pstate, NewppState utxoSt acnt pp, e) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      slot <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoFirst ei e
      let Coin oblgCurr = obligation pp (_stkCreds dstate) (_stPools pstate) slot
          Coin oblgNew = obligation ppNew' (_stkCreds dstate) (_stPools pstate) slot
          diff = oblgCurr - oblgNew
          Coin reserves = _reserves acnt
          Coin requiredInstantaneousRewards = foldl' (+) (Coin 0) $ _irwd dstate

      (Coin oblgCurr) == (_deposited utxoSt) ?! UnexpectedDepositPot (Coin oblgCurr) (_deposited utxoSt)

      if reserves + diff >= requiredInstantaneousRewards
        && (_maxTxSize ppNew' + _maxBHSize ppNew') < _maxBBSize ppNew'
        then
          let utxoSt' = utxoSt {_deposited = Coin oblgNew}
           in let acnt' = acnt {_reserves = Coin $ reserves + diff}
               in pure $ NewppState (clearPpup utxoSt') acnt' ppNew'
        else pure $ NewppState (clearPpup utxoSt) acnt pp
    Nothing -> pure $ NewppState (clearPpup utxoSt) acnt pp
