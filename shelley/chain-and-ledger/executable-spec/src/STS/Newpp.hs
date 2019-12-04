{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Newpp
  ( NEWPP
  , NewppState (..)
  , NewppEnv (..)
  )
where

import qualified Data.Map.Strict as Map
import           BaseTypes
import           Lens.Micro ((^.))

import           Coin
import           EpochBoundary
import           LedgerState (AccountState, DState, PState, UTxOState, pattern UTxOState, clearPpup,
                     emptyAccount, stkCreds, stPools, _deposited, _irwd, _reserves)
import           PParams
import           Slot
import           Updates
import           UTxO
import           Control.Monad.Trans.Reader (asks)

import           Control.State.Transition

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
    = FailureNEWPP
    deriving (Show, Eq)

  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: InitialRule (NEWPP crypto)
initialNewPp = pure $ NewppState
  (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)
  emptyAccount
  emptyPParams

newPpTransition :: TransitionRule (NEWPP crypto)
newPpTransition = do
  TRC (NewppEnv ppNew ds ps, NewppState utxoSt acnt pp, e) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      slot_ <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoFirst ei e
      let Coin oblgCurr = obligation pp (ds ^. stkCreds) (ps ^. stPools) slot_
          Coin oblgNew = obligation ppNew' (ds ^. stkCreds) (ps ^. stPools) slot_
          diff = oblgCurr - oblgNew
          Coin reserves = _reserves acnt
          Coin requiredInstantaneousRewards = foldl (+) (Coin 0) $ _irwd ds

      if reserves + diff >= requiredInstantaneousRewards
         && (_maxTxSize ppNew' + _maxBHSize ppNew') <  _maxBBSize ppNew'
        then
          let utxoSt' = utxoSt { _deposited = Coin oblgNew }
          in  -- TODO: update mechanism
              let acnt' = acnt { _reserves = Coin $ reserves + diff }
              in pure $ NewppState (clearPpup utxoSt') acnt' ppNew'
        else
          pure $ NewppState (clearPpup utxoSt) acnt pp
    Nothing -> pure $ NewppState (clearPpup utxoSt) acnt pp
