{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Rupd
  ( RUPD,
    RupdEnv (..),
    PredicateFailure,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
  )
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    StrictMaybe (..),
    epochInfo,
    maxLovelaceSupply,
    randomnessStabilisationWindow,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.LedgerState (EpochState, RewardUpdate, createRUpd)
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    (+*),
  )
import Shelley.Spec.Ledger.Value

data RUPD crypto v

data RupdEnv crypto v
  = RupdEnv (BlocksMade crypto) (EpochState crypto v)

instance CV crypto v => STS (RUPD crypto v) where
  type State (RUPD crypto v) = StrictMaybe (RewardUpdate crypto)
  type Signal (RUPD crypto v) = SlotNo
  type Environment (RUPD crypto v) = RupdEnv crypto v
  type BaseM (RUPD crypto v) = ShelleyBase
  data PredicateFailure (RUPD crypto v) -- No predicate failures
    deriving (Show, Eq, Generic)

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

instance NoUnexpectedThunks (PredicateFailure (RUPD crypto v))

rupdTransition :: CV crypto v => TransitionRule (RUPD crypto v)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (epoch, slot, maxLL) <- liftSTS $ do
    ei <- asks epochInfo
    sr <- asks randomnessStabilisationWindow
    e <- epochInfoEpoch ei s
    slot <- epochInfoFirst ei e <&> (+* (Duration sr))
    maxLL <- asks maxLovelaceSupply
    return (e, slot, maxLL)
  if s <= slot
    then pure ru
    else case ru of
      SNothing -> SJust <$> (liftSTS $ createRUpd epoch b es (Coin $ fromIntegral maxLL))
      SJust _ -> pure ru
