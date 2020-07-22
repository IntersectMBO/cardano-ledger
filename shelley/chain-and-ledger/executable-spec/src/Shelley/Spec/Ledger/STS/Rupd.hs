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
import Data.Typeable (Typeable)
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

data RUPD crypto

data RupdEnv crypto
  = RupdEnv (BlocksMade crypto) (EpochState crypto)

instance Typeable crypto => STS (RUPD crypto) where
  type State (RUPD crypto) = StrictMaybe (RewardUpdate crypto)
  type Signal (RUPD crypto) = SlotNo
  type Environment (RUPD crypto) = RupdEnv crypto
  type BaseM (RUPD crypto) = ShelleyBase
  data PredicateFailure (RUPD crypto) -- No predicate failures
    deriving (Show, Eq, Generic)

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

instance NoUnexpectedThunks (PredicateFailure (RUPD crypto))

rupdTransition :: Typeable crypto => TransitionRule (RUPD crypto)
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
