{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Rupd
  ( RUPD,
    RupdEnv (..),
    PredicateFailure,
    RupdPredicateFailure,
  )
where

import Cardano.Ledger.Era (Crypto, Era)
import Control.Monad.Trans.Reader (asks)
import Control.Provenance (runProvM)
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
  )
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
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
    epochInfoSize,
    (+*),
  )

data RUPD era

data RupdEnv era
  = RupdEnv (BlocksMade (Crypto era)) (EpochState era)

data RupdPredicateFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (RupdPredicateFailure era)

instance (Era era) => STS (RUPD era) where
  type State (RUPD era) = StrictMaybe (RewardUpdate (Crypto era))
  type Signal (RUPD era) = SlotNo
  type Environment (RUPD era) = RupdEnv era
  type BaseM (RUPD era) = ShelleyBase
  type PredicateFailure (RUPD era) = RupdPredicateFailure era

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

rupdTransition :: Era era => TransitionRule (RUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, maxLL) <- liftSTS $ do
    ei <- asks epochInfo
    sr <- asks randomnessStabilisationWindow
    e <- epochInfoEpoch ei s
    slotsPerEpoch <- epochInfoSize ei e
    slot <- epochInfoFirst ei e <&> (+* (Duration sr))
    maxLL <- asks maxLovelaceSupply
    return (slotsPerEpoch, slot, maxLL)
  if s <= slot
    then pure ru
    else case ru of
      SNothing ->
        SJust
          <$> ( liftSTS $
                  runProvM $
                    createRUpd
                      slotsPerEpoch
                      b
                      es
                      (Coin (fromIntegral maxLL))
              )
      SJust _ -> pure ru
