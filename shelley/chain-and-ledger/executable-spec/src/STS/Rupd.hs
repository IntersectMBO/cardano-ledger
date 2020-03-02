{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Rupd
  ( RUPD
  , RupdEnv(..)
  )
where

import           BaseTypes (ShelleyBase, epochInfo, maxLovelaceSupply, startRewards)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin (..))
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition (STS (..), TRC (..), TransitionRule, judgmentContext,
                     liftSTS)
import           Data.Functor ((<&>))
import           EpochBoundary (BlocksMade)
import           GHC.Generics (Generic)
import           LedgerState (EpochState, RewardUpdate, createRUpd)
import           Slot (Duration (..), SlotNo, epochInfoEpoch, epochInfoFirst, (+*))

data RUPD crypto

data RupdEnv crypto
  = RupdEnv (BlocksMade crypto) (EpochState crypto)

instance STS (RUPD crypto) where
  type State (RUPD crypto) = Maybe (RewardUpdate crypto)
  type Signal (RUPD crypto) = Slot.SlotNo
  type Environment (RUPD crypto) = RupdEnv crypto
  type BaseM (RUPD crypto) = ShelleyBase
  data PredicateFailure (RUPD crypto)
    = FailureRUPD
    deriving (Show, Eq, Generic)

  initialRules = [pure Nothing]
  transitionRules = [rupdTransition]

instance NoUnexpectedThunks (PredicateFailure (RUPD crypto))

rupdTransition :: TransitionRule (RUPD crypto)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (epoch, slot, maxLL) <- liftSTS $ do
    ei <- asks epochInfo
    sr <- asks startRewards
    e <- epochInfoEpoch ei s
    slot <- epochInfoFirst ei e <&> (+* (Duration sr))
    maxLL <- asks maxLovelaceSupply
    return (e, slot, maxLL)
  if s <= slot
    then pure ru
    else case ru of
      Nothing -> Just <$> (liftSTS $ createRUpd epoch b es (Coin $ fromIntegral maxLL))
      Just _  -> pure ru
