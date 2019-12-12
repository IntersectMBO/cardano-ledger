{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Rupd
  ( RUPD
  , RupdEnv(..)
  )
where

import           BaseTypes
import           BlockChain
import           EpochBoundary
import           LedgerState
import           Slot
import           Data.Functor ((<&>))
import           Control.State.Transition
import           Control.Monad.Trans.Reader (asks)

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
    deriving (Show, Eq)

  initialRules = [pure Nothing]
  transitionRules = [rupdTransition]

rupdTransition :: TransitionRule (RUPD crypto)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (epoch, slot) <- liftSTS $ do
    ei <- asks epochInfo
    e <- epochInfoEpoch ei s
    slot <- epochInfoFirst ei e <&> (+* startRewards)
    return (e, slot)
  if s <= slot
    then pure ru
    else case ru of
      Nothing -> Just <$> (liftSTS $ createRUpd epoch b es)
      Just _  -> pure ru
