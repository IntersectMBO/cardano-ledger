{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Tick
  ( TICK
  , TickEnv (..)
  , PredicateFailure (..)
  , State
  )
where

import           Data.Set (Set)
import           BaseTypes
import           Keys
import           LedgerState
import           Slot
import           STS.NewEpoch
import           STS.Rupd
import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition
import           Control.Monad.Trans.Reader (asks)

data TICK crypto

data TickEnv crypto
  = TickEnv (Set (GenKeyHash crypto))

instance Crypto crypto
  => STS (TICK crypto)
 where
  type State (TICK crypto)
    = NewEpochState crypto
  type Signal (TICK crypto)
    = SlotNo
  type Environment (TICK crypto) = TickEnv crypto
  type BaseM (TICK crypto) = ShelleyBase
  data PredicateFailure (TICK crypto)
    = NewEpochFailure (PredicateFailure (NEWEPOCH crypto))
    | RupdFailure (PredicateFailure (RUPD crypto))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

bheadTransition
  :: forall crypto
   . ( Crypto crypto)
  => TransitionRule (TICK crypto)
bheadTransition = do
  TRC (TickEnv gkeys, nes@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  epoch <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoEpoch ei slot

  nes' <- trans @(NEWEPOCH crypto)
    $ TRC (NewEpochEnv slot gkeys, nes, epoch)

  ru' <- trans @(RUPD crypto) $ TRC (RupdEnv bprev es, nesRu nes', slot)
  let nes'' = nes' { nesRu = ru' }
  pure nes''

instance Crypto crypto
  => Embed (NEWEPOCH crypto) (TICK crypto)
 where
  wrapFailed = NewEpochFailure

instance Crypto crypto
  => Embed (RUPD crypto) (TICK crypto)
 where
  wrapFailed = RupdFailure
