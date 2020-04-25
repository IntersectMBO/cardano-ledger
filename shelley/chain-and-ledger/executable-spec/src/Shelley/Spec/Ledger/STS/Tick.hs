{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Tick
  ( TICK
  , TickEnv (..)
  , State
  , PredicateFailure (..)
  )
where

import           Byron.Spec.Ledger.Core ((◁), (⨃))
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.BaseTypes (ShelleyBase, epochInfo)
import           Shelley.Spec.Ledger.Crypto (Crypto)
import           Shelley.Spec.Ledger.Keys (GenDelegs (..), GenKeyHash)
import           Shelley.Spec.Ledger.LedgerState (DPState (..), DState (..), EpochState (..),
                     FutureGenDeleg (..), LedgerState (..), NewEpochEnv (..), NewEpochState (..))
import           Shelley.Spec.Ledger.Slot (SlotNo, epochInfoEpoch)
import           Shelley.Spec.Ledger.STS.NewEpoch (NEWEPOCH)
import           Shelley.Spec.Ledger.STS.Rupd (RUPD, RupdEnv (..))

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
    deriving (Show, Generic, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

instance NoUnexpectedThunks (PredicateFailure (TICK crypto))

adoptGenesisDelegs
  :: EpochState crypto
  -> SlotNo
  -> EpochState crypto
adoptGenesisDelegs es slot = es'
  where
    ls = esLState es
    dp = _delegationState ls
    ds = _dstate dp

    fGenDelegs = _fGenDelegs ds
    GenDelegs genDelegs = _genDelegs ds
    (curr, fGenDelegs') = Map.partitionWithKey (\(FutureGenDeleg s _) _ -> s <= slot) fGenDelegs
    curr' = Map.mapKeys (\(FutureGenDeleg s g) -> (s, g)) curr
    maxSlotNo = maximum . Set.map fGenDelegSlot . Map.keysSet
    latestPerGKey gk =
      ( (maxSlotNo . Map.filterWithKey (\(FutureGenDeleg _ c) _ -> c == gk)) curr
      , gk)
    genDelegsKeys = Set.map
                latestPerGKey
                (Set.map fGenDelegGenKeyHash (Map.keysSet curr))
    genDelegs' = Map.mapKeys snd $ genDelegsKeys ◁ curr'

    ds' = ds { _fGenDelegs = fGenDelegs'
             , _genDelegs = GenDelegs $ genDelegs ⨃ Map.toList genDelegs'
             }
    dp' = dp {_dstate = ds'}
    ls' = ls {_delegationState = dp'}
    es' = es {esLState = ls'}

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

  let es' = adoptGenesisDelegs (nesEs nes') slot
      nes'' = nes' { nesRu = ru'
                   , nesEs = es'
                   }
  pure nes''

instance Crypto crypto
  => Embed (NEWEPOCH crypto) (TICK crypto)
 where
  wrapFailed = NewEpochFailure

instance Crypto crypto
  => Embed (RUPD crypto) (TICK crypto)
 where
  wrapFailed = RupdFailure
