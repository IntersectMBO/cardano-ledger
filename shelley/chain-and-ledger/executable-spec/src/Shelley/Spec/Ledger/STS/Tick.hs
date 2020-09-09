{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Tick
  ( TICK,
    TickEnv (..),
    State,
    TickPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (eval, (⨃))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, epochInfo)
import Shelley.Spec.Ledger.Keys (GenDelegs (..), KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    LedgerState (..),
    NewEpochEnv (..),
    NewEpochState (..),
  )
import Shelley.Spec.Ledger.STS.NewEpoch (NEWEPOCH)
import Shelley.Spec.Ledger.STS.Rupd (RUPD, RupdEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo, epochInfoEpoch)

data TICK era

data TickEnv era
  = TickEnv (Set (KeyHash 'Genesis era))

data TickPredicateFailure era
  = NewEpochFailure (PredicateFailure (NEWEPOCH era)) -- Subtransition Failures
  | RupdFailure (PredicateFailure (RUPD era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance Show (TickPredicateFailure (Shelley c))

deriving stock instance Eq (TickPredicateFailure (Shelley c))

instance NoUnexpectedThunks (TickPredicateFailure (Shelley c))

instance
  Crypto c =>
  STS (TICK (Shelley c))
  where
  type
    State (TICK (Shelley c)) =
      NewEpochState (Shelley c)
  type
    Signal (TICK (Shelley c)) =
      SlotNo
  type Environment (TICK (Shelley c)) = TickEnv (Shelley c)
  type BaseM (TICK (Shelley c)) = ShelleyBase
  type PredicateFailure (TICK (Shelley c)) = TickPredicateFailure (Shelley c)

  initialRules = []
  transitionRules = [bheadTransition]

adoptGenesisDelegs ::
  EpochState era ->
  SlotNo ->
  EpochState era
adoptGenesisDelegs es slot = es'
  where
    ls = esLState es
    dp = _delegationState ls
    ds = _dstate dp
    fGenDelegs = _fGenDelegs ds
    GenDelegs genDelegs = _genDelegs ds
    (curr, fGenDelegs') = Map.partitionWithKey (\(FutureGenDeleg s _) _ -> s <= slot) fGenDelegs
    latestPerGKey (FutureGenDeleg s genKeyHash) delegate latest =
      case Map.lookup genKeyHash latest of
        Nothing -> Map.insert genKeyHash (s, delegate) latest
        Just (t, _) ->
          if s > t
            then Map.insert genKeyHash (s, delegate) latest
            else latest
    genDelegs' = Map.map snd $ Map.foldrWithKey latestPerGKey Map.empty curr
    ds' =
      ds
        { _fGenDelegs = fGenDelegs',
          _genDelegs = GenDelegs $ eval (genDelegs ⨃ genDelegs')
        }
    dp' = dp {_dstate = ds'}
    ls' = ls {_delegationState = dp'}
    es' = es {esLState = ls'}

bheadTransition ::
  forall era c.
  (Era era, era ~ Shelley c) =>
  TransitionRule (TICK era)
bheadTransition = do
  TRC (TickEnv gkeys, nes@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  epoch <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoEpoch ei slot

  nes' <-
    trans @(NEWEPOCH era) $
      TRC (NewEpochEnv slot gkeys, nes, epoch)

  ru'' <- trans @(RUPD era) $ TRC (RupdEnv bprev es, nesRu nes', slot)

  let es'' = adoptGenesisDelegs (nesEs nes') slot
      nes'' =
        nes'
          { nesRu = ru'',
            nesEs = es''
          }
  pure nes''

instance
  Crypto c =>
  Embed (NEWEPOCH (Shelley c)) (TICK (Shelley c))
  where
  wrapFailed = NewEpochFailure

instance
  Crypto c =>
  Embed (RUPD (Shelley c)) (TICK (Shelley c))
  where
  wrapFailed = RupdFailure
