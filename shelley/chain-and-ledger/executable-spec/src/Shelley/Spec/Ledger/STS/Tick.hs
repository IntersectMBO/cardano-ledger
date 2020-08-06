{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Tick
  ( TICK,
    TickEnv (..),
    State,
    PredicateFailure (..),
  )
where

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
import Shelley.Spec.Ledger.Value

data TICK crypto v

data TickEnv crypto
  = TickEnv (Set (KeyHash 'Genesis crypto))

instance
  CV crypto v =>
  STS (TICK crypto v)
  where
  type
    State (TICK crypto v) =
      NewEpochState crypto v
  type
    Signal (TICK crypto v) =
      SlotNo
  type Environment (TICK crypto v) = TickEnv crypto
  type BaseM (TICK crypto v) = ShelleyBase
  data PredicateFailure (TICK crypto v)
    = NewEpochFailure (PredicateFailure (NEWEPOCH crypto v)) -- Subtransition Failures
    | RupdFailure (PredicateFailure (RUPD crypto v)) -- Subtransition Failures
    deriving (Show, Generic, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

instance NoUnexpectedThunks (PredicateFailure (TICK crypto v))

adoptGenesisDelegs ::
  EpochState crypto v ->
  SlotNo ->
  EpochState crypto v
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
  forall crypto v.
  (CV crypto v) =>
  TransitionRule (TICK crypto v)
bheadTransition = do
  TRC (TickEnv gkeys, nes@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  epoch <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoEpoch ei slot

  nes' <-
    trans @(NEWEPOCH crypto v) $
      TRC (NewEpochEnv slot gkeys, nes, epoch)

  ru' <- trans @(RUPD crypto v) $ TRC (RupdEnv bprev es, nesRu nes', slot)

  let es' = adoptGenesisDelegs (nesEs nes') slot
      nes'' =
        nes'
          { nesRu = ru',
            nesEs = es'
          }
  pure nes''

instance
  CV crypto v =>
  Embed (NEWEPOCH crypto v) (TICK crypto v)
  where
  wrapFailed = NewEpochFailure

instance
  CV crypto v =>
  Embed (RUPD crypto v) (TICK crypto v)
  where
  wrapFailed = RupdFailure
