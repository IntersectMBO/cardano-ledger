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

import Cardano.Ledger.Era (Era)
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

instance
  Era era =>
  STS (TICK era)
  where
  type
    State (TICK era) =
      NewEpochState era
  type
    Signal (TICK era) =
      SlotNo
  type Environment (TICK era) = TickEnv era
  type BaseM (TICK era) = ShelleyBase
  data PredicateFailure (TICK era)
    = NewEpochFailure (PredicateFailure (NEWEPOCH era)) -- Subtransition Failures
    | RupdFailure (PredicateFailure (RUPD era)) -- Subtransition Failures
    deriving (Show, Generic, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

instance NoUnexpectedThunks (PredicateFailure (TICK era))

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
  forall era.
  (Era era) =>
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
  Era era =>
  Embed (NEWEPOCH era) (TICK era)
  where
  wrapFailed = NewEpochFailure

instance
  Era era =>
  Embed (RUPD era) (TICK era)
  where
  wrapFailed = RupdFailure
