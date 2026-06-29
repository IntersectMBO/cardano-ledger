{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.PoolReap (
  tests,
) where

import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Protocol.TPraos.BHeader (
  bhbody,
  bheaderSlotNo,
 )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  chainSstWithTick,
  forAllChainTrace,
  traceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  epochFromSlotNo,
 )
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

tests ::
  forall era.
  ( EraGen era
  , EraStake era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  TestTree
tests =
  testProperty "pool is removed from stake pool and retiring maps" $
    forAllChainTrace traceLen defaultConstants $ \tr ->
      conjoin $
        map removedAfterPoolreap_ $
          filter (not . sameEpoch) (chainSstWithTick tr)
  where
    stakePools target = (chainNes target) ^. nesEsL . esLStateL . lsCertStateL . certPStateL

    removedAfterPoolreap_ :: SourceSignalTarget (CHAIN era) -> Property
    removedAfterPoolreap_ (SourceSignalTarget {source, target, signal = (Block bh _)}) =
      let e = (epochFromSlotNo . bheaderSlotNo . bhbody) bh
       in removedAfterPoolreap (stakePools source) (stakePools target) e

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap ::
  forall era.
  PState era ->
  PState era ->
  EpochNo ->
  Property
removedAfterPoolreap p p' epoch =
  property $
    (retire `Set.isSubsetOf` (Map.keysSet stp))
      && Set.null (Set.intersection retire (Map.keysSet stp'))
      && Set.null (Set.intersection retire (Map.keysSet retiring'))
  where
    stp = psStakePools p
    stp' = psStakePools p'
    retiring = psRetiring p
    retiring' = psRetiring p'
    retire = Map.keysSet $ Map.filter (epoch ==) retiring

sameEpoch ::
  forall era.
  SourceSignalTarget (CHAIN era) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes
