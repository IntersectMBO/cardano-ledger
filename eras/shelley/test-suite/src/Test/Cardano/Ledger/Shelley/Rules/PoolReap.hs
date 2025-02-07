{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.PoolReap (
  tests,
)
where

import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.CertState (EraCertState (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  PState (..),
  esLStateL,
  lsCertStateL,
  nesEsL,
  psStakePoolParams,
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Protocol.TPraos.BHeader (
  bhbody,
  bheaderSlotNo,
 )
import Control.SetAlgebra (dom, eval, setSingleton, (∩), (⊆), (▷))
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
    poolState target = (chainNes target) ^. nesEsL . esLStateL . lsCertStateL . certPStateL

    removedAfterPoolreap_ :: SourceSignalTarget (CHAIN era) -> Property
    removedAfterPoolreap_ (SourceSignalTarget {source, target, signal = (UnserialisedBlock bh _)}) =
      let e = (epochFromSlotNo . bheaderSlotNo . bhbody) bh
       in removedAfterPoolreap (poolState source) (poolState target) e

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap ::
  forall era.
  PState era ->
  PState era ->
  EpochNo ->
  Property
removedAfterPoolreap p p' e =
  property $
    eval (retire ⊆ dom stp)
      && Set.null (eval (retire ∩ dom stp'))
      && Set.null (eval (retire ∩ dom retiring'))
  where
    stp = psStakePoolParams p
    stp' = psStakePoolParams p'
    retiring = psRetiring p
    retiring' = psRetiring p'
    retire :: Set.Set (KeyHash 'StakePool) -- This declaration needed to disambiguate 'eval'
    retire = eval (dom (retiring ▷ setSingleton e))

sameEpoch ::
  forall era.
  SourceSignalTarget (CHAIN era) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes
