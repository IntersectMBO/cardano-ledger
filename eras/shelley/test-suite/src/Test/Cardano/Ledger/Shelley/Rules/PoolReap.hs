{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Shelley.Rules.PoolReap (
  tests,
)
where

import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  chainSstWithTick,
  forAllChainTrace,
  traceLen,
 )

import Cardano.Ledger.Block (
  Block (..),
 )
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  psStakePoolParams,
 )
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Protocol.TPraos.BHeader (
  bhbody,
  bheaderSlotNo,
 )
import Control.SetAlgebra (dom, eval, setSingleton, (∩), (⊆), (▷))
import Control.State.Transition.Trace (
  SourceSignalTarget (..),
 )
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import qualified Data.Set as Set
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  epochFromSlotNo,
 )
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
  forall era hc.
  ( ChainProperty era hc
  , EraGen era
  , EraGovernance era
  , QC.HasTrace (CHAIN era hc) (GenEnv era hc)
  ) =>
  TestTree
tests =
  testProperty "pool is removed from stake pool and retiring maps" $
    forAllChainTrace traceLen defaultConstants $ \tr ->
      conjoin $
        map removedAfterPoolreap_ $
          filter (not . sameEpoch) (chainSstWithTick tr)
  where
    poolState = certPState . lsCertState . esLState . nesEs . chainNes

    removedAfterPoolreap_ :: SourceSignalTarget (CHAIN era hc) -> Property
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
    retire :: Set.Set (KeyHash 'StakePool (EraCrypto era)) -- This declaration needed to disambiguate 'eval'
    retire = eval (dom (retiring ▷ setSingleton e))

sameEpoch ::
  forall era hc.
  SourceSignalTarget (CHAIN era hc) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes
