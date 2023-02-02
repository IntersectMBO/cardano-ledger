{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.Deposits (
  depositsProps,
)
where

import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  -- Helper Functions
  -- Helper Constraints
  shortChainTrace,
 )

import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley.Core (EraTallyState (..))
import Cardano.Ledger.Shelley.LedgerState (
  DPState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PPUPState,
  PState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules.Reports (
  synopsisCoinMap,
 )
import Cardano.Ledger.TreeDiff (diffExpr)
import Cardano.Ledger.UMapCompact (depositView)
import qualified Cardano.Ledger.UMapCompact as UM
import Cardano.Ledger.Val ((<+>))
import Control.State.Transition.Trace (
  SourceSignalTarget (..),
 )
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Default.Class (Default)
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))

import Test.QuickCheck (
  Property,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

-- | Tests that redundant Deposit information is consistent
depositsProps ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , Default (PPUPState era)
  , EraTallyState era
  ) =>
  TestTree
depositsProps =
  testGroup
    "Deposit Invariants"
    [ TQC.testProperty "Non negative deposits" (shortChainTrace (nonNegativeDeposits @era))
    , TQC.testProperty "Deposits = KeyDeposits + PoolDeposits" (shortChainTrace (depositInvariant @era))
    , TQC.testProperty "Reward domain = Deposit domain" (shortChainTrace (rewardDepositDomainInvariant @era))
    ]

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      UTxOState {utxosDeposited = d} = (lsUTxOState . esLState) es
   in counterexample ("nonNegativeDeposits: " ++ show d) (d >= mempty)

-- | Check that the sum of key Deposits (in the UMap) and the pool Depoits (in psDeposits) are equal to the utsosDeposits
depositInvariant ::
  SourceSignalTarget (CHAIN era) ->
  Property
depositInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsUTxOState = utxost, lsDPState = DPState dstate pstate} = (esLState . nesEs . chainNes) chainSt
      allDeposits = utxosDeposited utxost
      sumCoin m = Map.foldl' (<+>) (Coin 0) m
      keyDeposits = (UM.fromCompact . UM.sumDepositView . UM.RewardDeposits . dsUnified) dstate
      poolDeposits = sumCoin (psDeposits pstate)
   in counterexample
        ( unlines
            [ "Deposit invariant fails"
            , "All deposits = " ++ show allDeposits
            , "Key deposits = " ++ synopsisCoinMap (Just (depositView (dsUnified dstate)))
            , "Pool deposits = " ++ synopsisCoinMap (Just (psDeposits pstate))
            ]
        )
        (allDeposits === keyDeposits <+> poolDeposits)

rewardDepositDomainInvariant ::
  SourceSignalTarget (CHAIN era) ->
  Property
rewardDepositDomainInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsDPState = DPState dstate _} = (esLState . nesEs . chainNes) chainSt
      rewardDomain = UM.domain (UM.RewardDeposits (dsUnified dstate))
      depositDomain = Map.keysSet (depositView (dsUnified dstate))
   in counterexample
        ( unlines
            [ "Reward-Deposit domain invariant fails"
            , diffExpr rewardDomain depositDomain
            ]
        )
        (rewardDomain === depositDomain)
