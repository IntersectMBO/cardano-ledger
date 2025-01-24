{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.Deposits (
  tests,
)
where

import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  shortChainTrace,
 )

import Cardano.Ledger.CertState (EraCertState (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules.Reports (
  synopsisCoinMap,
 )
import Cardano.Ledger.UMap (depositMap)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<+>))
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import qualified Prettyprinter as Pretty
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC

import Test.Cardano.Ledger.Binary.TreeDiff (ansiDocToString, diffExpr)
import Test.QuickCheck (
  Property,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- | Tests that redundant Deposit information is consistent
tests ::
  forall era.
  ( EraGen era
  , EraGov era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraCertState era
  ) =>
  TestTree
tests =
  testGroup
    "Deposit Invariants"
    [ testProperty "Non negative deposits" (shortChainTrace defaultConstants (nonNegativeDeposits @era))
    , testProperty
        "Deposits = KeyDeposits + PoolDeposits"
        (shortChainTrace defaultConstants (depositInvariant @era))
    , testProperty
        "Reward domain = Deposit domain"
        (shortChainTrace defaultConstants (rewardDepositDomainInvariant @era))
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
  EraCertState era =>
  SourceSignalTarget (CHAIN era) ->
  Property
depositInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsUTxOState = utxost, lsCertState = certState} = (esLState . nesEs . chainNes) chainSt
      dstate = certState ^. certDStateL
      pstate = certState ^. certPStateL
      allDeposits = utxosDeposited utxost
      sumCoin = Map.foldl' (<+>) (Coin 0)
      keyDeposits = (UM.fromCompact . UM.sumDepositUView . UM.RewDepUView . dsUnified) dstate
      poolDeposits = sumCoin (psDeposits pstate)
   in counterexample
        ( ansiDocToString . Pretty.vsep $
            [ "Deposit invariant fails:"
            , Pretty.indent 2 . Pretty.vsep . map Pretty.pretty $
                [ "All deposits = " ++ show allDeposits
                , "Key deposits = " ++ synopsisCoinMap (Just (depositMap (dsUnified dstate)))
                , "Pool deposits = " ++ synopsisCoinMap (Just (psDeposits pstate))
                ]
            ]
        )
        (allDeposits === keyDeposits <+> poolDeposits)

rewardDepositDomainInvariant ::
  EraCertState era =>
  SourceSignalTarget (CHAIN era) ->
  Property
rewardDepositDomainInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsCertState = certState} = (esLState . nesEs . chainNes) chainSt
      dstate = certState ^. certDStateL
      rewardDomain = UM.domain (UM.RewDepUView (dsUnified dstate))
      depositDomain = Map.keysSet (depositMap (dsUnified dstate))
   in counterexample
        ( ansiDocToString . Pretty.vsep $
            [ "Reward-Deposit domain invariant fails:"
            , Pretty.indent 2 $ diffExpr rewardDomain depositDomain
            ]
        )
        (rewardDomain === depositDomain)
