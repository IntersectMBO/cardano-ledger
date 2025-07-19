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
) where

import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules.Reports (synopsisCoinMap)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Val ((<+>))
import Lens.Micro ((^.))
import qualified Prettyprinter as Pretty
import Test.Cardano.Ledger.Binary.TreeDiff (ansiDocToString)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Rules.TestChain (shortChainTrace)
import Test.Control.State.Transition.Trace (SourceSignalTarget (..))
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
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
  , EraStake era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  TestTree
tests =
  testGroup
    "Deposit Invariants"
    [ testProperty "Non negative deposits" (shortChainTrace defaultConstants (nonNegativeDeposits @era))
    , testProperty
        "Deposits = KeyDeposits + PoolDeposits"
        (shortChainTrace defaultConstants (depositInvariant @era))
    ]

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      UTxOState {utxosDeposited = d} = (lsUTxOState . esLState) es
   in counterexample ("nonNegativeDeposits: " ++ show d) (d >= mempty)

-- | Check that the sum of key Deposits (in the UMap) and the pool Depoits (in psDeposits) are equal to the utxosDeposits
depositInvariant ::
  EraCertState era =>
  SourceSignalTarget (CHAIN era) ->
  Property
depositInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsUTxOState = utxost, lsCertState = certState} = esLState . nesEs $ chainNes chainSt
      dstate = certState ^. certDStateL
      pstate = certState ^. certPStateL
      allDeposits = utxosDeposited utxost
      keyDeposits = sumDepositsAccounts (dstate ^. accountsL)
      poolDeposits = foldMap fromCompact (psDeposits pstate)
   in counterexample
        ( ansiDocToString . Pretty.vsep $
            [ "Deposit invariant fails:"
            , Pretty.indent 2 . Pretty.vsep . map Pretty.pretty $
                [ "All deposits = " ++ show allDeposits
                , "Key deposits = "
                    ++ show ((^. depositAccountStateL) <$> (dstate ^. accountsL . accountsMapL))
                , "Pool deposits = " ++ synopsisCoinMap (Just (fromCompact <$> psDeposits pstate))
                ]
            ]
        )
        (allDeposits === keyDeposits <+> poolDeposits)
