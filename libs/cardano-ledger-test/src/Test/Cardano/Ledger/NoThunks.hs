{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.NoThunks (
  test,
) where

import Cardano.Ledger.Conway.Core (Era (..))
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
import Cardano.Ledger.Shelley.State
import NoThunks.Class (NoThunks)
import Test.Cardano.Ledger.Generic.GenState (EraGenericGen, GenSize, defaultGenSize)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, noThunksGen)
import Test.Cardano.Ledger.Generic.Proof (
  AllegraEra,
  AlonzoEra,
  BabbageEra,
  MaryEra,
  ShelleyEra,
 )
import Test.Cardano.Ledger.Generic.Trace (Gen1, traceProp)
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test :: TestTree
test =
  testGroup
    "There are no unexpected thunks in MockChainState"
    [ f @ShelleyEra
    , f @AllegraEra
    , f @MaryEra
    , f @AlonzoEra
    , f @BabbageEra
    ]
  where
    f ::
      forall era.
      ( HasTrace (MOCKCHAIN era) (Gen1 era)
      , EraGenericGen era
      , ShelleyEraAccounts era
      , NoThunks (StashedAVVMAddresses era)
      ) =>
      TestTree
    f = testThunks @era 100 defaultGenSize

testThunks ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , EraGenericGen era
  , NoThunks (StashedAVVMAddresses era)
  , ShelleyEraAccounts era
  ) =>
  Int ->
  GenSize ->
  TestTree
testThunks numTx gensize =
  testProperty (eraName @era ++ " era. Trace length = " ++ show numTx) $
    traceProp @era
      numTx
      gensize
      ( \_ !trc -> do
          nt <- noThunksGen trc
          case nt of
            Just x -> error $ "Thunks present: " <> show x
            Nothing -> return ()
      )
