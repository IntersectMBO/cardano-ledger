{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.NoThunks (
  test,
) where

import Data.Default (def)
import Test.Cardano.Ledger.Generic.GenState (GenSize)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, noThunksGen)
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect)
import Test.Cardano.Ledger.Generic.Trace (Gen1, traceProp)
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test :: TestTree
test =
  testGroup
    "There are no unexpected thunks in MockChainState"
    [ f Shelley
    , f Allegra
    , f Mary
    , f Alonzo
    , f Babbage
    , f Conway
    ]
  where
    f proof = testThunks proof 100 def

testThunks ::
  forall era.
  ( Reflect era
  , HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  TestTree
testThunks proof numTx gensize =
  testProperty (show proof ++ " era. Trace length = " ++ show numTx) $
    traceProp
      proof
      numTx
      gensize
      ( \_ !trc -> do
          nt <- noThunksGen proof trc
          case nt of
            Just x -> error $ "Thunks present: " <> show x
            Nothing -> return ()
      )
