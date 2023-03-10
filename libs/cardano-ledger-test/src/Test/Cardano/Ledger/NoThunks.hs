{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.NoThunks (
  test,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Pretty (PrettyA)
import Control.State.Transition.Extended (STS)
import Data.Default.Class (def)
import Test.Cardano.Ledger.Generic.GenState (GenSize)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, noThunksGen)
import Test.Cardano.Ledger.Generic.Proof (Evidence (Mock), Proof (..), Reflect)
import Test.Cardano.Ledger.Generic.Trace (traceProp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test :: TestTree
test =
  testGroup
    "There are no unexpected thunks in MockChainState"
    [ f $ Babbage Mock
    , f $ Alonzo Mock
    , f $ Allegra Mock
    , f $ Mary Mock
    , f $ Shelley Mock
    ]
  where
    f proof = testThunks proof 100 def

testThunks ::
  forall era.
  ( Reflect era
  , STS (MOCKCHAIN era)
  , PrettyA (DCert era)
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

-- main :: IO ()
-- main = defaultMain test
