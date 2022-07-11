{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module Test.Cardano.Ledger.NoThunks
  (
  )
where

import Data.Default.Class (def)
import Test.Cardano.Ledger.Generic.GenState (GenSize)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, noThunksGen)
import Test.Cardano.Ledger.Generic.Proof (Evidence (Mock), Proof (Babbage), Reflect)
import Test.Cardano.Ledger.Generic.Trace (traceProp)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Control.State.Transition.Extended (STS)
import qualified Cardano.Ledger.Babbage.PParams

test ::
  forall era.
  ( Reflect era,
    STS (MOCKCHAIN era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  TestTree
test proof numTx gensize =
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

main :: IO ()
main = defaultMain $ test (Babbage Mock) 200 def
