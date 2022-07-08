{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.NoThunks
  (
  )
where

import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace)
import Data.Default.Class (def)
import NoThunks.Class (NoThunks, unsafeNoThunks)
import Test.Cardano.Ledger.Generic.GenState (GenSize)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockChainState)
import Test.Cardano.Ledger.Generic.Proof (Evidence (Mock), Proof (Babbage), Reflect)
import Test.Cardano.Ledger.Generic.Trace (Gen1, traceProp)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.QuickCheck (testProperty)

test ::
  forall era.
  ( Reflect era,
    HasTrace (MOCKCHAIN era) (Gen1 era),
    NoThunks (MockChainState era)
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
      ( \_ trc -> case unsafeNoThunks trc of
          Just x -> error $ "Thunks present: " <> show x
          Nothing -> ()
      )

main :: IO ()
main = defaultMain $ test (Babbage Mock) 200 def
