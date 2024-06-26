{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babel.Examples (
  CHAINExample (..),
  testCHAINExample,
)
where

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Era (BabelBBODY)
import Cardano.Ledger.Block (Block)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Control.State.Transition.Extended hiding (Assertion)
import Data.List.NonEmpty (NonEmpty)
import Test.Cardano.Ledger.Babel.Rules.Chain (
  CHAIN,
  ChainEvent (BbodyEvent),
  ChainState,
  TestChainPredicateFailure (BbodyFailure),
  totalAda,
 )
import Test.Cardano.Ledger.Babel.TreeDiff (expectExprEqual)
import Test.Cardano.Ledger.Babel.Utils (applySTSTest, maxLLSupply, runShelleyBase)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (StandardCrypto, TestCrypto)
import Test.Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Test.Tasty.HUnit (Assertion, (@?=))

type A = BabelEra TestCrypto

instance Embed (BabelBBODY A) (CHAIN A) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

data CHAINExample h era = CHAINExample
  { startState :: ChainState era
  -- ^ State to start testing with
  , newBlock :: Block h era
  -- ^ Block to run chain state transition system on
  , intendedResult :: Either (NonEmpty (PredicateFailure (CHAIN era))) (ChainState era)
  -- ^ type of fatal error, if failure expected and final chain state if success expected
  }

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample (BHeader StandardCrypto) (BabelEra StandardCrypto) -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  ( checkTrace @(CHAIN (BabelEra StandardCrypto))
      runShelleyBase
      ()
      (pure initSt .- block .->> expectedSt)
    )
    >> expectExprEqual (totalAda expectedSt) maxLLSupply
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN (BabelEra StandardCrypto)) (TRC ((), initSt, block))
  st @?= predicateFailure