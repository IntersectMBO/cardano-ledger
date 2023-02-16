{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Examples (
  CHAINExample (..),
  testCHAINExample,
)
where

import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Shelley ()
import Cardano.Ledger.Shelley.Scripts ()
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Test.Cardano.Ledger.Binary.TreeDiff (expectExprEqual)
import Test.Cardano.Ledger.Shelley.Rules.Chain (totalAda)
import Test.Cardano.Protocol.TPraos.ConcreteCryptoTypes (C, C_Crypto)
import Test.Cardano.Protocol.TPraos.Rules (CHAIN, ChainState)
import Test.Cardano.Protocol.TPraos.Utils (applySTSTest, maxLLSupply, runShelleyBase)
import Test.Tasty.HUnit (Assertion, (@?=))

data CHAINExample h era = CHAINExample
  { startState :: ChainState era
  -- ^ State to start testing with
  , newBlock :: Block h era
  -- ^ Block to run chain state transition system on
  , intendedResult :: Either [PredicateFailure (CHAIN era)] (ChainState era)
  -- ^ type of fatal error, if failure expected and final chain state if success expected
  }

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample (BHeader C_Crypto) C -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  (checkTrace @(CHAIN C) runShelleyBase () $ pure initSt .- block .->> expectedSt)
    >> expectExprEqual (totalAda expectedSt) maxLLSupply
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN C) (TRC ((), initSt, block))
  st @?= predicateFailure
