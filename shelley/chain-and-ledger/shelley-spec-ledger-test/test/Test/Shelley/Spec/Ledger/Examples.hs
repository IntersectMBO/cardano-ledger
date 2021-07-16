{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample (..),
    testCHAINExample,
  )
where

import Cardano.Ledger.Shelley ()
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Shelley.Spec.Ledger.BlockChain (Block)
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainState, totalAda)
import Shelley.Spec.Ledger.Scripts ()
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, maxLLSupply, runShelleyBase)
import Test.Tasty.HUnit (Assertion, (@?=))

data CHAINExample h = CHAINExample
  { -- | State to start testing with
    startState :: ChainState h,
    -- | Block to run chain state transition system on
    newBlock :: Block h,
    -- | type of fatal error, if failure expected and final chain state if success expected
    intendedResult :: Either [PredicateFailure (CHAIN h)] (ChainState h)
  }

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample C -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  (checkTrace @(CHAIN C) runShelleyBase () $ pure initSt .- block .-> expectedSt)
    >> (totalAda expectedSt @?= maxLLSupply)
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN C) (TRC ((), initSt, block))
  st @?= predicateFailure
