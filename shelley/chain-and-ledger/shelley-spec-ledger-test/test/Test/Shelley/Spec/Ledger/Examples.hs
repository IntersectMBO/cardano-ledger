{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample (..),
    testCHAINExample,
  )
where

import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Shelley.Spec.Ledger.BlockChain (Block)
import Shelley.Spec.Ledger.Coin(Coin)
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainState, totalAda)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase, maxLLSupply, applySTSTest)
import Test.Tasty.HUnit ((@?=), Assertion)

data CHAINExample h v = CHAINExample
  { -- | State to start testing with
    startState :: ChainState h v,
    -- | Block to run chain state transition system on
    newBlock :: Block h v,
    -- | type of fatal error, if failure expected and final chain state if success expected
    intendedResult :: Either [[PredicateFailure (CHAIN h v)]] (ChainState h v)
  }

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample C Coin -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  (checkTrace @(CHAIN C Coin) runShelleyBase () $ pure initSt .- block .-> expectedSt)
    >> (totalAda expectedSt @?= maxLLSupply)
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN C Coin) (TRC ((), initSt, block))
  st @?= predicateFailure