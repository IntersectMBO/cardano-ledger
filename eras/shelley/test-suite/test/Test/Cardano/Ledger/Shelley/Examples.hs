{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Examples (
  CHAINExample (..),
  testCHAINExample,
)
where

import Cardano.Ledger.Block (Block)
import Cardano.Ledger.PoolDistr (individualTotalPoolStakeL, poolDistrDistrL, poolDistrTotalL)
import Cardano.Ledger.Shelley ()
import Cardano.Ledger.Shelley.LedgerState (nesPdL)
import Cardano.Ledger.Shelley.Scripts ()
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Control.State.Transition.Extended hiding (Assertion)
import Data.List.NonEmpty (NonEmpty)
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState, chainStateNesL, totalAda)
import Test.Cardano.Ledger.Shelley.TreeDiff (expectExprEqual)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, maxLLSupply, runShelleyBase)
import Test.Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Test.Tasty.HUnit (Assertion, (@?=))

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
testCHAINExample :: HasCallStack => CHAINExample (BHeader C_Crypto) C -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  ( checkTrace @(CHAIN C) runShelleyBase () $
      ( pure initSt .- block
          <&> chainStateNesL . nesPdL . poolDistrTotalL .~ mempty
          <&> chainStateNesL . nesPdL . poolDistrDistrL %~ (<&> individualTotalPoolStakeL .~ mempty)
      )
        .->> ( expectedSt
                & chainStateNesL . nesPdL . poolDistrTotalL .~ mempty
                & chainStateNesL . nesPdL . poolDistrDistrL %~ (<&> individualTotalPoolStakeL .~ mempty)
             )
    )
    >> expectExprEqual (totalAda expectedSt) maxLLSupply
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN C) (TRC ((), initSt, block))
  st @?= predicateFailure
