{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Examples.Chain (
  CHAINExample (..),
  testCHAINExample,
) where

import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses, nesPdL)
import Cardano.Ledger.State
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Control.State.Transition.Extended hiding (Assertion)
import Data.List.NonEmpty (NonEmpty)
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState, chainStateNesL, totalAda)
import Test.Cardano.Ledger.Shelley.TreeDiff (expectExprEqual)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, maxLLSupply, runShelleyBase)
import Test.Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Test.Tasty.HUnit (Assertion, (@?=))

data CHAINExample era = CHAINExample
  { startState :: ChainState era
  -- ^ State to start testing with
  , newBlock :: Block (BHeader MockCrypto) era
  -- ^ Block to run chain state transition system on
  , intendedResult :: Either (NonEmpty (PredicateFailure (CHAIN era))) (ChainState era)
  -- ^ type of fatal error, if failure expected and final chain state if success expected
  }

deriving instance
  ( EraGov era
  , EraTxOut era
  , Show (BlockBody era)
  , Show (CertState era)
  , Show (InstantStake era)
  , Show (StashedAVVMAddresses era)
  , Show (PredicateFailure (EraRule "BBODY" era))
  , Show (PredicateFailure (EraRule "TICK" era))
  , Show (PredicateFailure (EraRule "TICKN" era))
  ) =>
  Show (CHAINExample era)

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: HasCallStack => CHAINExample ShelleyEra -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  ( checkTrace @(CHAIN ShelleyEra) runShelleyBase () $
      ( pure initSt .- block
          <&> chainStateNesL . nesPdL . poolDistrTotalL .~ knownNonZeroCoin @1
          <&> chainStateNesL . nesPdL . poolDistrDistrL %~ (<&> individualTotalPoolStakeL .~ mempty)
      )
        .->> ( expectedSt
                 & chainStateNesL . nesPdL . poolDistrTotalL .~ knownNonZeroCoin @1
                 & chainStateNesL . nesPdL . poolDistrDistrL %~ (<&> individualTotalPoolStakeL .~ mempty)
             )
    )
    >> expectExprEqual (totalAda expectedSt) maxLLSupply
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN ShelleyEra) (TRC ((), initSt, block))
  st @?= predicateFailure
