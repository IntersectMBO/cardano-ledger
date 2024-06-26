{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babel.TxInfo where

import Data.Proxy (Proxy (..))
import Test.Tasty (TestTree, testGroup)

txInfoTests ::
  forall era.
  Proxy era ->
  TestTree
txInfoTests _p = testGroup "Babel Plutus Tests" [] -- Disabled for now
-- B.txInfoTestsV2 p PlutusV3 -- The V2 tests in Babbage should all hold for V3
