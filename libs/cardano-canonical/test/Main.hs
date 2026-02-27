{-# LANGUAGE DataKinds #-}

module Main (
  main,
) where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.SCLS.Spec

main :: IO ()
main = ledgerTestMain $ do
  Test.Cardano.Ledger.SCLS.Spec.spec
