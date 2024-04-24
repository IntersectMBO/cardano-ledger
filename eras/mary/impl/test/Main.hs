{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Mary (Mary)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Mary.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Mary.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Mary.Imp as Imp
import Test.Cardano.Ledger.Mary.ImpTest ()
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Mary" $ do
      ValueSpec.spec
      BinarySpec.spec
      CddlSpec.spec
      describe "Imp" $ do
        Imp.spec @Mary
