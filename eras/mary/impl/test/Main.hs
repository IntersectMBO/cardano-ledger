{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Mary (Mary)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Mary.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Mary.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Mary.ImpTest ()
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Mary" $ do
      ValueSpec.spec
      BinarySpec.spec
      CddlSpec.spec
      describe "Imp" $ do
        ShelleyImp.spec @Mary
