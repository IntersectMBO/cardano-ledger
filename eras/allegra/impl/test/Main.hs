{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Allegra (Allegra)
import qualified Test.Cardano.Ledger.Allegra.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Allegra.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Allegra.ImpTest ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Allegra" $ do
      BinarySpec.spec
      CddlSpec.spec
      describe "Imp" $ do
        ShelleyImp.spec @Allegra
