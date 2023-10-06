{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (Babbage)
import Data.Data (Proxy (..))
import qualified Test.Cardano.Ledger.Babbage.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Babbage.ImpTest ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.ImpTestSpec as ImpTestSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Babbage" $ do
      BinarySpec.spec
      ImpTestSpec.spec $ Proxy @Babbage
