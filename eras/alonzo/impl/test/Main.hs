{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo)
import Data.Data (Proxy (..))
import qualified Test.Cardano.Ledger.Alonzo.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Alonzo.ImpTest ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.ImpTestSpec as ImpTestSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Alonzo" $ do
      BinarySpec.spec
      ImpTestSpec.spec $ Proxy @Alonzo
