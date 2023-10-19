{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Shelley (Shelley)
import Data.Data (Proxy (..))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Shelley.ImpTestSpec as ImpTestSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Shelley" $ do
      Binary.spec
      ImpTestSpec.spec $ Proxy @Shelley
