{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Mary (Mary)
import Data.Data (Proxy (..))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Mary.BinarySpec as BinarySpec
import Test.Cardano.Ledger.Mary.ImpTest ()
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec
import qualified Test.Cardano.Ledger.Shelley.ImpTestSpec as ImpTestSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Mary" $ do
      ValueSpec.spec
      BinarySpec.spec
      ImpTestSpec.spec $ Proxy @Mary
