module Main where

import Paths_cardano_ledger_mary
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import qualified Test.Cardano.Ledger.Mary.CDDL as Mary

-- Generate cddl files for all relevant specifications
main :: IO ()
main = do
  specFile <- getDataFileName "cddl-files/mary.cddl"
  writeSpec Mary.cddl specFile
