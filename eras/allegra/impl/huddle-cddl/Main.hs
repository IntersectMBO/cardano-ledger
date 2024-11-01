module Main where

import Paths_cardano_ledger_allegra
import qualified Test.Cardano.Ledger.Allegra.CDDL as Allegra
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

-- Generate cddl files for all relevant specifications
main :: IO ()
main = do
  specFile <- getDataFileName "cddl-files/allegra.cddl"
  writeSpec Allegra.cddl specFile
