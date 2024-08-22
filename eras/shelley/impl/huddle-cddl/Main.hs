module Main where

import Paths_cardano_ledger_shelley
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import qualified Test.Cardano.Ledger.Shelley.CDDL as Shelley

-- Generate cddl files for all relevant specifications
main :: IO ()
main = do
  specFile <- getDataFileName "cddl-files/shelley.cddl"
  writeSpec Shelley.shelley specFile
