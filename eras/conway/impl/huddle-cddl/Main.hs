module Main where

import Paths_cardano_ledger_conway
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import qualified Test.Cardano.Ledger.Conway.CDDL as Conway

-- Generate cddl files for all relevant specifications
main :: IO ()
main = do
  specFile <- getDataFileName "cddl-files/conway.cddl"
  writeSpec Conway.conway specFile
