module Main where

import Cardano.Ledger.Allegra.HuddleSpec (allegraCDDL)
import Paths_cardano_ledger_allegra (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec allegraCDDL =<< getDataFileName "cddl/data/allegra.cddl"
