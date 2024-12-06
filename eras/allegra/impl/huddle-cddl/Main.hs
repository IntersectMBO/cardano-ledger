module Main where

import Paths_cardano_ledger_allegra
import Test.Cardano.Ledger.Allegra.CDDL (allegraCDDL)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec allegraCDDL =<< getDataFileName "cddl-files/allegra.cddl"
