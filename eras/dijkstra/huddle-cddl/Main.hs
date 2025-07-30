module Main where

import Paths_cardano_ledger_dijkstra
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import Test.Cardano.Ledger.Dijkstra.CDDL (dijkstraCDDL)

main :: IO ()
main = writeSpec dijkstraCDDL =<< getDataFileName "cddl-files/dijkstra.cddl"
