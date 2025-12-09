module Main where

import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Paths_cardano_ledger_dijkstra (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec dijkstraCDDL =<< getDataFileName "cddl/data/dijkstra.cddl"
