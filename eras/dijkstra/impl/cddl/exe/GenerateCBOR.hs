module Main where

import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Test.Cardano.Ledger.Binary.Cuddle (generateCBORMain)

main :: IO ()
main = generateCBORMain dijkstraCDDL
