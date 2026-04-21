module Main where

import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Test.Cardano.Ledger.Binary.Cuddle (generateCBORMain)

main :: IO ()
main = generateCBORMain conwayCDDL
