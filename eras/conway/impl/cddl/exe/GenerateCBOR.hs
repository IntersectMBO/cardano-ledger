module Main where

import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (generateCBORMain)

main :: IO ()
main = generateCBORMain conwayCDDL
