module Main where

import Cardano.Ledger.Mary.HuddleSpec (maryCDDL)
import Test.Cardano.Ledger.Binary.Cuddle (generateCBORMain)

main :: IO ()
main = generateCBORMain maryCDDL
