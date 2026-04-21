module Main where

import Cardano.Ledger.Mary.HuddleSpec (maryCDDL)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (generateCBORMain)

main :: IO ()
main = generateCBORMain maryCDDL
