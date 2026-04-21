module Main where

import Cardano.Ledger.Allegra.HuddleSpec (allegraCDDL)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (generateCBORMain)

main :: IO ()
main = generateCBORMain allegraCDDL
