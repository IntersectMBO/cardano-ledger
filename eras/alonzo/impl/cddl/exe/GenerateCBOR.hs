module Main where

import Cardano.Ledger.Alonzo.HuddleSpec (alonzoCDDL)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (generateCBORMain)

main :: IO ()
main = generateCBORMain alonzoCDDL
