module Main where

import Cardano.Ledger.Babbage.HuddleSpec (babbageCDDL)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (generateCBORMain)

main :: IO ()
main = generateCBORMain babbageCDDL
