module Main where

import Cardano.Ledger.Shelley.HuddleSpec (shelleyCDDL)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (generateCBORMain)

main :: IO ()
main = generateCBORMain shelleyCDDL
