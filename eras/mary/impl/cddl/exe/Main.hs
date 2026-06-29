module Main where

import Cardano.Ledger.Mary.HuddleSpec (maryCDDL)
import Paths_cardano_ledger_mary (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec maryCDDL =<< getDataFileName "cddl/data/mary.cddl"
