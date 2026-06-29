module Main where

import Cardano.Ledger.Alonzo.HuddleSpec (alonzoCDDL)
import Paths_cardano_ledger_alonzo (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec alonzoCDDL =<< getDataFileName "cddl/data/alonzo.cddl"
