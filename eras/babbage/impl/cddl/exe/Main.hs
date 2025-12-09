module Main where

import Cardano.Ledger.Babbage.HuddleSpec (babbageCDDL)
import Paths_cardano_ledger_babbage (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec babbageCDDL =<< getDataFileName "cddl/data/babbage.cddl"
