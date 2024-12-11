module Main where

import Paths_cardano_ledger_babbage
import Test.Cardano.Ledger.Babbage.CDDL (babbageCDDL)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec babbageCDDL =<< getDataFileName "cddl-files/babbage.cddl"
