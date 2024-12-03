module Main where

import Paths_cardano_ledger_alonzo
import Test.Cardano.Ledger.Alonzo.CDDL (alonzoCDDL)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec alonzoCDDL =<< getDataFileName "cddl-files/alonzo.cddl"
