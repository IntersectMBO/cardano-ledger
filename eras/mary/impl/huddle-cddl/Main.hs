module Main where

import Paths_cardano_ledger_mary
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import Test.Cardano.Ledger.Mary.CDDL (maryCDDL)

main :: IO ()
main = writeSpec maryCDDL =<< getDataFileName "cddl-files/mary.cddl"
