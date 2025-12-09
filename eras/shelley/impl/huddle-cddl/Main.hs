module Main where

import Paths_cardano_ledger_shelley
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import Test.Cardano.Ledger.Shelley.CDDL (shelleyCDDL)

main :: IO ()
main = writeSpec shelleyCDDL =<< getDataFileName "cddl/data/shelley.cddl"
