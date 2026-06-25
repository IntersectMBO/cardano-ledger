module Main where

import Cardano.Ledger.Shelley.HuddleSpec (shelleyCDDL)
import Paths_cardano_ledger_shelley (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec shelleyCDDL =<< getDataFileName "cddl/data/shelley.cddl"
