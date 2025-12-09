module Main where

import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Paths_cardano_ledger_conway (getDataFileName)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = writeSpec conwayCDDL =<< getDataFileName "cddl/data/conway.cddl"
