module Main where

import Paths_cardano_ledger_conway
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)
import Test.Cardano.Ledger.Conway.CDDL (conwayCDDL)

main :: IO ()
main = writeSpec conwayCDDL =<< getDataFileName "cddl/data/conway.cddl"
