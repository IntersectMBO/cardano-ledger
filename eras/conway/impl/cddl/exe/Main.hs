module Main where

import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Paths_cardano_ledger_conway (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/conway.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec conwayCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
