module Main where

import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Paths_cardano_ledger_dijkstra (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/dijkstra.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec dijkstraCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
