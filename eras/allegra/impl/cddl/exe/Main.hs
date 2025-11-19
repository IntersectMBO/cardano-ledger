module Main where

import Cardano.Ledger.Allegra.HuddleSpec (allegraCDDL)
import Paths_cardano_ledger_allegra (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/allegra.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec allegraCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
