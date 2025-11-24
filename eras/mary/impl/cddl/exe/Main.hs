module Main where

import Cardano.Ledger.Mary.HuddleSpec (maryCDDL)
import Paths_cardano_ledger_mary (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/mary.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec maryCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
