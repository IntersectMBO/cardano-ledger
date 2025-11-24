module Main where

import Cardano.Ledger.Babbage.HuddleSpec (babbageCDDL)
import Paths_cardano_ledger_babbage (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/babbage.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec babbageCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
