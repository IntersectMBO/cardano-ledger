module Main where

import Cardano.Ledger.Alonzo.HuddleSpec (alonzoCDDL)
import Paths_cardano_ledger_alonzo (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/alonzo.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec alonzoCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
