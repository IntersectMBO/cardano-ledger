module Main where

import Cardano.Ledger.Shelley.HuddleSpec (shelleyCDDL)
import Paths_cardano_ledger_shelley (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  outputPath <- getDataFileName "cddl-files/shelley.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec shelleyCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
