module Main where

import Cardano.Ledger.Shelley.CDDL (shelleyCDDL)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  let outputPath = "eras/shelley/auto-generated/shelley.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec shelleyCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
