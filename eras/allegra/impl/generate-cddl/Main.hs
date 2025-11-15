module Main where

import Cardano.Ledger.Allegra.CDDL (allegraCDDL)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Cardano.Ledger.Binary.Cuddle (writeSpec)

main :: IO ()
main = do
  let outputPath = "eras/allegra/auto-generated/allegra.cddl"
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeSpec allegraCDDL outputPath
  putStrLn $ "Generated CDDL file at: " ++ outputPath
