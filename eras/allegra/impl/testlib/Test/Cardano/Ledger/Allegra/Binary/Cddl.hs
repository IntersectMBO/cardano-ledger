module Test.Cardano.Ledger.Allegra.Binary.Cddl (
  readAllegraCddlFileNames,
  readAllegraCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_allegra

readAllegraCddlFileNames :: IO [FilePath]
readAllegraCddlFileNames = do
  base <- getDataFileName "cddl-files/allegra.cddl"
  pure [base]

readAllegraCddlFiles :: IO [BSL.ByteString]
readAllegraCddlFiles = mapM BSL.readFile =<< readAllegraCddlFileNames
