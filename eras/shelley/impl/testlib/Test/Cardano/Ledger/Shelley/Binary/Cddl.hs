module Test.Cardano.Ledger.Shelley.Binary.Cddl (
  readShelleyCddlFileNames,
  readShelleyCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_shelley

readShelleyCddlFileNames :: IO [FilePath]
readShelleyCddlFileNames = do
  base <- getDataFileName "cddl/data/shelley.cddl"
  pure [base]

readShelleyCddlFiles :: IO [BSL.ByteString]
readShelleyCddlFiles = mapM BSL.readFile =<< readShelleyCddlFileNames
