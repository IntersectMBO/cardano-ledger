module Test.Cardano.Ledger.Alonzo.Binary.Cddl (
  readAlonzoCddlFileNames,
  readAlonzoCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo

readAlonzoCddlFileNames :: IO [FilePath]
readAlonzoCddlFileNames = do
  base <- getDataFileName "cddl-files/alonzo.cddl"
  pure [base]

readAlonzoCddlFiles :: IO [BSL.ByteString]
readAlonzoCddlFiles = mapM BSL.readFile =<< readAlonzoCddlFileNames
