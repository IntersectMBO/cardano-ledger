module Test.Cardano.Ledger.Alonzo.Binary.Cddl (
  readAlonzoCddlFileNames,
  readAlonzoCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo

readAlonzoCddlFileNames :: IO [FilePath]
readAlonzoCddlFileNames = do
  base <- getDataFileName "cddl-files/alonzo.cddl"
  crypto <- getDataFileName "cddl-files/crypto.cddl"
  extras <- getDataFileName "cddl-files/extras.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure [base, crypto, extras]

readAlonzoCddlFiles :: IO [BSL.ByteString]
readAlonzoCddlFiles = mapM BSL.readFile =<< readAlonzoCddlFileNames
