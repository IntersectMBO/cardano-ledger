module Test.Cardano.Ledger.Mary.Binary.Cddl (
  readMaryCddlFileNames,
  readMaryCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_mary

readMaryCddlFileNames :: IO [FilePath]
readMaryCddlFileNames = do
  base <- getDataFileName "cddl-files/mary.cddl"
  crypto <- getDataFileName "cddl-files/real/crypto.cddl"
  extras <- getDataFileName "cddl-files/mock/extras.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure [base, crypto, extras]

readMaryCddlFiles :: IO [BSL.ByteString]
readMaryCddlFiles = mapM BSL.readFile =<< readMaryCddlFileNames
