module Test.Cardano.Ledger.Allegra.Binary.Cddl (
  readAllegraCddlFileNames,
  readAllegraCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_allegra

readAllegraCddlFileNames :: IO [FilePath]
readAllegraCddlFileNames = do
  base <- getDataFileName "cddl-files/allegra.cddl"
  crypto <- getDataFileName "cddl-files/real/crypto.cddl"
  extras <- getDataFileName "cddl-files/mock/extras.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure [base, crypto, extras]

readAllegraCddlFiles :: IO [BSL.ByteString]
readAllegraCddlFiles = mapM BSL.readFile =<< readAllegraCddlFileNames
