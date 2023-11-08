module Test.Cardano.Ledger.Conway.Binary.Cddl (
  readConwayCddlFileNames,
  readConwayCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_conway

readConwayCddlFileNames :: IO [FilePath]
readConwayCddlFileNames = do
  base <- getDataFileName "cddl-files/conway.cddl"
  crypto <- getDataFileName "cddl-files/crypto.cddl"
  extras <- getDataFileName "cddl-files/extra.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure [base, crypto, extras]

readConwayCddlFiles :: IO [BSL.ByteString]
readConwayCddlFiles = mapM BSL.readFile =<< readConwayCddlFileNames
