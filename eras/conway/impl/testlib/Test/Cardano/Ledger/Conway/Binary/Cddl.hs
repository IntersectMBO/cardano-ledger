module Test.Cardano.Ledger.Conway.Binary.Cddl (
  readConwayCddlFileNames,
  readConwayCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_conway

readConwayCddlFileNames :: IO [FilePath]
readConwayCddlFileNames = do
  base <- getDataFileName "cddl-files/conway.cddl"
  pure [base]

readConwayCddlFiles :: IO [BSL.ByteString]
readConwayCddlFiles = mapM BSL.readFile =<< readConwayCddlFileNames
