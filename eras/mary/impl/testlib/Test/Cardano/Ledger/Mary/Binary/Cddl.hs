module Test.Cardano.Ledger.Mary.Binary.Cddl (
  readMaryCddlFileNames,
  readMaryCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_mary

readMaryCddlFileNames :: IO [FilePath]
readMaryCddlFileNames = do
  base <- getDataFileName "cddl/data/mary.cddl"
  pure [base]

readMaryCddlFiles :: IO [BSL.ByteString]
readMaryCddlFiles = mapM BSL.readFile =<< readMaryCddlFileNames
