module Test.Cardano.Ledger.Babbage.Binary.Cddl (
  readBabbageCddlFileNames,
  readBabbageCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_babbage

readBabbageCddlFileNames :: IO [FilePath]
readBabbageCddlFileNames = do
  base <- getDataFileName "cddl-files/babbage.cddl"
  pure [base]

readBabbageCddlFiles :: IO [BSL.ByteString]
readBabbageCddlFiles = mapM BSL.readFile =<< readBabbageCddlFileNames
