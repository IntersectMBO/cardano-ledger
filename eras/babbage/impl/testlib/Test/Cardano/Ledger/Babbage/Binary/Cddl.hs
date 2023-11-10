module Test.Cardano.Ledger.Babbage.Binary.Cddl (
  readBabbageCddlFileNames,
  readBabbageCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_babbage

readBabbageCddlFileNames :: IO [FilePath]
readBabbageCddlFileNames = do
  base <- getDataFileName "cddl-files/babbage.cddl"
  crypto <- getDataFileName "cddl-files/crypto.cddl"
  extras <- getDataFileName "cddl-files/extras.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure [base, crypto, extras]

readBabbageCddlFiles :: IO [BSL.ByteString]
readBabbageCddlFiles = mapM BSL.readFile =<< readBabbageCddlFileNames
