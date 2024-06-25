module Test.Cardano.Ledger.Babel.Binary.Cddl (
  readBabelCddlFileNames,
  readBabelCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_babel

readBabelCddlFileNames :: IO [FilePath]
readBabelCddlFileNames = do
  base <- getDataFileName "cddl-files/babel.cddl"
  crypto <- getDataFileName "cddl-files/crypto.cddl"
  extras <- getDataFileName "cddl-files/extra.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure [base, crypto, extras]

readBabelCddlFiles :: IO [BSL.ByteString]
readBabelCddlFiles = mapM BSL.readFile =<< readBabelCddlFileNames
