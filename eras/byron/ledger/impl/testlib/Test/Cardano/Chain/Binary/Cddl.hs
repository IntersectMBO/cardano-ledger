{-# LANGUAGE ImplicitPrelude #-}

module Test.Cardano.Chain.Binary.Cddl (
  readByronCddlFileNames,
  readByronCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_byron

readByronCddlFileNames :: IO [FilePath]
readByronCddlFileNames = do
  base <- getDataFileName "cddl-spec/byron.cddl"
  pure [base]

readByronCddlFiles :: IO [BSL.ByteString]
readByronCddlFiles = mapM BSL.readFile =<< readByronCddlFileNames
