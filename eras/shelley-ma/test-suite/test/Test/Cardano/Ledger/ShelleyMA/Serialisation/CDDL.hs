{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL
  ( cddlTests,
  )
where

import Cardano.Ledger.Allegra (Allegra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (Mary)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_shelley_ma_test (getDataFileName)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

cddlTests :: Int -> TestTree
cddlTests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value Allegra) n "coin",
      cddlTest @(Core.Value Mary) n "value",
      cddlAnnotatorTest @(Core.TxBody Mary) n "transaction_body",
      cddlAnnotatorTest @(Core.TxBody Allegra) n "transaction_body_allegra",
      cddlAnnotatorTest @(Core.Script Mary) n "native_script",
      cddlAnnotatorTest @(Core.Script Allegra) n "native_script",
      cddlAnnotatorTest @(Core.AuxiliaryData Mary) n "auxiliary_data",
      cddlAnnotatorTest @(Core.AuxiliaryData Allegra) n "auxiliary_data"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- readDataFile "cddl-files/shelley-ma.cddl"
  crypto <- readDataFile "cddl-files/real/crypto.cddl"
  extras <- readDataFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile
