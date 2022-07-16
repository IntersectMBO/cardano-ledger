{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL
  ( cddlTests,
  )
where

import Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary (MaryEra)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_shelley_ma_test
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

type A = AllegraEra StandardCrypto

type M = MaryEra StandardCrypto

cddlTests :: Int -> TestTree
cddlTests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value A) n "coin",
      cddlTest @(Core.Value M) n "value",
      cddlAnnotatorTest @(Core.TxBody M) n "transaction_body",
      cddlAnnotatorTest @(Core.TxBody A) n "transaction_body_allegra",
      cddlAnnotatorTest @(Core.Script M) n "native_script",
      cddlAnnotatorTest @(Core.Script A) n "native_script",
      cddlAnnotatorTest @(Core.AuxiliaryData M) n "auxiliary_data",
      cddlAnnotatorTest @(Core.AuxiliaryData A) n "auxiliary_data"
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
