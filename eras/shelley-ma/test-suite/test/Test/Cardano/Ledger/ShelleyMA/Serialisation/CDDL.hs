{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL (
  cddlTests,
)
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary)
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import Test.Cardano.Ledger.Mary.Binary.Cddl (readMaryCddlFiles)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils (
  cddlAnnotatorTest,
  cddlTest,
 )
import Test.Tasty (TestTree, testGroup, withResource)

cddlTests :: Int -> TestTree
cddlTests n =
  testGroup
    "CDDL roundtrip tests"
    [ withResource combinedAllegraCDDL (const (pure ())) $ \cddl ->
        testGroup "Allegra" $
          [ cddlTest @(Value Allegra) (eraProtVerHigh @Allegra) n "coin"
          , cddlAnnotatorTest @(TxBody Allegra) (eraProtVerLow @Allegra) n "transaction_body"
          , cddlAnnotatorTest @(Script Allegra) (eraProtVerLow @Allegra) n "native_script"
          , cddlAnnotatorTest @(TxAuxData Allegra) (eraProtVerLow @Allegra) n "auxiliary_data"
          ]
            <*> pure cddl
    , withResource combinedMaryCDDL (const (pure ())) $ \cddl ->
        testGroup "Mary" $
          [ cddlTest @(Value Mary) (eraProtVerHigh @Mary) n "value"
          , cddlAnnotatorTest @(TxBody Mary) (eraProtVerLow @Mary) n "transaction_body"
          , cddlAnnotatorTest @(Script Mary) (eraProtVerLow @Mary) n "native_script"
          , cddlAnnotatorTest @(TxAuxData Mary) (eraProtVerLow @Mary) n "auxiliary_data"
          ]
            <*> pure cddl
    ]

combinedAllegraCDDL :: IO BSL.ByteString
combinedAllegraCDDL = mconcat <$> readAllegraCddlFiles

combinedMaryCDDL :: IO BSL.ByteString
combinedMaryCDDL = mconcat <$> readMaryCddlFiles
