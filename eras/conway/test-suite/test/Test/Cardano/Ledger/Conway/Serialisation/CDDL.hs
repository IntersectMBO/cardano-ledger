{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Serialisation.CDDL (
  tests,
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.Tx (Data)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage.TxBody (Datum)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Core
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_conway_test (getDataFileName)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils (
  cddlAnnotatorTest,
  cddlTest,
 )
import Test.Tasty (TestTree, testGroup, withResource)

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Value Conway) (eraProtVerHigh @Conway) n "coin"
    , cddlTest @(Value Conway) (eraProtVerHigh @Conway) n "value"
    , cddlAnnotatorTest @(TxBody Conway) (eraProtVerHigh @Conway) n "transaction_body"
    , cddlAnnotatorTest @(TxAuxData Conway) (eraProtVerHigh @Conway) n "auxiliary_data"
    , cddlAnnotatorTest @(Timelock Conway) (eraProtVerHigh @Conway) n "native_script"
    , cddlAnnotatorTest @(Data Conway) (eraProtVerHigh @Conway) n "plutus_data"
    , cddlTest @(TxOut Conway) (eraProtVerHigh @Conway) n "transaction_output"
    , cddlAnnotatorTest @(Script Conway) (eraProtVerHigh @Conway) n "script"
    , cddlTest @(Datum Conway) (eraProtVerHigh @Conway) n "datum_option"
    , cddlAnnotatorTest @(TxWits Conway) (eraProtVerHigh @Conway) n "transaction_witness_set"
    , cddlTest @(PParamsUpdate Conway) (eraProtVerHigh @Conway) n "protocol_param_update"
    , cddlTest @CostModels (eraProtVerHigh @Conway) n "costmdls"
    , cddlAnnotatorTest @(Redeemers Conway) (eraProtVerHigh @Conway) n "[* redeemer]"
    , cddlAnnotatorTest @(Tx Conway) (eraProtVerHigh @Conway) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- readDataFile "cddl-files/conway.cddl"
  crypto <- readDataFile "cddl-files/real/crypto.cddl"
  extras <- readDataFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile
