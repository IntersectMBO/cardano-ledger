{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.CDDL (
  tests,
  readDataFile,
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.Scripts.Data (Data)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers)
import Cardano.Ledger.Core
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo_test
import Test.Cardano.Ledger.Alonzo.Binary.Cddl (readAlonzoCddlFiles)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils (
  cddlAnnotatorTest,
  cddlTest,
 )
import Test.Tasty (TestTree, testGroup, withResource)

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Value Alonzo) (eraProtVerHigh @Alonzo) n "coin"
    , cddlAnnotatorTest @(TxBody Alonzo) (eraProtVerHigh @Alonzo) n "transaction_body"
    , cddlAnnotatorTest @(TxAuxData Alonzo) (eraProtVerHigh @Alonzo) n "auxiliary_data"
    , cddlAnnotatorTest @(Timelock Alonzo) (eraProtVerHigh @Alonzo) n "native_script"
    , cddlAnnotatorTest @(Data Alonzo) (eraProtVerHigh @Alonzo) n "plutus_data"
    , cddlTest @(TxOut Alonzo) (eraProtVerHigh @Alonzo) n "transaction_output"
    , cddlAnnotatorTest @(AlonzoTxWits Alonzo) (eraProtVerHigh @Alonzo) n "transaction_witness_set"
    , cddlTest @(PParamsUpdate Alonzo) (eraProtVerHigh @Alonzo) n "protocol_param_update"
    , cddlAnnotatorTest @(Redeemers Alonzo) (eraProtVerHigh @Alonzo) n "[* redeemer]"
    , cddlAnnotatorTest @(Tx Alonzo) (eraProtVerHigh @Alonzo) n "transaction"
    , cddlTest @CostModels (eraProtVerHigh @Alonzo) n "costmdls"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = mconcat <$> readAlonzoCddlFiles

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile
