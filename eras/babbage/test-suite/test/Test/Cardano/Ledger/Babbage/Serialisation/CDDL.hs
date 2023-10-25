{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.CDDL (
  tests,
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.Scripts.Data (Data)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.TxBody (Datum)
import Cardano.Ledger.Core
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Babbage.Binary.Cddl (readBabbageCddlFiles)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils (
  cddlAnnotatorTest,
  cddlTest,
 )
import Test.Tasty (TestTree, testGroup, withResource)

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Value Babbage) (eraProtVerHigh @Babbage) n "coin"
    , cddlAnnotatorTest @(TxBody Babbage) (eraProtVerHigh @Babbage) n "transaction_body"
    , cddlAnnotatorTest @(TxAuxData Babbage) (eraProtVerHigh @Babbage) n "auxiliary_data"
    , cddlAnnotatorTest @(Timelock Babbage) (eraProtVerHigh @Babbage) n "native_script"
    , cddlAnnotatorTest @(Data Babbage) (eraProtVerHigh @Babbage) n "plutus_data"
    , cddlTest @(TxOut Babbage) (eraProtVerHigh @Babbage) n "transaction_output"
    , cddlAnnotatorTest @(Script Babbage) (eraProtVerHigh @Babbage) n "script"
    , cddlTest @(Datum Babbage) (eraProtVerHigh @Babbage) n "datum_option"
    , cddlAnnotatorTest @(TxWits Babbage) (eraProtVerHigh @Babbage) n "transaction_witness_set"
    , cddlTest @(PParamsUpdate Babbage) (eraProtVerHigh @Babbage) n "protocol_param_update"
    , cddlTest @CostModels (eraProtVerHigh @Babbage) n "costmdls"
    , cddlAnnotatorTest @(Redeemers Babbage) (eraProtVerHigh @Babbage) n "[* redeemer]"
    , cddlAnnotatorTest @(Tx Babbage) (eraProtVerHigh @Babbage) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = mconcat <$> readBabbageCddlFiles
