{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.CDDL
  ( tests,
  )
where

import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.TxBody (Datum)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_babbage_test
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Value Babbage) n "coin",
      cddlAnnotatorTest @(TxBody Babbage) n "transaction_body",
      cddlAnnotatorTest @(TxAuxData Babbage) n "auxiliary_data",
      cddlAnnotatorTest @(MA.Timelock Babbage) n "native_script",
      cddlAnnotatorTest @(Data Babbage) n "plutus_data",
      cddlTest @(TxOut Babbage) n "transaction_output",
      cddlAnnotatorTest @(Script Babbage) n "script",
      cddlTest @(Datum Babbage) n "datum_option",
      cddlAnnotatorTest @(TxWits Babbage) n "transaction_witness_set",
      cddlTest @(PParamsUpdate Babbage) n "protocol_param_update",
      cddlTest @CostModels n "costmdls",
      cddlAnnotatorTest @(Redeemers Babbage) n "[* redeemer]",
      cddlAnnotatorTest @(Tx Babbage) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- readDataFile "cddl-files/babbage.cddl"
  crypto <- readDataFile "cddl-files/real/crypto.cddl"
  extras <- readDataFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile
