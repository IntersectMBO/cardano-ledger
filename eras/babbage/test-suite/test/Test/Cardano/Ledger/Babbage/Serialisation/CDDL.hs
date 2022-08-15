{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.CDDL
  ( tests,
  )
where

import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWitness (Redeemers)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.TxBody (Datum)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_babbage_test
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

type B = BabbageEra StandardCrypto

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Value B) n "coin",
      cddlAnnotatorTest @(TxBody B) n "transaction_body",
      cddlAnnotatorTest @(AuxiliaryData B) n "auxiliary_data",
      cddlAnnotatorTest @(MA.Timelock StandardCrypto) n "native_script",
      cddlAnnotatorTest @(Data B) n "plutus_data",
      cddlTest @(TxOut B) n "transaction_output",
      cddlAnnotatorTest @(Script B) n "script",
      cddlTest @(Datum B) n "datum_option",
      cddlAnnotatorTest @(TxWits B) n "transaction_witness_set",
      cddlTest @(PParamsUpdate B) n "protocol_param_update",
      cddlTest @CostModels n "costmdls",
      cddlAnnotatorTest @(Redeemers B) n "[* redeemer]",
      cddlAnnotatorTest @(Tx B) n "transaction"
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
