{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.CDDL
  ( tests,
    readDataFile,
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.TxWitness (Redeemers, TxWitness)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo_test
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

type A = AlonzoEra StandardCrypto

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Value A) n "coin",
      cddlAnnotatorTest @(TxBody A) n "transaction_body",
      cddlAnnotatorTest @(AuxiliaryData A) n "auxiliary_data",
      cddlAnnotatorTest @(MA.Timelock (AlonzoEra StandardCrypto)) n "native_script",
      cddlAnnotatorTest @(Data A) n "plutus_data",
      cddlTest @(TxOut A) n "transaction_output",
      cddlAnnotatorTest @(TxWitness A) n "transaction_witness_set",
      cddlTest @(PParamsUpdate A) n "protocol_param_update",
      cddlAnnotatorTest @(Redeemers A) n "[* redeemer]",
      cddlAnnotatorTest @(Tx A) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- readDataFile "cddl-files/alonzo.cddl"
  crypto <- readDataFile "cddl-files/real/crypto.cddl"
  extras <- readDataFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile
