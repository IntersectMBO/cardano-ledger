{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.CDDL
  ( tests,
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.PParams (PParamsUpdate)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxBody (TxOut)
import Cardano.Ledger.Alonzo.TxWitness (Redeemers, TxWitness)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

type A = AlonzoEra StandardCrypto

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value A) n "coin",
      cddlAnnotatorTest @(Core.TxBody A) n "transaction_body",
      cddlAnnotatorTest @(Core.AuxiliaryData A) n "auxiliary_data",
      cddlAnnotatorTest @(MA.Timelock StandardCrypto) n "native_script",
      cddlAnnotatorTest @(Data A) n "plutus_data",
      cddlTest @(TxOut A) n "transaction_output",
      cddlAnnotatorTest @(TxWitness A) n "transaction_witness_set",
      cddlTest @(PParamsUpdate A) n "protocol_param_update",
      cddlAnnotatorTest @(Redeemers A) n "[* redeemer]",
      cddlAnnotatorTest @(ValidatedTx A) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/alonzo.cddl"
  crypto <- BSL.readFile "cddl-files/real/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras
