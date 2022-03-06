{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.CDDL
  ( tests,
  )
where

import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.TxWitness (Redeemers, TxWitness)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParamsUpdate)
import Cardano.Ledger.Babbage.Tx (ValidatedTx)
import Cardano.Ledger.Babbage.TxBody (Datum, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlAnnotatorTest,
    cddlTest,
  )
import Test.Tasty (TestTree, testGroup, withResource)

type B = BabbageEra StandardCrypto

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value B) n "coin",
      cddlAnnotatorTest @(Core.TxBody B) n "transaction_body",
      cddlAnnotatorTest @(Core.AuxiliaryData B) n "auxiliary_data",
      cddlAnnotatorTest @(MA.Timelock StandardCrypto) n "native_script",
      cddlAnnotatorTest @(Data B) n "plutus_data",
      cddlTest @(TxOut B) n "transaction_output",
      cddlAnnotatorTest @(Core.Script B) n "script",
      cddlTest @(Datum B) n "datum",
      cddlAnnotatorTest @(TxWitness B) n "transaction_witness_set",
      cddlTest @(PParamsUpdate B) n "protocol_param_update",
      cddlAnnotatorTest @(Redeemers B) n "[* redeemer]",
      cddlAnnotatorTest @(ValidatedTx B) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/babbage.cddl"
  crypto <- BSL.readFile "cddl-files/real/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras
