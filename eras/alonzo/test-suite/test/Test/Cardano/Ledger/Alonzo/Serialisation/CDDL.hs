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
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils
  ( cddlTest,
    cddlTest',
  )
import Test.Tasty (TestTree, testGroup, withResource)

type A = AlonzoEra C_Crypto

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value A) n "coin",
      cddlTest' @(Core.TxBody A) n "transaction_body",
      cddlTest' @(Core.AuxiliaryData A) n "auxiliary_data",
      cddlTest' @(MA.Timelock C_Crypto) n "native_script",
      cddlTest' @(Data A) n "plutus_data",
      cddlTest @(TxOut A) n "transaction_output",
      cddlTest' @(TxWitness A) n "transaction_witness_set",
      cddlTest @(PParamsUpdate A) n "protocol_param_update",
      cddlTest' @(Redeemers A) n "[* redeemer]",
      cddlTest' @(ValidatedTx A) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/alonzo.cddl"
  crypto <- BSL.readFile "cddl-files/mock/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras
