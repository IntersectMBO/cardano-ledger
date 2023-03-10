{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialisation.CDDL (
  tests,
)
where

import Cardano.Ledger.Address (
  Addr,
  RewardAcnt,
 )
import Cardano.Ledger.BaseTypes (shelleyProtVer)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (
  Credential,
  MultiSig,
  ProposedPPUpdates,
  ShelleyTx,
  Update,
 )
import Cardano.Ledger.Shelley.Core (EraDCert (..), PParamsUpdate)
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Ledger.Shelley.TxBody (
  ShelleyTxBody,
  ShelleyTxOut,
  StakePoolRelay,
 )
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Protocol.TPraos.BHeader (BHBody, BHeader)
import Cardano.Protocol.TPraos.OCert (OCert)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_shelley_test
import Test.Cardano.Ledger.Shelley.LaxBlock (LaxBlock)
import Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils (
  cddlAnnotatorTest,
  cddlGroupTest,
  cddlTest,
 )
import Test.Tasty (TestTree, testGroup, withResource)

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlAnnotatorTest @(BHeader StandardCrypto) v n "header"
    , cddlAnnotatorTest @(BootstrapWitness StandardCrypto) v n "bootstrap_witness"
    , cddlTest @(BHBody StandardCrypto) v n "header_body"
    , cddlGroupTest @(OCert StandardCrypto) v n "operational_cert"
    , cddlTest @(Addr StandardCrypto) v n "address"
    , cddlTest @(RewardAcnt StandardCrypto) v n "reward_account"
    , cddlTest @(Credential 'Staking StandardCrypto) v n "stake_credential"
    , cddlAnnotatorTest @(ShelleyTxBody Shelley) v n "transaction_body"
    , cddlTest @(ShelleyTxOut Shelley) v n "transaction_output"
    , cddlTest @StakePoolRelay v n "relay"
    , cddlTest @(DCert Shelley) v n "certificate"
    , cddlTest @(TxIn StandardCrypto) v n "transaction_input"
    , cddlAnnotatorTest @(ShelleyTxAuxData Shelley) v n "transaction_metadata"
    , cddlAnnotatorTest @(MultiSig Shelley) v n "multisig_script"
    , cddlTest @(Update Shelley) v n "update"
    , cddlTest @(ProposedPPUpdates Shelley) v n "proposed_protocol_parameter_updates"
    , cddlTest @(PParamsUpdate Shelley) v n "protocol_param_update"
    , cddlAnnotatorTest @(ShelleyTx Shelley) v n "transaction"
    , cddlAnnotatorTest @(LaxBlock (BHeader StandardCrypto) Shelley) v n "block"
    ]
      <*> pure cddl
  where
    v = shelleyProtVer

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- readDataFile "cddl-files/shelley.cddl"
  crypto <- readDataFile "cddl-files/real/crypto.cddl"
  extras <- readDataFile "cddl-files/mock/extras.cddl"
  -- extras contains the types whose restrictions cannot be expressed in CDDL
  pure $ base <> crypto <> extras

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile
