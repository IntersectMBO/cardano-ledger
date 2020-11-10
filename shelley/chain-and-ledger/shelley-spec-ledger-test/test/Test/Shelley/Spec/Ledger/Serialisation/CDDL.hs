{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialisation.CDDL
  ( tests,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Shelley.Spec.Ledger.API
  ( Credential,
    DCert,
    MultiSig,
    OCert,
    ProposedPPUpdates,
    Tx,
    Update,
  )
import Shelley.Spec.Ledger.Address
  ( Addr,
    RewardAcnt,
  )
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.BlockChain
  ( BHBody,
    BHeader,
    LaxBlock,
  )
import Shelley.Spec.Ledger.Keys (KeyRole (Staking))
import Shelley.Spec.Ledger.MetaData (MetaData)
import Shelley.Spec.Ledger.PParams (PParamsUpdate)
import Shelley.Spec.Ledger.TxBody
  ( StakePoolRelay,
    TxBody,
    TxIn,
    TxOut,
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C, C_Crypto)
import Test.Tasty (TestTree, withResource, testGroup)
import Test.Shelley.Spec.Ledger.Serialisation.CDDLUtils
  ( cddlGroupTest,
    cddlTest,
    cddlTest',
  )

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest' @(BHeader C_Crypto) n "header",
      cddlTest' @(BootstrapWitness C) n "bootstrap_witness",
      cddlTest @(BHBody C_Crypto) n "header_body",
      cddlGroupTest @(OCert C_Crypto) n "operational_cert",
      cddlTest @(Addr C) n "address",
      cddlTest @(RewardAcnt C) n "reward_account",
      cddlTest @(Credential 'Staking C) n "stake_credential",
      cddlTest' @(TxBody C) n "transaction_body",
      cddlTest @(TxOut C) n "transaction_output",
      cddlTest @StakePoolRelay n "relay",
      cddlTest @(DCert C) n "certificate",
      cddlTest @(TxIn C) n "transaction_input",
      cddlTest' @MetaData n "transaction_metadata",
      cddlTest' @(MultiSig C) n "multisig_script",
      cddlTest @(Update C) n "update",
      cddlTest @(ProposedPPUpdates C) n "proposed_protocol_parameter_updates",
      cddlTest @(PParamsUpdate C) n "protocol_param_update",
      cddlTest' @(Tx C) n "transaction",
      cddlTest' @(LaxBlock C) n "block"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/shelley.cddl"
  crypto <- BSL.readFile "cddl-files/mock/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras
