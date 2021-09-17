{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Serialisation.CDDL
  ( tests,
  )
where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.VRF.Praos (PraosVRF)
import Cardano.Ledger.Address
  ( Addr,
    RewardAcnt,
  )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.TPraos.BHeader (BHBody, BHeader)
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
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.BlockChain (LaxBlock)
import Shelley.Spec.Ledger.Metadata (Metadata)
import Shelley.Spec.Ledger.PParams (PParamsUpdate)
import Shelley.Spec.Ledger.TxBody
  ( StakePoolRelay,
    TxBody,
    TxIn,
    TxOut,
  )
import Test.Shelley.Spec.Ledger.Serialisation.CDDLUtils
  ( cddlGroupTest,
    cddlTest,
    cddlTest',
  )
import Test.Tasty (TestTree, testGroup, withResource)

-- Crypto family as used in production Shelley
-- TODO: we really need a central location for all the Crypto and Era families.
-- I think we need something like Test.Cardano.Ledger.EraBuffet
-- (currently in the shelley-ma-test package)
-- that lives outside any era specific package.
data ShelleyC

instance Cardano.Ledger.Crypto.Crypto ShelleyC where
  type DSIGN ShelleyC = Ed25519DSIGN
  type KES ShelleyC = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF ShelleyC = PraosVRF
  type HASH ShelleyC = Blake2b_256
  type ADDRHASH ShelleyC = Blake2b_224

type ShelleyE = ShelleyEra ShelleyC

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest' @(BHeader ShelleyC) n "header",
      cddlTest' @(BootstrapWitness ShelleyC) n "bootstrap_witness",
      cddlTest @(BHBody ShelleyC) n "header_body",
      cddlGroupTest @(OCert ShelleyC) n "operational_cert",
      cddlTest @(Addr ShelleyC) n "address",
      cddlTest @(RewardAcnt ShelleyC) n "reward_account",
      cddlTest @(Credential 'Staking ShelleyC) n "stake_credential",
      cddlTest' @(TxBody ShelleyE) n "transaction_body",
      cddlTest @(TxOut ShelleyE) n "transaction_output",
      cddlTest @StakePoolRelay n "relay",
      cddlTest @(DCert ShelleyC) n "certificate",
      cddlTest @(TxIn ShelleyC) n "transaction_input",
      cddlTest' @(Metadata ShelleyE) n "transaction_metadata",
      cddlTest' @(MultiSig ShelleyC) n "multisig_script",
      cddlTest @(Update ShelleyE) n "update",
      cddlTest @(ProposedPPUpdates ShelleyE) n "proposed_protocol_parameter_updates",
      cddlTest @(PParamsUpdate ShelleyE) n "protocol_param_update",
      cddlTest' @(Tx ShelleyE) n "transaction",
      cddlTest' @(LaxBlock ShelleyE) n "block"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/shelley.cddl"
  crypto <- BSL.readFile "cddl-files/real/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  --extras contains the types whose restrictions cannot be expressed in CDDL
  pure $ base <> crypto <> extras
