{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generator.LedgerTrace where

import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.VRF.Fake (FakeVRF)

import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Generator.Core (traceKeyPairs)
import           Generator.Delegation (genPParams)
import           Generator.Utxo (genTx)
import           Slot (Slot (..))
import           STS.Ledger (LEDGER, LedgerEnv (..))

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance HasTrace (LEDGER ShortHash MockDSIGN FakeVRF)
  where
    envGen _ =
      LedgerEnv <$> pure (Slot 0)
                <*> pure 0
                <*> genPParams

    sigGen ledgerEnv ledgerSt =
      genTx ledgerEnv ledgerSt traceKeyPairs
