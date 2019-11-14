{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generator.LedgerTrace where

import           MockTypes (MockCrypto)
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Generator.Core (GenValidity (..), genCoin, traceKeyPairs, traceVRFKeyPairs)
import           Generator.Update (genPParams)
import           Generator.Utxo (genTx)
import           Slot (Slot (..))
import           STS.Ledger (LEDGER, LedgerEnv (..))

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance HasTrace (LEDGER MockCrypto)
  where
    envGen _ =
      LedgerEnv <$> pure (Slot 0)
                <*> pure 0
                <*> genPParams
                <*> genCoin 0 1000

    sigGen ledgerEnv ledgerSt =
      genTx ledgerEnv ledgerSt traceKeyPairs traceVRFKeyPairs GenValid
