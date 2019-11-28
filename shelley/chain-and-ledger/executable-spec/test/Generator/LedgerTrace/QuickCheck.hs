{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generator.LedgerTrace.QuickCheck where

import           Data.Word (Word64)

import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import           Generator.Core.QuickCheck (genCoin, traceKeyPairs, traceVRFKeyPairs)
import           Generator.Update.QuickCheck (genPParams)
import           Generator.Utxo.QuickCheck (genTx)
import           MockTypes (LEDGER)
import           Shrinkers (shrinkTx)
import           Slot (Slot (..))
import           STS.Ledger (LedgerEnv (..))

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance TQC.HasTrace LEDGER Word64 where

  envGen _ =
    LedgerEnv <$> pure (Slot 0)
              <*> pure 0
              <*> genPParams
              <*> genCoin 0 1000

  -- TODO pass core node keys as inputs
  sigGen _ ledgerEnv ledgerSt =
    genTx ledgerEnv ledgerSt traceKeyPairs [] traceVRFKeyPairs

  shrinkSignal = shrinkTx
