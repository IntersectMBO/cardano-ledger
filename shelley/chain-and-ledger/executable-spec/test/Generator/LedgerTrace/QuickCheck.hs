{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generator.LedgerTrace.QuickCheck where

import           BaseTypes (Globals)
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import           Data.Functor.Identity (runIdentity)
import           Data.Word (Word64)
import           Generator.Core.QuickCheck (coreKeyPairs, genCoin, traceKeyPairs, traceVRFKeyPairs)
import           Generator.Update.QuickCheck (genPParams)
import           Generator.Utxo.QuickCheck (genTx)
import           MockTypes (LEDGER)
import           Shrinkers (shrinkTx)
import           Slot (SlotNo (..))
import           STS.Ledger (LedgerEnv (..))

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance TQC.HasTrace LEDGER Word64 where

  envGen _ =
    LedgerEnv <$> pure (SlotNo 0)
              <*> pure 0
              <*> genPParams
              <*> genCoin 1000000 10000000

  sigGen _ ledgerEnv ledgerSt =
    genTx ledgerEnv ledgerSt traceKeyPairs coreKeyPairs traceVRFKeyPairs

  shrinkSignal = shrinkTx

  type BaseEnv LEDGER = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
