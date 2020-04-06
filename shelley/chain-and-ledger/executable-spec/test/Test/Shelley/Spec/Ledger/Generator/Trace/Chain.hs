{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- Allow for an orphan HasTrace instance for CHAIN, since HasTrace only pertains to tests
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Chain where

import           Data.Functor.Identity (runIdentity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, fromList, keysSet)
import           Numeric.Natural (Natural)
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import           Unsafe.Coerce (unsafeCoerce)

import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition (IRC (..))
import           Control.State.Transition.Trace.Generator.QuickCheck (BaseEnv, HasTrace, envGen,
                     interpretSTS, shrinkSignal, sigGen)
import           Shelley.Spec.Ledger.BaseTypes (Globals)
import           Shelley.Spec.Ledger.BlockChain (pattern HashHeader, LastAppliedBlock (..),
                     hashHeaderToNonce)
import           Shelley.Spec.Ledger.Keys (pattern GenDelegs, Hash, hash)
import           Shelley.Spec.Ledger.LedgerState (overlaySchedule)
import           Shelley.Spec.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.STS.Chain (initialShelleyState)
import           Shelley.Spec.Ledger.UTxO (balance)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, ChainState, GenDelegs,
                     HashHeader, KeyHash)
import           Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import           Test.Shelley.Spec.Ledger.Generator.Constants (maxGenesisUTxOouts, maxSlotTrace,
                     minGenesisUTxOouts, minSlotTrace)
import           Test.Shelley.Spec.Ledger.Generator.Core (KeySpace(..))
import           Test.Shelley.Spec.Ledger.Generator.Presets (genUtxo0, genesisDelegs0)
import           Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import           Test.Shelley.Spec.Ledger.Shrinkers (shrinkBlock)
import           Test.Shelley.Spec.Ledger.Utils (maxLLSupply, runShelleyBase)

-- The CHAIN STS at the root of the STS allows for generating blocks of transactions
-- with meaningful delegation certificates, protocol and application updates, withdrawals etc.
instance HasTrace CHAIN KeySpace where
  -- the current slot needs to be large enough to allow for many blocks
  -- to be processed (in large CHAIN traces)
  envGen _ = SlotNo <$> QC.choose (fromIntegral minSlotTrace, fromIntegral maxSlotTrace)

  sigGen ks env st =
    genBlock
      ks
      env
      st

  shrinkSignal = shrinkBlock

  type BaseEnv CHAIN = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ unsafeCoerce (hash 0 :: Hash ShortHash Int)

-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC CHAIN' (the "initial rule context") instead of simply 'Chain Env'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisChainState
  :: IRC CHAIN
  -> Gen (Either a ChainState)
mkGenesisChainState (IRC _slotNo) = do
  utxo0 <- genUtxo0 minGenesisUTxOouts maxGenesisUTxOouts

  pParams <- genPParams
  let osched_ = runShelleyBase $ overlaySchedule
                epoch0
                (Map.keysSet delegs0)
                pParams

  pure . Right $ initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
    epoch0
    utxo0
    (maxLLSupply - balance utxo0)
    delegs0
    osched_
    pParams
    (hashHeaderToNonce lastByronHeaderHash)
  where
    epoch0 = EpochNo 0
    delegs0 = genesisDelegs0

mkOCertIssueNos
  :: GenDelegs
  -> Map KeyHash Natural
mkOCertIssueNos (GenDelegs delegs0) =
  Map.fromList (fmap f (Map.elems delegs0))
  where
    f vk = (vk, 0)
