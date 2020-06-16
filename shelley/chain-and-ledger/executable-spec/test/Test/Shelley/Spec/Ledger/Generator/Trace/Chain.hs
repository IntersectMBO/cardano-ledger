{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- Allow for an orphan HasTrace instance for CHAIN, since HasTrace only pertains to tests
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Chain where

import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition (IRC (..))
import Control.State.Transition.Trace.Generator.QuickCheck
  ( BaseEnv,
    HasTrace,
    envGen,
    interpretSTS,
    shrinkSignal,
    sigGen,
  )
import Data.Coerce (coerce)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, fromList, keysSet)
import Data.Proxy
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Globals)
import Shelley.Spec.Ledger.BlockChain
  ( LastAppliedBlock (..),
    hashHeaderToNonce,
    pattern HashHeader,
  )
import Shelley.Spec.Ledger.Keys (Hash, KeyRole (BlockIssuer), coerceKeyRole, hash)
import Shelley.Spec.Ledger.LedgerState (_treasury, esAccountState, nesEs, overlaySchedule)
import Shelley.Spec.Ledger.STS.Chain (chainNes, initialShelleyState)
import qualified Shelley.Spec.Ledger.STS.Chain as STS (ChainState (ChainState))
import Shelley.Spec.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import Shelley.Spec.Ledger.UTxO (balance)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( CHAIN,
    ChainState,
    ConcreteCrypto,
    GenDelegs,
    HashHeader,
    KeyHash,
    pattern GenDelegPair,
    pattern GenDelegs,
  )
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genUtxo0, genesisDelegs0)
import Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import Test.Shelley.Spec.Ledger.Shrinkers (shrinkBlock)
import Test.Shelley.Spec.Ledger.Utils (maxLLSupply, runShelleyBase)

-- The CHAIN STS at the root of the STS allows for generating blocks of transactions
-- with meaningful delegation certificates, protocol and application updates, withdrawals etc.
instance HashAlgorithm h => HasTrace (CHAIN h) (GenEnv h) where
  envGen _ = pure ()

  sigGen ge _env st = genBlock ge st

  shrinkSignal = shrinkBlock

  type BaseEnv (CHAIN h) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: forall proxy h. HashAlgorithm h => proxy h -> HashHeader h
lastByronHeaderHash _ = HashHeader $ coerce (hash 0 :: Hash (ConcreteCrypto h) Int)

-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC CHAIN' (the "initial rule context") instead of simply 'Chain Env'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisChainState ::
  forall h a.
  HashAlgorithm h =>
  Constants ->
  IRC (CHAIN h) ->
  Gen (Either a (ChainState h))
mkGenesisChainState constants (IRC _slotNo) = do
  utxo0 <- genUtxo0 constants

  pParams <- genPParams constants
  let osched_ =
        runShelleyBase $
          overlaySchedule
            epoch0
            (Map.keysSet delegs0)
            pParams

  pure . Right . withRewards $
    initialShelleyState
      (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) (lastByronHeaderHash p))
      epoch0
      utxo0
      (maxLLSupply - balance utxo0)
      delegs0
      osched_
      pParams
      (hashHeaderToNonce (lastByronHeaderHash p))
  where
    epoch0 = EpochNo 0
    delegs0 = genesisDelegs0 constants
    -- We preload the initial state with some Treasury to enable generation
    -- of things dependent on Treasury (e.g. MIR Treasury certificates)
    withRewards :: ChainState h -> ChainState h
    withRewards st@STS.ChainState {..} =
      st
        { chainNes =
            chainNes
              { nesEs =
                  (nesEs chainNes)
                    { esAccountState =
                        (esAccountState (nesEs chainNes))
                          { _treasury = 1000000
                          }
                    }
              }
        }
    p :: Proxy h
    p = Proxy

mkOCertIssueNos ::
  GenDelegs h ->
  Map (KeyHash h 'BlockIssuer) Natural
mkOCertIssueNos (GenDelegs delegs0) =
  Map.fromList (fmap f (Map.elems delegs0))
  where
    f (GenDelegPair vk _) = (coerceKeyRole vk, 0)
