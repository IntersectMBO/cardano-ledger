{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- Allow for an orphan HasTrace instance for CHAIN, since HasTrace only pertains to tests
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generator.ChainTrace where

import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, fromList)
import           Numeric.Natural (Natural)
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import           Unsafe.Coerce (unsafeCoerce)

import           BaseTypes (Globals, Nonce (..), mkNonce)
import           BlockChain (pattern HashHeader)
import           Cardano.Crypto.Hash (ShortHash)
import           ConcreteCryptoTypes (CHAIN, ChainState, GenDelegs, HashHeader, KeyHash)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition (IRC)
import           Control.State.Transition.Trace.Generator.QuickCheck (BaseEnv, HasTrace, envGen,
                     interpretSTS, shrinkSignal, sigGen)
import           Data.Functor.Identity (runIdentity)
import           Data.Word (Word64)
import           Delegation.Certificates (pattern PoolDistr)
import           EpochBoundary (BlocksMade (..), emptySnapShots)
import           Generator.Block (genBlock)
import           Generator.Core.QuickCheck (coreNodeKeys, coreNodeVKG, genesisAccountState)
import           Generator.LedgerTrace.QuickCheck (mkGenesisLedgerState)
import           Generator.Update.QuickCheck (genPParams)
import           Keys (pattern GenDelegs, Hash, hash, hashKey)
import           LedgerState (pattern EpochState, pattern LedgerState, pattern NewEpochState,
                     _dstate, _genDelegs)
import           Shrinkers (shrinkBlock)
import           Slot (EpochNo (..), SlotNo (..))
import           STS.Chain (pattern ChainState)

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance HasTrace CHAIN Word64 where
  -- the current slot needs to be large enough to allow for many blocks
  -- to be processed (in large CHAIN traces)
  envGen _ = SlotNo <$> QC.choose (10000, 100000)

  sigGen _ env st =
    genBlock
      env
      st
      coreNodeKeys

  shrinkSignal = shrinkBlock

  type BaseEnv CHAIN = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ unsafeCoerce (hash 0 :: Hash ShortHash Int)

-- TODO @uroboros use `initialShelleyState`
-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC CHAIN' (the "initial rule context") instead of simply 'Chain Env'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisChainState
  :: IRC CHAIN
  -> Gen (Either a ChainState)
mkGenesisChainState _chainEnv = do
  (utxoSt, dpSt) <- fromRight (error "mkGenesisChainState - could not generate genesis Ledger State")
                    <$> mkGenesisLedgerState undefined

  pParams <- genPParams

  pure . Right $ ChainState
    (NewEpochState
      (EpochNo 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState genesisAccountState emptySnapShots (LedgerState utxoSt dpSt) pParams)
      Nothing
      (PoolDistr Map.empty)
      (leaderSchedule 300))
    (mkOCertIssueNos ((_genDelegs . _dstate) dpSt))
    (mkNonce 0)
    (mkNonce 0)
    (mkNonce 0)
    NeutralNonce
    lastByronHeaderHash
    (SlotNo 42)

  where
    -- TODO @uroboros replace with LedgerState.overlaySchedule
    leaderSchedule n = Map.fromList $
      zip ((SlotNo . fromIntegral) <$> [0..n])
          (replicate n ((Just . hashKey) (coreNodeVKG 0)))


mkOCertIssueNos
  :: GenDelegs
  -> Map KeyHash Natural
mkOCertIssueNos (GenDelegs delegs0) =
  Map.fromList (fmap f (Map.elems delegs0))
  where
    f vk = (vk, 0)
