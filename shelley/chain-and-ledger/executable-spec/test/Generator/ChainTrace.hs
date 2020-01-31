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

import           Data.ByteString.Char8 (pack)
import           Data.Functor.Identity (runIdentity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, fromList, keysSet)
import           Data.Word (Word64)
import           Numeric.Natural (Natural)
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import           Unsafe.Coerce (unsafeCoerce)

import           BaseTypes (Globals, interval1)
import           BlockChain (pattern HashHeader)
import           Cardano.Crypto.Hash (ShortHash)
import           ConcreteCryptoTypes (Applications, CHAIN, ChainState, GenDelegs, HashHeader,
                     KeyHash)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition (IRC (..))
import           Control.State.Transition.Trace.Generator.QuickCheck (BaseEnv, HasTrace, envGen,
                     interpretSTS, shrinkSignal, sigGen)
import           Generator.Block (genBlock)
import           Generator.Core.Constants (maxGenesisUTxOouts, minGenesisUTxOouts)
import           Generator.Core.QuickCheck (coreNodeKeys, genUtxo0, genesisDelegs0,
                     maxLovelaceSupply, traceKeyPairsByStakeHash)
import           Generator.Update.QuickCheck (genPParams)
import           Keys (pattern GenDelegs, Hash, hash)
import           LedgerState (overlaySchedule)
import           PParams (PParams (_activeSlotCoeff, _d))
import           Shrinkers (shrinkBlock)
import           Slot (EpochNo (..), SlotNo (..))
import           STS.Chain (initialShelleyState)
import           Test.Utils (runShelleyBase, unsafeMkUnitInterval)
import           Updates (ApName (..), ApVer (..), pattern Applications, pattern Mdt)

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance HasTrace CHAIN Word64 where
  -- the current slot needs to be large enough to allow for many blocks
  -- to be processed (in large CHAIN traces)
  envGen _ = SlotNo <$> QC.choose (10, 100)

  sigGen _ env st =
    genBlock
      env
      st
      coreNodeKeys
      traceKeyPairsByStakeHash

  shrinkSignal = shrinkBlock

  type BaseEnv CHAIN = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ unsafeCoerce (hash 0 :: Hash ShortHash Int)

byronApps :: Applications
byronApps = Applications $ Map.fromList
                            [ (ApName $ pack "Daedalus", (ApVer 16, Mdt Map.empty))
                            , (ApName $ pack "Yoroi", (ApVer 4, Mdt Map.empty))
                            ]

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
  -- TODO @uroboros remove d=1 restriction when using LedgerState.overlaySchedule
  -- TODO @mgu remove active slot coefficient 1
  let pParamsCentralised = pParams { _d = interval1
                                   , _activeSlotCoeff = unsafeMkUnitInterval 1.0 }
      osched_ = runShelleyBase $ overlaySchedule
                epoch0
                (Map.keysSet delegs0)
                pParamsCentralised

  pure . Right $ initialShelleyState
    (SlotNo 0)
    epoch0
    lastByronHeaderHash
    utxo0
    maxLovelaceSupply
    delegs0
    osched_
    byronApps
    pParamsCentralised
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
