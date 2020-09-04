{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- Allow for an orphan HasTrace instance for CHAIN, since HasTrace only pertains to tests
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Chain where

import Cardano.Ledger.Era (Crypto, Era)
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
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, fromList, keysSet)
import Data.Proxy
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API
  ( CHAIN,
    ChainState,
  )
import Shelley.Spec.Ledger.BaseTypes (Globals)
import Shelley.Spec.Ledger.BlockChain
  ( HashHeader (..),
    LastAppliedBlock (..),
    hashHeaderToNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash,
    KeyRole (BlockIssuer),
    coerceKeyRole,
  )
import Shelley.Spec.Ledger.LedgerState (esAccountState, nesEs, _treasury)
import Shelley.Spec.Ledger.OverlaySchedule (overlaySchedule)
import Shelley.Spec.Ledger.STS.Chain (chainNes, initialShelleyState)
import qualified Shelley.Spec.Ledger.STS.Chain as STS (ChainState (ChainState))
import Shelley.Spec.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import Shelley.Spec.Ledger.UTxO (balance)
import qualified Cardano.Ledger.Val as Val
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genUtxo0, genesisDelegs0)
import Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import Test.Shelley.Spec.Ledger.Shrinkers (shrinkBlock)
import Test.Shelley.Spec.Ledger.Utils (maxLLSupply, mkHash, runShelleyBase)

-- The CHAIN STS at the root of the STS allows for generating blocks of transactions
-- with meaningful delegation certificates, protocol and application updates, withdrawals etc.
instance
  (Era era, Mock (Crypto era)) =>
  HasTrace (CHAIN era) (GenEnv era)
  where
  envGen _ = pure ()

  sigGen ge _env st = genBlock ge st

  shrinkSignal = shrinkBlock

  type BaseEnv (CHAIN era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: forall proxy era. Era era => proxy era -> HashHeader era
lastByronHeaderHash _ = HashHeader $ mkHash 0

-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC CHAIN' (the "initial rule context") instead of simply 'Chain Env'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisChainState ::
  forall era a.
  Era era =>
  Constants ->
  IRC (CHAIN era) ->
  Gen (Either a (ChainState era))
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
      (maxLLSupply Val.~~ balance utxo0)
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
                          { _treasury = Coin 1000000
                          }
                    }
              }
        }
    p :: Proxy era
    p = Proxy

mkOCertIssueNos ::
  GenDelegs h ->
  Map (KeyHash 'BlockIssuer h) Natural
mkOCertIssueNos (GenDelegs delegs0) =
  Map.fromList (fmap f (Map.elems delegs0))
  where
    f (GenDelegPair vk _) = (coerceKeyRole vk, 0)
