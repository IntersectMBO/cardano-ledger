{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- Allow for an orphan HasTrace instance for CHAIN, since HasTrace only pertains to tests
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Chain where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Val as Val
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition (BaseM, Environment, IRC (..), STS, Signal, State)
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
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence (Seq)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.BlockChain
  ( LastAppliedBlock (..),
    hashHeaderToNonce,
  )
import Shelley.Spec.Ledger.LedgerState (stakeDistr)
import qualified Shelley.Spec.Ledger.STS.Chain as STS (ChainState (ChainState))
import Shelley.Spec.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import Test.Shelley.Spec.Ledger.Generator.Utxo (GenTxFunc (..))
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genUtxo0, genesisDelegs0)
import Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import Test.Shelley.Spec.Ledger.Shrinkers (shrinkBlock)
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest, maxLLSupply, mkHash)

-- The CHAIN STS at the root of the STS allows for generating blocks of transactions
-- with meaningful delegation certificates, protocol and application updates, withdrawals etc.
instance
  ( ShelleyTest era,
    GenTxFunc era, 
    GetLedgerView era,
    ApplyBlock era,
    STS (CHAIN era),
    BaseM (CHAIN era) ~ ShelleyBase,
    STS (LEDGERS era),
    BaseM (LEDGERS era) ~ ShelleyBase,
    Environment (CHAIN era) ~ (),
    State (CHAIN era) ~ ChainState era,
    Signal (CHAIN era) ~ Block era,
    Environment (LEDGERS era) ~ LedgersEnv era,
    State (LEDGERS era) ~ LedgerState era,
    Signal (LEDGERS era) ~ Seq (Tx era),
    STS (LEDGER era),
    BaseM (LEDGER era) ~ ShelleyBase,
    Environment (LEDGER era) ~ LedgerEnv era,
    State (LEDGER era) ~ (UTxOState era, DPState era),
    Signal (LEDGER era) ~ Tx era,
    Mock (Crypto era)
  ) =>
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
lastByronHeaderHash :: forall proxy era. Era era => proxy era -> HashHeader (Crypto era)
lastByronHeaderHash _ = HashHeader $ mkHash 0

-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC CHAIN' (the "initial rule context") instead of simply 'Chain Env'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisChainState ::
  forall era a.
  (ShelleyTest era) =>
  Gen (Core.Value era) ->
  Constants ->
  IRC (CHAIN era) ->
  Gen (Either a (ChainState era))
mkGenesisChainState gv constants (IRC _slotNo) = do
  utxo0 <- genUtxo0 gv constants

  pParams <- genPParams constants

  pure . Right . withRewards $
    initialShelleyState
      (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) (lastByronHeaderHash p))
      epoch0
      utxo0
      (maxLLSupply <-> (Val.coin $ balance utxo0))
      delegs0
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

-- Register the initial staking.
--
-- This allows stake pools to produce blocks from genesis.
registerGenesisStaking ::
  forall c.
  ShelleyTest c =>
  ShelleyGenesisStaking c ->
  ChainState c ->
  ChainState c
registerGenesisStaking
  ShelleyGenesisStaking {sgsPools, sgsStake}
  cs@(STS.ChainState {chainNes = oldChainNes}) =
    cs
      { chainNes = newChainNes
      }
    where
      oldEpochState = nesEs $ oldChainNes
      oldLedgerState = esLState oldEpochState
      oldDPState = _delegationState oldLedgerState

      -- Note that this is only applicable in the initial configuration where
      -- there is no existing stake distribution, since it would completely
      -- overwrite any such thing.
      newPoolDistr = calculatePoolDistr initSnapShot

      newChainNes =
        oldChainNes
          { nesEs = newEpochState,
            nesPd = newPoolDistr
          }
      newEpochState =
        oldEpochState
          { esLState = newLedgerState,
            esSnapshots =
              (esSnapshots oldEpochState)
                { _pstakeMark = initSnapShot
                }
          }
      newLedgerState =
        oldLedgerState
          { _delegationState = newDPState
          }
      newDPState =
        oldDPState
          { _dstate = newDState,
            _pstate = newPState
          }
      -- New delegation state. Since we're using base addresses, we only care
      -- about updating the '_delegations' field.
      --
      -- See STS DELEG for details
      newDState :: DState c
      newDState =
        (_dstate oldDPState)
          { _rewards =
              Map.map (const $ Coin 0)
                . Map.mapKeys KeyHashObj
                $ sgsStake,
            _delegations = Map.mapKeys KeyHashObj sgsStake
          }

      -- We consider pools as having been registered in slot 0
      -- See STS POOL for details
      newPState :: PState c
      newPState =
        (_pstate oldDPState)
          { _pParams = sgsPools
          }

      -- The new stake distribution is made on the basis of a snapshot taken
      -- during the previous epoch. We create a "fake" snapshot in order to
      -- establish an initial stake distribution.
      initSnapShot =
        stakeDistr @c
          (_utxo . _utxoState . esLState $ oldEpochState)
          newDState
          newPState
