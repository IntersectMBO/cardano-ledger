{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Allow for an orphan HasTrace instance for CHAIN, since HasTrace only pertains to tests
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Generator.Trace.Chain where

import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, incrementalStakeDistr)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv,
  ShelleyBbodyState,
 )
import Cardano.Ledger.Slot (
  BlockNo (..),
  EpochNo (..),
  SlotNo (..),
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader (
  HashHeader (..),
  LastAppliedBlock (..),
  hashHeaderToNonce,
 )
import Cardano.Protocol.TPraos.Rules.Tickn (
  TicknEnv,
  TicknState,
 )
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
import Data.Functor.Identity (runIdentity)
import qualified Data.ListMap as LM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Block (genBlock)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (
  EraGen (..),
  MinCHAIN_STS,
  MinLEDGER_STS,
  genUtxo0,
 )
import Test.Cardano.Ledger.Shelley.Generator.Presets (genesisDelegs0)
import Test.Cardano.Ledger.Shelley.Rules.Chain (
  CHAIN,
  ChainState (..),
  initialShelleyState,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.Chain as STS (ChainState (ChainState))
import Test.Cardano.Ledger.Shelley.Utils (maxLLSupply, mkHash)
import Test.Control.State.Transition.Trace.Generator.QuickCheck (
  BaseEnv,
  HasTrace,
  envGen,
  interpretSTS,
  sigGen,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (Gen)

-- ======================================================

-- The CHAIN STS at the root of the STS allows for generating blocks of transactions
-- with meaningful delegation certificates, protocol and application updates, withdrawals etc.
instance
  ( EraGen era
  , EraSegWits era
  , Mock (EraCrypto era)
  , ApplyBlock era
  , GetLedgerView era
  , MinLEDGER_STS era
  , MinCHAIN_STS era
  , Embed (EraRule "BBODY" era) (CHAIN era)
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block (BHeaderView (EraCrypto era)) era
  , Embed (EraRule "TICKN" era) (CHAIN era)
  , Environment (EraRule "TICKN" era) ~ TicknEnv
  , State (EraRule "TICKN" era) ~ TicknState
  , Signal (EraRule "TICKN" era) ~ Bool
  , Embed (EraRule "TICK" era) (CHAIN era)
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , QC.HasTrace (EraRule "LEDGERS" era) (GenEnv era)
  ) =>
  HasTrace (CHAIN era) (GenEnv era)
  where
  envGen _ = pure ()

  sigGen ge _env st = genBlock ge st

  shrinkSignal = (\_x -> []) -- shrinkBlock -- TO DO FIX ME

  type BaseEnv (CHAIN era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: forall proxy era. Era era => proxy era -> HashHeader (EraCrypto era)
lastByronHeaderHash _ = HashHeader $ mkHash 0

-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC CHAIN' (the "initial rule context") instead of simply 'Chain Env'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisChainState ::
  forall era a.
  ( EraGen era
  , EraGov era
  ) =>
  GenEnv era ->
  IRC (CHAIN era) ->
  Gen (Either a (ChainState era))
mkGenesisChainState ge@(GenEnv _ _ constants) (IRC _slotNo) = do
  utxo0 <- genUtxo0 ge

  pParams <- genEraPParams @era constants

  pure . Right . withRewards $
    initialShelleyState @era
      (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) (lastByronHeaderHash p))
      epoch0
      utxo0
      (maxLLSupply <-> Val.coin (balance utxo0))
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
                          { asTreasury = Coin 1000000
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
  forall era.
  EraGov era =>
  ShelleyGenesisStaking (EraCrypto era) ->
  ChainState era ->
  ChainState era
registerGenesisStaking
  ShelleyGenesisStaking {sgsPools, sgsStake}
  cs@STS.ChainState {chainNes = oldChainNes} =
    cs
      { chainNes = newChainNes
      }
    where
      keyDeposit :: UM.CompactForm Coin
      keyDeposit = (UM.compactCoinOrError . view ppKeyDepositL . view curPParamsEpochStateL . nesEs) oldChainNes
      oldEpochState = nesEs oldChainNes
      oldLedgerState = esLState oldEpochState
      oldCertState = lsCertState oldLedgerState

      -- Note that this is only applicable in the initial configuration where
      -- there is no existing stake distribution, since it would completely
      -- overwrite any such thing.
      newPoolDistr = calculatePoolDistr initSnapShot

      newChainNes =
        oldChainNes
          { nesEs = newEpochState
          , nesPd = newPoolDistr
          }
      newEpochState =
        oldEpochState
          { esLState = newLedgerState
          , esSnapshots =
              (esSnapshots oldEpochState)
                { ssStakeMark = initSnapShot
                , ssStakeMarkPoolDistr = newPoolDistr
                }
          }
      newLedgerState =
        oldLedgerState
          { lsCertState = newCertState
          }
      newCertState =
        oldCertState
          { certDState = newDState
          , certPState = newPState
          }
      -- New delegation state. Since we're using base addresses, we only care
      -- about updating the 'ssDelegations' field.
      --
      -- See STS DELEG for details
      pairWithDepositsButNoRewards _ = UM.RDPair (UM.CompactCoin 0) keyDeposit
      newDState :: DState era
      newDState =
        (certDState oldCertState)
          { dsUnified =
              UM.unify
                (Map.map pairWithDepositsButNoRewards . Map.mapKeys KeyHashObj . LM.toMap $ sgsStake)
                (UM.ptrMap (dsUnified (certDState oldCertState)))
                (Map.mapKeys KeyHashObj $ LM.toMap sgsStake)
                Map.empty
          }

      -- We consider pools as having been registered in slot 0
      -- See STS POOL for details
      newPState :: PState era
      newPState =
        (certPState oldCertState)
          { psStakePoolParams = LM.toMap sgsPools
          }

      -- The new stake distribution is made on the basis of a snapshot taken
      -- during the previous epoch. We create a "fake" snapshot in order to
      -- establish an initial stake distribution.
      initSnapShot =
        incrementalStakeDistr
          (newEpochState ^. curPParamsEpochStateL)
          (utxosStakeDistr . lsUTxOState . esLState $ oldEpochState)
          newDState
          newPState
