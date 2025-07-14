{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The initial state for Shelley Ledger Examples.
module Test.Cardano.Ledger.Shelley.Examples.Init (
  ppEx,
  initSt,
  nonce0,
  lastByronHeaderHash,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Nonce (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (
  BlockNo (..),
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.Val ((<->))
import Cardano.Protocol.TPraos.BHeader (
  LastAppliedBlock (..),
  hashHeaderToNonce,
 )
import Cardano.Slotting.Slot (WithOrigin (..))
import Data.Default
import Lens.Micro
import Test.Cardano.Ledger.Shelley.Examples.Federation (genDelegs)
import Test.Cardano.Ledger.Shelley.Rules.Chain (
  ChainState (..),
  initialShelleyState,
 )
import Test.Cardano.Ledger.Shelley.Utils (maxLLSupply, mkHash, unsafeBoundRational)

-- | Initial Protocol Parameters
ppEx :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => PParams era
ppEx =
  emptyPParams
    & ppMaxBBSizeL .~ 50000
    & ppMaxBHSizeL .~ 10000
    & ppMaxTxSizeL .~ 10000
    & ppEMaxL .~ EpochInterval 10000
    & ppKeyDepositL .~ Coin 7
    & ppPoolDepositL .~ Coin 250
    & ppDL .~ unsafeBoundRational 0.5
    & ppTauL .~ unsafeBoundRational 0.2
    & ppRhoL .~ unsafeBoundRational 0.0021
    & ppMinUTxOValueL .~ Coin 100

-- | === The hash of the last Bryon Header
--
-- The first block of the Shelley era will point back to the
-- last block of the Byron era.
-- For our purposes in the examples we can bootstrap the chain
-- by just coercing the value.
-- When this transition actually occurs,
-- the consensus layer will do the work of making
-- sure that the hash gets translated across the fork.
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ mkHash 0

-- | === Initial Nonce
nonce0 :: Nonce
nonce0 = hashHeaderToNonce lastByronHeaderHash

-- | === Initial Chain State
--
-- The initial state for the examples uses the function
-- 'initialShelleyState' with the genesis delegation
-- 'genDelegs' and any given starting 'UTxO' set.
initSt ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , Default (StashedAVVMAddresses era)
  ) =>
  UTxO era ->
  ChainState era
initSt utxo =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
    (EpochNo 0)
    utxo
    (maxLLSupply <-> sumCoinUTxO utxo)
    genDelegs
    (ppEx @era)
    nonce0
