{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Init
-- Description : Initial State for Shelley ledger examples
--
-- The initial state for Shelley Ledger Examples.
module Test.Cardano.Ledger.Shelley.Examples.Init (
  ppEx,
  initSt,
  nonce0,
  lastByronHeaderHash,
)
where

import Cardano.Ledger.BaseTypes (Nonce (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Core (EraTallyState)
import Cardano.Ledger.Shelley.LedgerState (PPUPState, ShelleyPPUPState (..), StashedAVVMAddresses)
import Cardano.Ledger.Slot (
  BlockNo (..),
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.UTxO (UTxO (..), balance)
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (
  HashHeader (..),
  LastAppliedBlock (..),
  hashHeaderToNonce,
 )
import Cardano.Slotting.Slot (WithOrigin (..))
import Data.Default.Class
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
    & ppEMaxL .~ EpochNo 10000
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
lastByronHeaderHash ::
  forall c.
  Crypto c =>
  HashHeader c
lastByronHeaderHash = HashHeader $ mkHash 0

-- | === Initial Nonce
nonce0 ::
  forall c.
  Crypto c =>
  Nonce
nonce0 = hashHeaderToNonce (lastByronHeaderHash @c)

-- | === Initial Chain State
--
-- The initial state for the examples uses the function
-- 'initialShelleyState' with the genesis delegation
-- 'genDelegs' and any given starting 'UTxO' set.
initSt ::
  forall era.
  ( EraTxOut era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , Default (StashedAVVMAddresses era)
  , ShelleyPPUPState era ~ PPUPState era
  , EraTallyState era
  ) =>
  UTxO era ->
  ChainState era
initSt utxo =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
    (EpochNo 0)
    utxo
    (maxLLSupply <-> Val.coin (balance utxo))
    genDelegs
    (ppEx @era)
    (nonce0 @(EraCrypto era))
