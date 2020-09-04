{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Init
-- Description : Initial State for Shelley ledger examples
--
-- The initial state for Shelley Ledger Examples.
module Test.Shelley.Spec.Ledger.Examples.Init
  ( ppEx,
    initSt,
    nonce0,
    lastByronHeaderHash,
  )
where

import Cardano.Ledger.Era (Era)
import Cardano.Slotting.Slot (WithOrigin (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (..),
  )
import Shelley.Spec.Ledger.BlockChain
  ( HashHeader (..),
    LastAppliedBlock (..),
    hashHeaderToNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    emptyPParams,
  )
import Shelley.Spec.Ledger.STS.Chain
  ( ChainState (..),
    initialShelleyState,
  )
import Shelley.Spec.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    SlotNo (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..), balance)
import qualified Cardano.Ledger.Val as Val
import Test.Shelley.Spec.Ledger.Examples.Federation (genDelegs, overlayScheduleFor)
import Test.Shelley.Spec.Ledger.Utils (maxLLSupply, mkHash, unsafeMkUnitInterval)

-- === Initial Protocol Parameters
--
-- @
--   emptyPParams
--     { _maxBBSize = 50000,
--       _maxBHSize = 10000,
--       _maxTxSize = 10000,
--       _eMax = EpochNo 10000,
--       _keyDeposit = Coin 7,
--       _poolDeposit = Coin 250,
--       _d = unsafeMkUnitInterval 0.5,
--       _tau = unsafeMkUnitInterval 0.2,
--       _rho = unsafeMkUnitInterval 0.0021,
--       _minUTxOValue = 100
--     }
-- @
ppEx :: PParams
ppEx =
  emptyPParams
    { _maxBBSize = 50000,
      _maxBHSize = 10000,
      _maxTxSize = 10000,
      _eMax = EpochNo 10000,
      _keyDeposit = Coin 7,
      _poolDeposit = Coin 250,
      _d = unsafeMkUnitInterval 0.5,
      _tau = unsafeMkUnitInterval 0.2,
      _rho = unsafeMkUnitInterval 0.0021,
      _minUTxOValue = Coin 100
    }

-- | === The hash of the last Bryon Header
--
-- The first block of the Shelley era will point back to the
-- last block of the Byron era.
-- For our purposes in the examples we can bootstrap the chain
-- by just coercing the value.
-- When this transition actually occurs,
-- the consensus layer will do the work of making
-- sure that the hash gets translated across the fork.
lastByronHeaderHash :: forall era. Era era => HashHeader era
lastByronHeaderHash = HashHeader $ mkHash 0

-- | === Initial Nonce
nonce0 :: forall era. Era era => Nonce
nonce0 = hashHeaderToNonce (lastByronHeaderHash @era)

-- | === Initial Chain State
--
-- The initial state for the examples uses the function
-- 'initialShelleyState' with the genesis delegation
-- 'genDelegs' and any given starting 'UTxO' set.
initSt :: forall era. Era era => UTxO era -> ChainState era
initSt utxo =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
    (EpochNo 0)
    utxo
    (maxLLSupply Val.~~ (balance utxo))
    genDelegs
    (overlayScheduleFor (EpochNo 0) ppEx)
    ppEx
    (nonce0 @era)
