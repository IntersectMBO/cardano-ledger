{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Init
-- Description : Initial State for Shelley ledger examples
--
-- The initial state for Shelley Ledger Examples.
module Test.Cardano.Ledger.Shelley.Examples.Init
  ( ppEx,
    initSt,
    nonce0,
    lastByronHeaderHash,
  )
where

import Cardano.Ledger.BaseTypes
  ( Nonce (..),
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.PParams
  ( PParams,
    PParams' (..),
    emptyPParams,
  )
import Cardano.Ledger.Shelley.Rules.Chain
  ( ChainState (..),
    initialShelleyState,
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    SlotNo (..),
  )
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader
  ( HashHeader (..),
    LastAppliedBlock (..),
    hashHeaderToNonce,
  )
import Cardano.Slotting.Slot (WithOrigin (..))
import Test.Cardano.Ledger.Shelley.Examples.Federation (genDelegs)
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest, maxLLSupply, mkHash, unsafeBoundRational)

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
--       _d = unsafeBoundRational 0.5,
--       _tau = unsafeBoundRational 0.2,
--       _rho = unsafeBoundRational 0.0021,
--       _minUTxOValue = 100
--     }
-- @
ppEx :: PParams era
ppEx =
  emptyPParams
    { _maxBBSize = 50000,
      _maxBHSize = 10000,
      _maxTxSize = 10000,
      _eMax = EpochNo 10000,
      _keyDeposit = Coin 7,
      _poolDeposit = Coin 250,
      _d = unsafeBoundRational 0.5,
      _tau = unsafeBoundRational 0.2,
      _rho = unsafeBoundRational 0.0021,
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
lastByronHeaderHash ::
  forall c.
  CryptoClass.Crypto c =>
  HashHeader c
lastByronHeaderHash = HashHeader $ mkHash 0

-- | === Initial Nonce
nonce0 ::
  forall c.
  CryptoClass.Crypto c =>
  Nonce
nonce0 = hashHeaderToNonce (lastByronHeaderHash @c)

-- | === Initial Chain State
--
-- The initial state for the examples uses the function
-- 'initialShelleyState' with the genesis delegation
-- 'genDelegs' and any given starting 'UTxO' set.
initSt :: forall era. ShelleyTest era => UTxO era -> ChainState era
initSt utxo =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
    (EpochNo 0)
    utxo
    (maxLLSupply <-> (Val.coin $ balance utxo))
    genDelegs
    ppEx
    (nonce0 @(Crypto era))
