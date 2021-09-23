{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Examples.EmptyBlock
  ( exEmptyBlock,
  )
where

import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Shelley.BlockChain (Block)
import Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Slot
  ( BlockNo (..),
    SlotNo (..),
  )
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..))
import Test.Cardano.Ledger.Shelley.Examples.Combinators
  ( evolveNonceUnfrozen,
    newLab,
  )
import Test.Cardano.Ledger.Shelley.Examples.Federation (coreNodeKeysBySchedule)
import Test.Cardano.Ledger.Shelley.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( NatNonce (..),
    PreAlonzo,
    mkBlockFakeVRF,
    mkOCert,
  )
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest, getBlockNonce)

-- =============================================================

initStEx1 :: forall era. ShelleyTest era => ChainState era
initStEx1 = initSt (UTxO Map.empty)

blockEx1 ::
  forall era.
  ( HasCallStack,
    ShelleyTest era,
    ExMock (Crypto era)
  ) =>
  Block era
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @era ppEx 10)
    []
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto era))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 10) 0 (KESPeriod 0))

blockNonce ::
  forall era.
  ( HasCallStack,
    PreAlonzo era,
    ShelleyTest era,
    ExMock (Crypto era)
  ) =>
  Nonce
blockNonce = getBlockNonce (blockEx1 @era)

expectedStEx1 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era), PreAlonzo era) =>
  ChainState era
expectedStEx1 =
  (evolveNonceUnfrozen (blockNonce @era))
    . (newLab blockEx1)
    $ initStEx1

-- | = Empty Block Example
--
-- This is the most minimal example of using the CHAIN STS transition.
-- It applies an empty block to an initial shelley chain state.
--
-- The only things that change in the chain state are the
-- evolving and candidate nonces, and the last applied block.
exEmptyBlock :: (ShelleyTest era, ExMock (Crypto era), PreAlonzo era) => CHAINExample era
exEmptyBlock = CHAINExample initStEx1 blockEx1 (Right expectedStEx1)
