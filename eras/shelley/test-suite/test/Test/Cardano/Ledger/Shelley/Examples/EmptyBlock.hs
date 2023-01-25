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
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Slot
  ( BlockNo (..),
    SlotNo (..),
  )
import Cardano.Protocol.HeaderCrypto (HeaderCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
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
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest, getBlockNonce)

-- =============================================================

initStEx1 ::
  forall era hc.
  (ShelleyTest era,
   HeaderCrypto hc,
   PParams era ~ ShelleyPParams era) => ChainState era
initStEx1 = initSt @era @hc (UTxO mempty)

blockEx1 ::
  forall era hc.
  ( HasCallStack,
    ShelleyTest era,
    EraSegWits era,
    ExMock (Crypto era) hc
  ) =>
  Block (BHeader (Crypto era) hc) era
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
  forall era hc.
  ( HasCallStack,
    PreAlonzo era,
    ShelleyTest era,
    EraSegWits era,
    ExMock (Crypto era) hc
  ) =>
  Nonce
blockNonce = getBlockNonce (blockEx1 @era @hc)

expectedStEx1 ::
  forall era hc.
  ( ShelleyTest era,
    EraSegWits era,
    ExMock (Crypto era) hc,
    PreAlonzo era,
    PParams era ~ ShelleyPParams era
  ) =>
  ChainState era
expectedStEx1 = evolveNonceUnfrozen (blockNonce @era @hc) . newLab (blockEx1 @era @hc) $ (initStEx1 @era @hc)

-- | = Empty Block Example
--
-- This is the most minimal example of using the CHAIN STS transition.
-- It applies an empty block to an initial shelley chain state.
--
-- The only things that change in the chain state are the
-- evolving and candidate nonces, and the last applied block.
exEmptyBlock ::
  forall era hc.
  ( ShelleyTest era,
    ExMock (Crypto era) hc,
    PreAlonzo era,
    EraSegWits era,
    PParams era ~ ShelleyPParams era
  ) =>
  CHAINExample (BHeader (Crypto era) hc) era hc
exEmptyBlock = CHAINExample (initStEx1 @era @hc) (blockEx1 @era @hc) (Right (expectedStEx1 @era @hc))
