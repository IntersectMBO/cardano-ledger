{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Examples.EmptyBlock (
  exEmptyBlock,
)
where

import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Protocol.HeaderCrypto (HeaderCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Data.Default.Class
import Data.Proxy
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..))
import Test.Cardano.Ledger.Shelley.Examples.Combinators (
  evolveNonceUnfrozen,
  newLab,
 )
import Test.Cardano.Ledger.Shelley.Examples.Federation (coreNodeKeysBySchedule)
import Test.Cardano.Ledger.Shelley.Examples.Init (
  initSt,
  lastByronHeaderHash,
  nonce0,
  ppEx,
 )
import Test.Cardano.Ledger.Shelley.Generator.Core (
  NatNonce (..),
  mkBlockFakeVRF,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (getBlockNonce)

-- =============================================================

initStEx1 ::
  ( EraTxOut era
  , HeaderCrypto hc
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , Default (StashedAVVMAddresses era)
  , EraGovernance era
  ) =>
  Proxy (EraCrypto era) ->
  Proxy hc ->
  ChainState era
initStEx1 p q = initSt p q (UTxO mempty)

blockEx1 ::
  forall era hc c.
  ( HasCallStack
  , EraSegWits era
  , ExMock (EraCrypto era) hc
  , c ~ EraCrypto era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  ) =>
  Block (BHeader c hc) era
blockEx1 =
  mkBlockFakeVRF @era @hc
    lastByronHeaderHash
    -- (coreNodeKeysBySchedule p q ppEx 10)
    keys
    []
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy @c))
    (NatNonce 1)
    minBound
    0
    0
    -- (mkOCert @(EraCrypto era) @hc (coreNodeKeysBySchedule p q ppEx 10) 0 (KESPeriod 0))
    (mkOCert keys 0 (KESPeriod 0))
  where
    keys = coreNodeKeysBySchedule @era @hc @c ppEx 10

blockNonce ::
  forall era hc.
  ( HasCallStack
  , EraSegWits era
  , ExMock (EraCrypto era) hc
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  ) =>
  Nonce
blockNonce = getBlockNonce (blockEx1 @era @hc)

expectedStEx1 ::
  forall era hc.
  ( EraSegWits era
  , ExMock (EraCrypto era) hc
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , EraGovernance era
  , Default (StashedAVVMAddresses era)
  ) =>
  ChainState era
expectedStEx1 = evolveNonceUnfrozen (blockNonce @era @hc) . newLab (blockEx1 @era @hc) $ initStEx1 (Proxy @(EraCrypto era)) (Proxy @hc)

-- | = Empty Block Example
--
-- This is the most minimal example of using the CHAIN STS transition.
-- It applies an empty block to an initial shelley chain state.
--
-- The only things that change in the chain state are the
-- evolving and candidate nonces, and the last applied block.
exEmptyBlock ::
  forall era hc.
  ( ExMock (EraCrypto era) hc
  , EraSegWits era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , Default (StashedAVVMAddresses era)
  , EraGovernance era
  ) =>
  CHAINExample (BHeader (EraCrypto era) hc) era hc
exEmptyBlock = CHAINExample (initStEx1 (Proxy @(EraCrypto era)) (Proxy @hc)) blockEx1 (Right $ expectedStEx1 @era @hc)
