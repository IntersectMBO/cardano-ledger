{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.PoolReReg
-- Description : Pool Re-Registration
--
-- Example demonstrating the adoption of of pool parameters
-- when re-registratiing.
module Test.Cardano.Ledger.Shelley.Examples.PoolReReg (
  poolReRegExample,
)
where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Globals (..),
  Nonce,
  StrictMaybe (..),
 )
import Cardano.Ledger.Block (Block, bheader, txid)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as Cr
import Cardano.Ledger.EpochBoundary (SnapShot (ssPoolParams), emptySnapShot)
import Cardano.Ledger.Era (EraCrypto (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (PulsingRewUpdate, emptyRewardUpdate)
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
 )
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  PoolCert (..),
  PoolParams (..),
  ShelleyTxBody (..),
  ShelleyTxOut (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxWits (
  addrWits,
 )
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import qualified Cardano.Protocol.HeaderCrypto as Cr
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation (coreNodeKeysBySchedule)
import Test.Cardano.Ledger.Shelley.Examples.Init (
  initSt,
  lastByronHeaderHash,
  nonce0,
  ppEx,
 )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makeCompletedPulser)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  NatNonce (..),
  genesisCoins,
  mkBlockFakeVRF,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (getBlockNonce, testGlobals)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Cr.Crypto c => UTxO (ShelleyEra c)
initUTxO = genesisCoins genesisId [ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin)]

initStPoolReReg :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ChainState (ShelleyEra c)
initStPoolReReg = initSt (Proxy @c) (Proxy @hc) initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

feeTx1 :: Coin
feeTx1 = Coin 3

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> Coin 250 <-> feeTx1

txbodyEx1 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    (StrictSeq.fromList [DCertPool (RegPool $ Cast.alicePoolParams (Proxy @c) (Proxy @hc))])
    (Withdrawals Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: forall c hc. (Cr.Crypto c, ExMock (EraCrypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx1 =
  ShelleyTx
    (txbodyEx1 @c @hc)
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated $ txbodyEx1 @c @hc)
            ( [asWitness $ Cast.alicePay]
                <> [asWitness $ Cast.aliceStake]
                <> [asWitness $ aikCold $ Cast.alicePoolKeys (Proxy @c) (Proxy @hc)]
            )
      }
    SNothing

blockEx1 ::
  forall c hc.
  (HasCallStack, ExMock (EraCrypto (ShelleyEra c)) hc) =>
  Block (BHeader c hc) (ShelleyEra c)
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1 @c @hc]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy @c))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

expectedStEx1 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  ChainState (ShelleyEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @c @hc))
    . C.newLab (blockEx1 @c @hc)
    . C.feesAndDeposits ppEx feeTx1 [] [alicePoolParams']
    . C.newUTxO (txbodyEx1 @c @hc)
    . C.newPool alicePoolParams'
    $ (initStPoolReReg @c @hc)
  where
    alicePoolParams' = Cast.alicePoolParams (Proxy @c) (Proxy @hc)

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block Alice registers a stake pool.
poolReReg1 :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolReReg1 = CHAINExample (initStPoolReReg @c @hc) blockEx1 (Right $ expectedStEx1 @c @hc)

--
-- Block 2, Slot 20, Epoch 0
--

feeTx2 :: Coin
feeTx2 = Coin 3

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

newPoolParams :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => PoolParams c
newPoolParams = (Cast.alicePoolParams (Proxy @c) (Proxy @hc)) {ppCost = Coin 500}

txbodyEx2 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx2 =
  ShelleyTxBody
    (Set.fromList [TxIn (txid $ txbodyEx1 @c @hc) minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx2)])
    ( StrictSeq.fromList
        ( [ DCertPool (RegPool (newPoolParams @c @hc))
          ]
        )
    )
    (Withdrawals Map.empty)
    feeTx2
    (SlotNo 100)
    SNothing
    SNothing

txEx2 :: forall c hc. (ExMock c hc) => ShelleyTx (ShelleyEra c)
txEx2 =
  ShelleyTx
    (txbodyEx2 @c @hc)
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated $ txbodyEx2 @c @hc)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ aikCold $ Cast.alicePoolKeys (Proxy @c) (Proxy @hc)]
            )
      }
    SNothing

word64SlotToKesPeriodWord :: Word64 -> Word
word64SlotToKesPeriodWord slot =
  fromIntegral (toInteger slot) `div` fromIntegral (toInteger $ slotsPerKESPeriod testGlobals)

blockEx2 :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => Word64 -> Block (BHeader c hc) (ShelleyEra c)
blockEx2 slot =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx1)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx slot)
    [txEx2 @c @hc]
    (SlotNo slot)
    (BlockNo 2)
    (nonce0 (Proxy @c))
    (NatNonce 2)
    minBound
    (word64SlotToKesPeriodWord slot)
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 20) 0 (KESPeriod 0))

blockEx2A :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx2A = blockEx2 20

expectedStEx2 :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx2 =
  C.feesAndDeposits ppEx feeTx2 [] [newPoolParams @c @hc] -- The deposit should be ignored because the poolId is already registered
    . C.newUTxO (txbodyEx2 @c @hc)
    . C.reregPool (newPoolParams @c @hc)
    $ (expectedStEx1 @c @hc)

expectedStEx2A :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx2A =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2A @c @hc))
    . C.newLab (blockEx2A @c @hc)
    $ (expectedStEx2 @c @hc)

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- early in the epoch.
poolReReg2A :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolReReg2A = CHAINExample (expectedStEx1 @c @hc) blockEx2A (Right $ expectedStEx2A @c @hc)

pulserEx2 :: forall c hc. (ExMock c hc) => PulsingRewUpdate c
pulserEx2 = makeCompletedPulser (BlocksMade mempty) (expectedStEx2 @c @hc)

expectedStEx2B :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx2B =
  C.evolveNonceFrozen (getBlockNonce (blockEx2B @c @hc))
    . C.newLab (blockEx2B @c @hc)
    . C.pulserUpdate (pulserEx2 @c @hc)
    $ (expectedStEx2 @c @hc)

blockEx2B :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx2B = blockEx2 90

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- late in the epoch.
poolReReg2B :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolReReg2B = CHAINExample (expectedStEx1 @c @hc) blockEx2B (Right $ expectedStEx2B @c @hc)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2B @c @hc)

blockEx3 :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx2B)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c @hc)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

snapEx3 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => SnapShot c
snapEx3 =
  emptySnapShot {ssPoolParams = [(aikColdKeyHash $ Cast.alicePoolKeys pc phc, Cast.alicePoolParams pc phc)]}
  where
    pc = Proxy @c
    phc = Proxy @hc

expectedStEx3 :: forall c hc. (ExMock (EraCrypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx3 =
  C.newEpoch (blockEx3 @c @hc)
    . C.newSnapshot (snapEx3 @c @hc) (feeTx1 <+> feeTx2)
    . C.applyRewardUpdate emptyRewardUpdate
    . C.updatePoolParams (newPoolParams @c @hc)
    $ (expectedStEx2B @c @hc)

-- === Block 3, Slot 110, Epoch 1
--
-- The third block is empty and trigger the epoch change,
-- and Alice's new pool parameters are adopted.
poolReReg3 :: forall c hc. (ExMock c hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolReReg3 = CHAINExample (expectedStEx2B @c @hc) blockEx3 (Right $ expectedStEx3 @c @hc)

--
-- Pool Lifetime Test Group
--

poolReRegExample :: TestTree
poolReRegExample =
  testGroup
    "pool rereg"
    [ testCase "initial pool registration" $ testCHAINExample poolReReg1
    , testCase "early epoch re-registration" $ testCHAINExample poolReReg2A
    , testCase "late epoch re-registration" $ testCHAINExample poolReReg2B
    , testCase "adopt new pool parameters" $ testCHAINExample poolReReg3
    ]
