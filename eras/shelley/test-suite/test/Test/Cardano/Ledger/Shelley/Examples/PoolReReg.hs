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
module Test.Cardano.Ledger.Shelley.Examples.PoolReReg
  ( poolReRegExample,
  )
where

import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Globals (..),
    Nonce,
    StrictMaybe (..),
  )
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as Cr
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.EpochBoundary (SnapShot (_poolParams), emptySnapShot)
import Cardano.Ledger.Shelley.LedgerState (PulsingRewUpdate, emptyRewardUpdate)
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    PoolCert (..),
    PoolParams (..),
    TxBody (..),
    TxOut (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessesVKey)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation (coreNodeKeysBySchedule)
import Test.Cardano.Ledger.Shelley.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makeCompletedPulser)
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( AllIssuerKeys (..),
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
initUTxO = genesisCoins genesisId [TxOut Cast.aliceAddr (Val.inject aliceInitCoin)]

initStPoolReReg :: Cr.Crypto c => ChainState (ShelleyEra c)
initStPoolReReg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

feeTx1 :: Coin
feeTx1 = Coin 3

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> _poolDeposit ppEx <-> feeTx1

txbodyEx1 :: Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    (StrictSeq.fromList ([DCertPool (RegPool Cast.alicePoolParams)]))
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: forall c. (Cr.Crypto c, ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @c)
            ( [asWitness $ Cast.alicePay]
                <> [asWitness $ Cast.aliceStake]
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx1 ::
  forall c.
  (HasCallStack, Cr.Crypto c, ExMock (Crypto (ShelleyEra c))) =>
  Block (BHeader c) (ShelleyEra c)
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

expectedStEx1 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  ChainState (ShelleyEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @c))
    . C.newLab blockEx1
    . C.feesAndDeposits feeTx1 (_poolDeposit ppEx)
    . C.newUTxO txbodyEx1
    . C.newPool Cast.alicePoolParams
    $ initStPoolReReg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block Alice registers a stake pool.
poolReReg1 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample (BHeader c) (ShelleyEra c)
poolReReg1 = CHAINExample initStPoolReReg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 20, Epoch 0
--

feeTx2 :: Coin
feeTx2 = Coin 3

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

newPoolParams :: Cr.Crypto c => PoolParams c
newPoolParams = Cast.alicePoolParams {_poolCost = Coin 500}

txbodyEx2 :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx2 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx1) 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr (Val.inject aliceCoinEx2)])
    ( StrictSeq.fromList
        ( [ DCertPool (RegPool newPoolParams)
          ]
        )
    )
    (Wdrl Map.empty)
    feeTx2
    (SlotNo 100)
    SNothing
    SNothing

txEx2 :: forall c. (Cr.Crypto c, ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx2 =
  Tx
    txbodyEx2
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx2 @c)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

word64SlotToKesPeriodWord :: Word64 -> Word
word64SlotToKesPeriodWord slot =
  (fromIntegral $ toInteger slot) `div` (fromIntegral $ toInteger $ slotsPerKESPeriod testGlobals)

blockEx2 :: forall c. (Cr.Crypto c, ExMock (Crypto (ShelleyEra c))) => Word64 -> Block (BHeader c) (ShelleyEra c)
blockEx2 slot =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c) @(ShelleyEra c) blockEx1)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx slot)
    [txEx2]
    (SlotNo slot)
    (BlockNo 2)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 2)
    minBound
    (word64SlotToKesPeriodWord slot)
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 20) 0 (KESPeriod 0))

blockEx2A :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block (BHeader c) (ShelleyEra c)
blockEx2A = blockEx2 20

expectedStEx2 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx2 =
  C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO txbodyEx2
    . C.reregPool newPoolParams
    $ expectedStEx1

expectedStEx2A :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx2A =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2A @c))
    . C.newLab blockEx2A
    $ expectedStEx2

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- early in the epoch.
poolReReg2A :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample (BHeader c) (ShelleyEra c)
poolReReg2A = CHAINExample expectedStEx1 blockEx2A (Right expectedStEx2A)

pulserEx2 :: forall c. (ExMock c) => PulsingRewUpdate c
pulserEx2 = makeCompletedPulser (BlocksMade mempty) expectedStEx2

expectedStEx2B :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx2B =
  C.evolveNonceFrozen (getBlockNonce (blockEx2B @c))
    . C.newLab blockEx2B
    . C.pulserUpdate pulserEx2
    $ expectedStEx2

blockEx2B :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block (BHeader c) (ShelleyEra c)
blockEx2B = blockEx2 90

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- late in the epoch.
poolReReg2B :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample (BHeader c) (ShelleyEra c)
poolReReg2B = CHAINExample expectedStEx1 blockEx2B (Right expectedStEx2B)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: forall c. (ExMock (Crypto (ShelleyEra c))) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2B @c)

blockEx3 :: forall c. (Cr.Crypto c, ExMock (Crypto (ShelleyEra c))) => Block (BHeader c) (ShelleyEra c)
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c) @(ShelleyEra c) blockEx2B)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

snapEx3 :: Cr.Crypto c => SnapShot c
snapEx3 =
  emptySnapShot {_poolParams = [(hk Cast.alicePoolKeys, Cast.alicePoolParams)]}

expectedStEx3 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx3 =
  C.newEpoch blockEx3
    . C.newSnapshot snapEx3 (feeTx1 <+> feeTx2)
    . C.applyRewardUpdate emptyRewardUpdate
    . C.updatePoolParams newPoolParams
    $ expectedStEx2B

-- === Block 3, Slot 110, Epoch 1
--
-- The third block is empty and trigger the epoch change,
-- and Alice's new pool parameters are adopted.
poolReReg3 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample (BHeader c) (ShelleyEra c)
poolReReg3 = CHAINExample expectedStEx2B blockEx3 (Right expectedStEx3)

--
-- Pool Lifetime Test Group
--

poolReRegExample :: TestTree
poolReRegExample =
  testGroup
    "pool rereg"
    [ testCase "initial pool registration" $ testCHAINExample poolReReg1,
      testCase "early epoch re-registration" $ testCHAINExample poolReReg2A,
      testCase "late epoch re-registration" $ testCHAINExample poolReReg2B,
      testCase "adopt new pool parameters" $ testCHAINExample poolReReg3
    ]
