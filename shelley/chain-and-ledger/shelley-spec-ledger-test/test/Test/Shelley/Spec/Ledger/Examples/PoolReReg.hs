{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.PoolReReg
-- Description : Pool Re-Registration
--
-- Example demonstrating the adoption of of pool parameters
-- when re-registratiing.
module Test.Shelley.Spec.Ledger.Examples.PoolReReg
  ( poolReRegExample,
  )
where

import Cardano.Ledger.Era (Crypto (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Nonce,
    StrictMaybe (..),
  )
import Shelley.Spec.Ledger.BlockChain (Block, bhHash, bheader)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (SnapShot (_poolParams), emptySnapShot)
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.LedgerState (emptyRewardUpdate)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    PoolCert (..),
    PoolParams (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import qualified Cardano.Ledger.Val as Val
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ExMock)
import Test.Shelley.Spec.Ledger.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import qualified Test.Shelley.Spec.Ledger.Examples.Combinators as C
import Test.Shelley.Spec.Ledger.Examples.Federation (coreNodeKeysBySchedule)
import Test.Shelley.Spec.Ledger.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    NatNonce (..),
    genesisCoins,
    genesisId,
    mkBlockFakeVRF,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils (getBlockNonce, testGlobals)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Era era => UTxO era
initUTxO = genesisCoins [TxOut Cast.aliceAddr aliceInitCoin]

initStPoolReReg :: forall era. Era era => ChainState era
initStPoolReReg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

feeTx1 :: Coin
feeTx1 = Coin 3

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin Val.~~ _poolDeposit ppEx Val.~~ feeTx1

txbodyEx1 :: Era era => TxBody era
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr aliceCoinEx1])
    (StrictSeq.fromList ([DCertPool (RegPool Cast.alicePoolParams)]))
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: forall era. (Era era, ExMock (Crypto era)) => Tx era
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx1)
            ( [asWitness $ Cast.alicePay]
                <> [asWitness $ Cast.aliceStake]
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx1 :: forall era. (HasCallStack, Era era, ExMock (Crypto era)) => Block era
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @era)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @era))
    . C.newLab blockEx1
    . C.feesAndDeposits feeTx1 (_poolDeposit ppEx)
    . C.newUTxO txbodyEx1
    . C.newPool Cast.alicePoolParams
    $ initStPoolReReg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block Alice registers a stake pool.
poolReReg1 :: (Era era, ExMock (Crypto era)) => CHAINExample era
poolReReg1 = CHAINExample initStPoolReReg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 20, Epoch 0
--

feeTx2 :: Coin
feeTx2 = Coin 3

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 Val.~~ feeTx2

newPoolParams :: Era era => PoolParams era
newPoolParams = Cast.alicePoolParams {_poolCost = Coin 500}

txbodyEx2 :: Era era => TxBody era
txbodyEx2 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx1) 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr aliceCoinEx2])
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

txEx2 :: (Era era, ExMock (Crypto era)) => Tx era
txEx2 =
  Tx
    txbodyEx2
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx2)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

word64SlotToKesPeriodWord :: Word64 -> Word
word64SlotToKesPeriodWord slot =
  (fromIntegral $ toInteger slot) `div` (fromIntegral $ toInteger $ slotsPerKESPeriod testGlobals)

blockEx2 :: forall era. (Era era, ExMock (Crypto era)) => Word64 -> Block era
blockEx2 slot =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule ppEx slot)
    [txEx2]
    (SlotNo slot)
    (BlockNo 2)
    (nonce0 @era)
    (NatNonce 2)
    zero
    (word64SlotToKesPeriodWord slot)
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 20) 0 (KESPeriod 0))

blockEx2A :: forall era. (Era era, ExMock (Crypto era)) => Block era
blockEx2A = blockEx2 20

expectedStEx2 :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx2 =
  C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO txbodyEx2
    . C.reregPool newPoolParams
    $ expectedStEx1

expectedStEx2A :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx2A =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2A @era))
    . C.newLab blockEx2A
    $ expectedStEx2

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- early in the epoch.
poolReReg2A :: (Era era, ExMock (Crypto era)) => CHAINExample era
poolReReg2A = CHAINExample expectedStEx1 blockEx2A (Right expectedStEx2A)

expectedStEx2B :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx2B =
  C.evolveNonceFrozen (getBlockNonce (blockEx2B @era))
    . C.newLab blockEx2B
    . C.rewardUpdate emptyRewardUpdate
    $ expectedStEx2

blockEx2B :: forall era. (Era era, ExMock (Crypto era)) => Block era
blockEx2B = blockEx2 90

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- late in the epoch.
poolReReg2B :: (Era era, ExMock (Crypto era)) => CHAINExample era
poolReReg2B = CHAINExample expectedStEx1 blockEx2B (Right expectedStEx2B)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: forall era. (Era era, ExMock (Crypto era)) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2B @era)

blockEx3 :: forall era. (Era era, ExMock (Crypto era)) => Block era
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx2B)
    (coreNodeKeysBySchedule ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @era)
    (NatNonce 3)
    zero
    5
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 110) 0 (KESPeriod 0))

snapEx3 :: Era era => SnapShot era
snapEx3 =
  emptySnapShot {_poolParams = Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams}

expectedStEx3 :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx3 =
  C.newEpoch blockEx3
    . C.newSnapshot snapEx3 (feeTx1 <> feeTx2)
    . C.applyRewardUpdate emptyRewardUpdate
    . C.updatePoolParams newPoolParams
    $ expectedStEx2B

-- === Block 3, Slot 110, Epoch 1
--
-- The third block is empty and trigger the epoch change,
-- and Alice's new pool parameters are adopted.
poolReReg3 :: (Era era, ExMock (Crypto era)) => CHAINExample era
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
