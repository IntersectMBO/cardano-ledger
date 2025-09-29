{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Example demonstrating the adoption of of pool parameters
-- when re-registratiing.
module Test.Cardano.Ledger.Shelley.Examples.PoolReReg (
  poolReRegExample,
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Globals (..),
  Nonce,
  StrictMaybe (..),
 )
import Cardano.Ledger.Block (Block (blockHeader))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Shelley (ShelleyEra, Tx (..), TxBody (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (PulsingRewUpdate, emptyRewardUpdate)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (addrWits)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.State (PoolParams (..), SnapShot (ssPoolParams), UTxO (..), emptySnapShot)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import Test.Cardano.Ledger.Shelley.Examples.Chain (CHAINExample (..), testCHAINExample)
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

initUTxO :: UTxO ShelleyEra
initUTxO = genesisCoins genesisId [ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin)]

initStPoolReReg :: ChainState ShelleyEra
initStPoolReReg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

feeTx1 :: Coin
feeTx1 = Coin 3

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> Coin 250 <-> feeTx1

txbodyEx1 :: TxBody ShelleyEra
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    (StrictSeq.fromList [RegPoolTxCert Cast.alicePoolParams])
    (Withdrawals Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: ShelleyTx ShelleyEra
txEx1 =
  ShelleyTx
    txbodyEx1
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx1)
            ( [asWitness $ Cast.alicePay]
                <> [asWitness $ Cast.aliceStake]
                <> [asWitness $ aikCold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx1 :: HasCallStack => Block (BHeader MockCrypto) ShelleyEra
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @ShelleyEra ppEx 10)
    [MkShelleyTx txEx1]
    (SlotNo 10)
    (BlockNo 1)
    nonce0
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: ChainState ShelleyEra
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce blockEx1)
    . C.newLab blockEx1
    . C.addFees feeTx1
    . C.newUTxO txbodyEx1
    . C.regPool Cast.alicePoolParams
    $ initStPoolReReg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block Alice registers a stake pool.
poolReReg1 :: CHAINExample ShelleyEra
poolReReg1 = CHAINExample initStPoolReReg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 20, Epoch 0
--

feeTx2 :: Coin
feeTx2 = Coin 3

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

newPoolParams :: PoolParams
newPoolParams = Cast.alicePoolParams {ppCost = Coin 500}

txbodyEx2 :: TxBody ShelleyEra
txbodyEx2 =
  ShelleyTxBody
    (Set.fromList [TxIn (txIdTxBody txbodyEx1) minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx2)])
    ( StrictSeq.fromList
        ( [ RegPoolTxCert newPoolParams
          ]
        )
    )
    (Withdrawals Map.empty)
    feeTx2
    (SlotNo 100)
    SNothing
    SNothing

txEx2 :: ShelleyTx ShelleyEra
txEx2 =
  ShelleyTx
    txbodyEx2
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx2)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ aikCold Cast.alicePoolKeys]
            )
      }
    SNothing

word64SlotToKesPeriodWord :: Word64 -> Word
word64SlotToKesPeriodWord slot =
  fromIntegral (toInteger slot) `div` fromIntegral (toInteger $ slotsPerKESPeriod testGlobals)

blockEx2 :: Word64 -> Block (BHeader MockCrypto) ShelleyEra
blockEx2 slot =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx1)
    (coreNodeKeysBySchedule @ShelleyEra ppEx slot)
    [MkShelleyTx txEx2]
    (SlotNo slot)
    (BlockNo 2)
    nonce0
    (NatNonce 2)
    minBound
    (word64SlotToKesPeriodWord slot)
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 20) 0 (KESPeriod 0))

blockEx2A :: Block (BHeader MockCrypto) ShelleyEra
blockEx2A = blockEx2 20

expectedStEx2 :: ChainState ShelleyEra
expectedStEx2 =
  C.addFees feeTx2
    . C.newUTxO txbodyEx2
    . C.regPool newPoolParams
    $ expectedStEx1

expectedStEx2A :: ChainState ShelleyEra
expectedStEx2A =
  C.evolveNonceUnfrozen (getBlockNonce blockEx2A)
    . C.newLab blockEx2A
    $ expectedStEx2

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- early in the epoch.
poolReReg2A :: CHAINExample ShelleyEra
poolReReg2A = CHAINExample expectedStEx1 blockEx2A (Right expectedStEx2A)

pulserEx2 :: PulsingRewUpdate
pulserEx2 = makeCompletedPulser (BlocksMade mempty) expectedStEx2

expectedStEx2B :: ChainState ShelleyEra
expectedStEx2B =
  C.evolveNonceFrozen (getBlockNonce blockEx2B)
    . C.newLab blockEx2B
    . C.pulserUpdate pulserEx2
    $ expectedStEx2

blockEx2B :: Block (BHeader MockCrypto) ShelleyEra
blockEx2B = blockEx2 90

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice re-registers with new pool parameters
-- late in the epoch.
poolReReg2B :: CHAINExample ShelleyEra
poolReReg2B = CHAINExample expectedStEx1 blockEx2B (Right (C.solidifyProposals expectedStEx2B))

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: Nonce
epoch1Nonce = chainCandidateNonce expectedStEx2B

blockEx3 :: Block (BHeader MockCrypto) ShelleyEra
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx2B)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    epoch1Nonce
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 110) 0 (KESPeriod 0))

snapEx3 :: SnapShot
snapEx3 =
  emptySnapShot {ssPoolParams = [(aikColdKeyHash Cast.alicePoolKeys, Cast.alicePoolParams)]}

expectedStEx3 :: ChainState ShelleyEra
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
poolReReg3 :: CHAINExample ShelleyEra
poolReReg3 = CHAINExample expectedStEx2B blockEx3 (Right expectedStEx3)

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
