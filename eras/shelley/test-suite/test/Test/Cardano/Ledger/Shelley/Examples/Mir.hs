{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Mir
-- Description : MIR Example
--
-- Example demonstrating the Move Instantaneous Rewards mechanism
module Test.Cardano.Ledger.Shelley.Examples.Mir (
  mirExample,
) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Nonce, StrictMaybe (..), mkCertIxPartial)
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..), toDeltaCoin)
import Cardano.Ledger.Credential (Ptr (..), SlotNo32 (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Shelley (ShelleyEra, Tx (..), TxBody (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
  PulsingRewUpdate,
  emptyRewardUpdate,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure (..),
  ShelleyUtxowPredFailure (..),
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (addrWits)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation (
  coreNodeIssuerKeys,
  coreNodeKeysBySchedule,
 )
import Test.Cardano.Ledger.Shelley.Examples.Init (
  initSt,
  lastByronHeaderHash,
  nonce0,
  ppEx,
 )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makePulser')
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  NatNonce (..),
  genesisCoins,
  mkBlockFakeVRF,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..), TestChainPredicateFailure (..))
import Test.Cardano.Ledger.Shelley.Utils (getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

initUTxO :: UTxO ShelleyEra
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr aliceInitCoin
    , ShelleyTxOut Cast.bobAddr bobInitCoin
    ]
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStMIR :: Coin -> ChainState ShelleyEra
initStMIR treasury = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    cs = initSt @ShelleyEra initUTxO
    chainAccountState = esChainAccountState . nesEs $ chainNes cs
    chainAccountState' =
      ChainAccountState
        { casTreasury = casTreasury chainAccountState <+> treasury
        , casReserves = casReserves chainAccountState <-> treasury
        }
    es' = (nesEs $ chainNes cs) {esChainAccountState = chainAccountState'}

--
-- Block 1, Slot 10, Epoch 0
--

aliceMIRCoin :: Coin
aliceMIRCoin = Coin 100

ir :: MIRTarget
ir = StakeAddressesMIR $ Map.fromList [(Cast.aliceSHK, toDeltaCoin aliceMIRCoin)]

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: MIRPot -> TxBody ShelleyEra
txbodyEx1 pot =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ ShelleyTxCertMir (MIRCert pot ir)
        , RegTxCert Cast.aliceSHK
        ]
    )
    (Withdrawals Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    aliceCoinEx1 = aliceInitCoin <-> Val.inject (feeTx1 <+> Coin 7)

mirWits :: [Int] -> [KeyPair 'Witness]
mirWits = map (asWitness . aikCold . coreNodeIssuerKeys)

sufficientMIRWits :: [KeyPair 'Witness]
sufficientMIRWits = mirWits [0 .. 4]

insufficientMIRWits :: [KeyPair 'Witness]
insufficientMIRWits = mirWits [0 .. 3]

txEx1 :: [KeyPair 'Witness] -> MIRPot -> ShelleyTx ShelleyEra
txEx1 txwits pot =
  ShelleyTx
    (txbodyEx1 pot)
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated $ txbodyEx1 pot)
            ([asWitness Cast.alicePay] <> txwits)
      }
    SNothing

blockEx1' :: [KeyPair 'Witness] -> MIRPot -> Block (BHeader MockCrypto) ShelleyEra
blockEx1' txwits pot =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @ShelleyEra ppEx 10)
    [MkShelleyTx $ txEx1 txwits pot]
    (SlotNo 10)
    (BlockNo 1)
    nonce0
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 10) 0 (KESPeriod 0))

blockEx1 :: MIRPot -> Block (BHeader MockCrypto) ShelleyEra
blockEx1 = blockEx1' sufficientMIRWits

expectedStEx1' :: [KeyPair 'Witness] -> MIRPot -> ChainState ShelleyEra
expectedStEx1' txwits pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1' txwits pot))
    . C.newLab (blockEx1' txwits pot)
    . C.feesAndDeposits ppEx feeTx1 [Cast.aliceSHK] []
    . C.newUTxO (txbodyEx1 pot)
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo32 10) minBound (mkCertIxPartial 1))
    . C.mir Cast.aliceSHK pot aliceMIRCoin
    $ initStMIR (Coin 1000)

expectedStEx1 :: MIRPot -> ChainState ShelleyEra
expectedStEx1 = expectedStEx1' sufficientMIRWits

-- === Block 1, Slot 10, Epoch 0, Successful MIR Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mir1 :: MIRPot -> CHAINExample ShelleyEra
mir1 pot =
  CHAINExample
    (initStMIR (Coin 1000))
    (blockEx1 pot)
    (Right $ expectedStEx1 pot)

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR Wits, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailWits :: MIRPot -> CHAINExample ShelleyEra
mirFailWits pot =
  CHAINExample
    (initStMIR (Coin 1000))
    (blockEx1' insufficientMIRWits pot)
    ( Left
        . pure
        . BbodyFailure
        . injectFailure
        $ MIRInsufficientGenesisSigsUTXOW ws
    )
  where
    ws = Set.fromList $ map (asWitness . aikColdKeyHash . coreNodeIssuerKeys) [0 .. 3]

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR funds, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailFunds ::
  MIRPot ->
  Coin ->
  Coin ->
  Coin ->
  CHAINExample ShelleyEra
mirFailFunds pot treasury llNeeded llReceived =
  CHAINExample
    (initStMIR treasury)
    (blockEx1' sufficientMIRWits pot)
    ( Left
        . pure
        . BbodyFailure
        . injectFailure
        $ InsufficientForInstantaneousRewardsDELEG pot
        $ Mismatch llNeeded llReceived
    )

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 :: MIRPot -> Block (BHeader MockCrypto) ShelleyEra
blockEx2 pot =
  mkBlockFakeVRF
    (bhHash $ bheader (blockEx1 pot))
    (coreNodeKeysBySchedule @ShelleyEra ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    nonce0
    (NatNonce 2)
    minBound
    2
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 50) 0 (KESPeriod 0))

pulserEx2 :: MIRPot -> PulsingRewUpdate
pulserEx2 pot = makePulser' (expectedStEx1 pot)

expectedStEx2 :: MIRPot -> ChainState ShelleyEra
expectedStEx2 pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 pot))
    . C.newLab (blockEx2 pot)
    . C.pulserUpdate (pulserEx2 pot)
    $ expectedStEx1 pot

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to create an empty reward update.
mir2 :: MIRPot -> CHAINExample ShelleyEra
mir2 pot =
  CHAINExample
    (expectedStEx1 pot)
    (blockEx2 pot)
    (Right $ C.solidifyProposals (expectedStEx2 pot))

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: MIRPot -> Nonce
epoch1Nonce pot = chainCandidateNonce (expectedStEx2 pot)

blockEx3 :: MIRPot -> Block (BHeader MockCrypto) ShelleyEra
blockEx3 pot =
  mkBlockFakeVRF
    (bhHash $ bheader (blockEx2 pot))
    (coreNodeKeysBySchedule @ShelleyEra ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce pot)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 110) 0 (KESPeriod 0))

expectedStEx3 :: MIRPot -> ChainState ShelleyEra
expectedStEx3 pot =
  C.newEpoch (blockEx3 pot)
    . C.newSnapshot emptySnapShot feeTx1
    . C.applyRewardUpdate emptyRewardUpdate
    . C.applyMIR pot (Map.singleton Cast.aliceSHK aliceMIRCoin)
    $ expectedStEx2 pot

-- === Block 3, Slot 110, Epoch 1
--
-- Submit an empty block in the next epoch to apply the MIR rewards.
mir3 :: MIRPot -> CHAINExample ShelleyEra
mir3 pot = CHAINExample (expectedStEx2 pot) (blockEx3 pot) (Right $ expectedStEx3 pot)

--
-- MIR Test Group
--

mirExample :: TestTree
mirExample =
  testGroup
    "move inst rewards"
    [ testCase "create MIR cert - reserves" $ testCHAINExample (mir1 ReservesMIR)
    , testCase "create MIR cert - treasury" $ testCHAINExample (mir1 TreasuryMIR)
    , testCase "insufficient MIR witnesses, reserves" $
        testCHAINExample (mirFailWits ReservesMIR)
    , testCase "insufficient MIR witnesses, treasury" $
        testCHAINExample (mirFailWits TreasuryMIR)
    , testCase "insufficient MIR funds, reserves" $
        testCHAINExample (mirFailFunds ReservesMIR (Coin 34000000000000000) (Coin 100) (Coin 0))
    , testCase "insufficient MIR funds, treasury" $
        testCHAINExample (mirFailFunds TreasuryMIR (Coin 99) (Coin 100) (Coin 99))
    , testCase "end of epoch after MIR - reserves" $
        testCHAINExample (mir2 ReservesMIR)
    , testCase "end of epoch after MIR - treasury" $
        testCHAINExample (mir2 TreasuryMIR)
    , testCase "apply MIR - reserves" $ testCHAINExample (mir3 ReservesMIR)
    , testCase "apply MIR - treasury" $ testCHAINExample (mir3 TreasuryMIR)
    ]
