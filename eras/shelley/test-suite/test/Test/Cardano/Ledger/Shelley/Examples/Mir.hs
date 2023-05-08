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
)
where

import Cardano.Ledger.BaseTypes (Nonce, StrictMaybe (..), mkCertIxPartial)
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..), toDeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.EpochBoundary (emptySnapShot)
import Cardano.Ledger.Keys (
  KeyRole (..),
  asWitness,
 )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Delegation.Certificates (DelegCert (..), MIRCert (..))
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  NewEpochState (..),
  PulsingRewUpdate,
  emptyRewardUpdate,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure (..),
  ShelleyDelegPredFailure (..),
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersPredFailure (..),
  ShelleyUtxowPredFailure (..),
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  MIRPot (..),
  MIRTarget (..),
  ShelleyTxBody (..),
  ShelleyTxOut (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxWits (addrWits)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.HeaderCrypto (HeaderCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock, Mock)
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

initUTxO :: Crypto c => UTxO (ShelleyEra c)
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr aliceInitCoin
    , ShelleyTxOut Cast.bobAddr bobInitCoin
    ]
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStMIR ::
  forall c hc.
  (Crypto c, HeaderCrypto hc) =>
  Proxy hc ->
  Coin ->
  ChainState (ShelleyEra c)
initStMIR _ treasury = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    cs = initSt (Proxy @c) (Proxy @hc) initUTxO
    as = esAccountState . nesEs . chainNes $ cs
    as' =
      as
        { asTreasury = asTreasury as <+> treasury
        , asReserves = asReserves as <-> treasury
        }
    es' = (nesEs $ chainNes cs) {esAccountState = as'}

--
-- Block 1, Slot 10, Epoch 0
--

aliceMIRCoin :: Coin
aliceMIRCoin = Coin 100

ir :: Crypto c => MIRTarget c
ir = StakeAddressesMIR $ Map.fromList [(Cast.aliceSHK, toDeltaCoin aliceMIRCoin)]

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: Crypto c => MIRPot -> TxBody (ShelleyEra c)
txbodyEx1 pot =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertMir (MIRCert pot ir)
        , DCertDeleg (RegKey Cast.aliceSHK)
        ]
    )
    (Withdrawals Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    aliceCoinEx1 = aliceInitCoin <-> (Val.inject $ feeTx1 <+> Coin 7)

mirWits :: forall c hc. (Crypto c, HeaderCrypto hc) => [Int] -> [KeyPair 'Witness c]
mirWits = map (asWitness . aikCold . coreNodeIssuerKeys (Proxy @c) (Proxy @hc))

sufficientMIRWits :: forall c hc. (Crypto c, HeaderCrypto hc) => [KeyPair 'Witness c]
sufficientMIRWits = mirWits @c @hc [0 .. 4]

insufficientMIRWits ::
  forall c hc.
  (Crypto c, HeaderCrypto hc) =>
  [KeyPair 'Witness c]
insufficientMIRWits = mirWits @c @hc [0 .. 3]

txEx1 ::
  forall c hc.
  (Mock c hc) =>
  [KeyPair 'Witness c] ->
  MIRPot ->
  ShelleyTx (ShelleyEra c)
txEx1 txwits pot =
  ShelleyTx
    (txbodyEx1 pot)
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated $ txbodyEx1 @c pot)
            ([asWitness Cast.alicePay] <> txwits)
      }
    SNothing

blockEx1' ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  [KeyPair 'Witness (EraCrypto (ShelleyEra c))] ->
  MIRPot ->
  Block (BHeader c hc) (ShelleyEra c)
blockEx1' txwits pot =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1 @c @hc txwits pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy @c))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

blockEx1 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  Block (BHeader c hc) (ShelleyEra c)
blockEx1 = blockEx1' (sufficientMIRWits @c @hc)

expectedStEx1' ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  [KeyPair 'Witness (EraCrypto (ShelleyEra c))] ->
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx1' txwits pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1' @c @hc txwits pot))
    . C.newLab (blockEx1 @c @hc pot)
    . C.feesAndDeposits ppEx feeTx1 [Cast.aliceSHK] []
    . C.newUTxO (txbodyEx1 pot)
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 1))
    . C.mir Cast.aliceSHK pot aliceMIRCoin
    $ initStMIR (Proxy @hc) (Coin 1000)

expectedStEx1 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx1 = expectedStEx1' @c @hc (sufficientMIRWits @c @hc)

-- === Block 1, Slot 10, Epoch 0, Successful MIR Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mir1 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
mir1 pot =
  CHAINExample
    (initStMIR (Proxy @hc) (Coin 1000))
    (blockEx1 pot)
    (Right $ expectedStEx1 @c @hc pot)

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR Wits, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailWits ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
mirFailWits pot =
  CHAINExample
    (initStMIR (Proxy @hc) (Coin 1000))
    (blockEx1' (insufficientMIRWits @c @hc) pot)
    ( Left
        [ BbodyFailure @(ShelleyEra c)
            ( LedgersFailure
                ( LedgerFailure
                    ( UtxowFailure $
                        MIRInsufficientGenesisSigsUTXOW ws
                    )
                )
            )
        ]
    )
  where
    ws = Set.fromList $ map (asWitness . aikColdKeyHash . coreNodeIssuerKeys (Proxy @c) (Proxy @hc)) [0 .. 3]

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR funds, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailFunds ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  Coin ->
  Coin ->
  Coin ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
mirFailFunds pot treasury llNeeded llReceived =
  CHAINExample
    (initStMIR (Proxy @hc) treasury)
    (blockEx1' (sufficientMIRWits @c @hc) pot)
    ( Left
        [ BbodyFailure
            ( LedgersFailure
                ( LedgerFailure
                    ( DelegsFailure
                        ( DelplFailure
                            ( DelegFailure $
                                InsufficientForInstantaneousRewardsDELEG
                                  pot
                                  llNeeded
                                  llReceived
                            )
                        )
                    )
                )
            )
        ]
    )

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  Block (BHeader c hc) (ShelleyEra c)
blockEx2 pot =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) (blockEx1 pot))
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 (Proxy @c))
    (NatNonce 2)
    minBound
    2
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 50) 0 (KESPeriod 0))

pulserEx2 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  PulsingRewUpdate c
pulserEx2 pot = makePulser' (expectedStEx1 @c @hc pot)

expectedStEx2 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx2 pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @c @hc pot))
    . C.newLab (blockEx2 @c @hc pot)
    . C.pulserUpdate (pulserEx2 @c @hc pot)
    $ (expectedStEx1 @c @hc pot)

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to create an empty reward update.
mir2 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
mir2 pot =
  CHAINExample
    (expectedStEx1 @c @hc pot)
    (blockEx2 pot)
    (Right $ expectedStEx2 @c @hc pot)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  Nonce
epoch1Nonce pot = chainCandidateNonce (expectedStEx2 @c @hc pot)

blockEx3 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  Block (BHeader c hc) (ShelleyEra c)
blockEx3 pot =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) (blockEx2 pot))
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c @hc pot)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

expectedStEx3 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx3 pot =
  C.newEpoch (blockEx3 @c @hc pot)
    . C.newSnapshot emptySnapShot feeTx1
    . C.applyRewardUpdate emptyRewardUpdate
    . C.applyMIR pot (Map.singleton Cast.aliceSHK aliceMIRCoin)
    $ (expectedStEx2 @c @hc pot)

-- === Block 3, Slot 110, Epoch 1
--
-- Submit an empty block in the next epoch to apply the MIR rewards.
mir3 ::
  forall c hc.
  (ExMock (EraCrypto (ShelleyEra c)) hc) =>
  MIRPot ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
mir3 pot = CHAINExample (expectedStEx2 @c @hc pot) (blockEx3 pot) (Right $ expectedStEx3 @c @hc pot)

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
