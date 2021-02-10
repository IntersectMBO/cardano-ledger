{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Mir
-- Description : MIR Example
--
-- Example demonstrating the Move Instantaneous Rewards mechanism
module Test.Shelley.Spec.Ledger.Examples.Mir
  ( mirExample,
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (Nonce, StrictMaybe (..))
import Shelley.Spec.Ledger.BlockChain (Block, bhHash, bheader)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential, Ptr (..))
import Shelley.Spec.Ledger.Delegation.Certificates (DelegCert (..), MIRCert (..))
import Shelley.Spec.Ledger.EpochBoundary (emptySnapShot)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    EpochState (..),
    NewEpochState (..),
    emptyRewardUpdate,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Bbody (BbodyPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Chain (ChainPredicateFailure (..), ChainState (..))
import Shelley.Spec.Ledger.STS.Deleg (DelegPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Delegs (DelegsPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Delpl (DelplPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Ledgers (LedgersPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (Tx (..), WitnessSetHKD (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    MIRPot (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ExMock, Mock)
import Test.Shelley.Spec.Ledger.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import qualified Test.Shelley.Spec.Ledger.Examples.Combinators as C
import Test.Shelley.Spec.Ledger.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
  )
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
    mkBlockFakeVRF,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Generator.EraGen (genesisId)
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest, getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

initUTxO :: (ShelleyTest era) => UTxO era
initUTxO =
  genesisCoins
    genesisId
    [ TxOut Cast.aliceAddr aliceInitCoin,
      TxOut Cast.bobAddr bobInitCoin
    ]
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStMIR :: forall era. ShelleyTest era => Coin -> ChainState era
initStMIR treasury = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    cs = initSt @era initUTxO
    as = esAccountState . nesEs . chainNes $ cs
    as' =
      as
        { _treasury = (_treasury as) <+> treasury,
          _reserves = (_reserves as) <-> treasury
        }
    es' = (nesEs $ chainNes cs) {esAccountState = as'}

--
-- Block 1, Slot 10, Epoch 0
--

aliceMIRCoin :: Coin
aliceMIRCoin = Coin 100

ir :: CryptoClass.Crypto c => Map (Credential 'Staking c) Coin
ir = Map.fromList [(Cast.aliceSHK, aliceMIRCoin)]

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: (ShelleyTest era) => MIRPot -> TxBody era
txbodyEx1 pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertMir (MIRCert pot ir),
          DCertDeleg (RegKey Cast.aliceSHK)
        ]
    )
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    aliceCoinEx1 = aliceInitCoin <-> (Val.inject $ feeTx1 <+> _keyDeposit ppEx)

mirWits :: (CryptoClass.Crypto c) => [Int] -> [KeyPair 'Witness c]
mirWits nodes = asWitness <$> map (\x -> cold . coreNodeIssuerKeys $ x) nodes

sufficientMIRWits :: (CryptoClass.Crypto c) => [KeyPair 'Witness c]
sufficientMIRWits = mirWits [0 .. 4]

insufficientMIRWits :: (CryptoClass.Crypto c) => [KeyPair 'Witness c]
insufficientMIRWits = mirWits [0 .. 3]

txEx1 :: forall c. (Mock c) =>
  [KeyPair 'Witness c] ->
  MIRPot ->
  Tx (ShelleyEra c)
txEx1 wits pot =
  Tx
    (txbodyEx1 pot)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @(ShelleyEra c) pot)
            ([asWitness Cast.alicePay] <> wits)
      }
    SNothing

blockEx1' ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  [KeyPair 'Witness (Crypto (ShelleyEra c))] ->
  MIRPot ->
  Block (ShelleyEra c)
blockEx1' wits pot =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1 wits pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

blockEx1 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  Block (ShelleyEra c)
blockEx1 = blockEx1' sufficientMIRWits

expectedStEx1' ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  [KeyPair 'Witness (Crypto (ShelleyEra c))] ->
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx1' wits pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1' @c wits pot))
    . C.newLab (blockEx1' wits pot)
    . C.feesAndDeposits feeTx1 (_keyDeposit ppEx)
    . C.newUTxO (txbodyEx1 pot)
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) 0 1)
    . C.mir Cast.aliceSHK pot aliceMIRCoin
    $ initStMIR (Coin 1000)

expectedStEx1 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx1 = expectedStEx1' sufficientMIRWits

-- === Block 1, Slot 10, Epoch 0, Successful MIR Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mir1 :: (ExMock (Crypto (ShelleyEra c))) => MIRPot -> CHAINExample (ShelleyEra c)
mir1 pot =
  CHAINExample
    (initStMIR (Coin 1000))
    (blockEx1 pot)
    (Right $ expectedStEx1 pot)

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR Wits, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailWits ::
  forall c.
  ( ExMock (Crypto (ShelleyEra c))
  ) =>
  MIRPot ->
  CHAINExample (ShelleyEra c)
mirFailWits pot =
  CHAINExample
    (initStMIR (Coin 1000))
    (blockEx1' insufficientMIRWits pot)
    ( Left
        [ [ BbodyFailure @(ShelleyEra c)
              ( LedgersFailure
                  ( LedgerFailure
                      ( UtxowFailure $
                          MIRInsufficientGenesisSigsUTXOW ws
                      )
                  )
              )
          ]
        ]
    )
  where
    ws = Set.fromList $ asWitness <$> map (\x -> hk . coreNodeIssuerKeys $ x) [0 .. 3]

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR funds, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailFunds ::
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  Coin ->
  Coin ->
  Coin ->
  CHAINExample (ShelleyEra c)
mirFailFunds pot treasury llNeeded llReceived =
  CHAINExample
    (initStMIR treasury)
    (blockEx1' sufficientMIRWits pot)
    ( Left
        [ [ BbodyFailure
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
        ]
    )

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  Block (ShelleyEra c)
blockEx2 pot =
  mkBlockFakeVRF
    (bhHash $ bheader @(ShelleyEra c) (blockEx1 pot))
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 2)
    zero
    2
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 50) 0 (KESPeriod 0))

expectedStEx2 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx2 pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @c pot))
    . C.newLab (blockEx2 pot)
    . C.rewardUpdate emptyRewardUpdate
    $ (expectedStEx1 pot)

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to create an empty reward update.
mir2 ::
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  CHAINExample (ShelleyEra c)
mir2 pot =
  CHAINExample
    (expectedStEx1 pot)
    (blockEx2 pot)
    (Right $ expectedStEx2 pot)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  Nonce
epoch1Nonce pot = chainCandidateNonce (expectedStEx2 @c pot)

blockEx3 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  Block (ShelleyEra c)
blockEx3 pot =
  mkBlockFakeVRF
    (bhHash $ bheader @(ShelleyEra c) (blockEx2 pot))
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c pot)
    (NatNonce 3)
    zero
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

expectedStEx3 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  MIRPot ->
  ChainState (ShelleyEra c)
expectedStEx3 pot =
  C.newEpoch (blockEx3 pot)
    . C.newSnapshot emptySnapShot feeTx1
    . C.applyRewardUpdate emptyRewardUpdate
    . C.applyMIR pot (Map.singleton Cast.aliceSHK aliceMIRCoin)
    $ (expectedStEx2 pot)

-- === Block 3, Slot 110, Epoch 1
--
-- Submit an empty block in the next epoch to apply the MIR rewards.
mir3 :: (ExMock (Crypto (ShelleyEra c))) => MIRPot -> CHAINExample (ShelleyEra c)
mir3 pot = CHAINExample (expectedStEx2 pot) (blockEx3 pot) (Right $ expectedStEx3 pot)

--
-- MIR Test Group
--

mirExample :: TestTree
mirExample =
  testGroup
    "move inst rewards"
    [ testCase "create MIR cert - reserves" $ testCHAINExample (mir1 ReservesMIR),
      testCase "create MIR cert - treasury" $ testCHAINExample (mir1 TreasuryMIR),
      testCase "insufficient MIR witnesses, reserves" $
        testCHAINExample (mirFailWits ReservesMIR),
      testCase "insufficient MIR witnesses, treasury" $
        testCHAINExample (mirFailWits TreasuryMIR),
      testCase "insufficient MIR funds, reserves" $
        testCHAINExample (mirFailFunds ReservesMIR (Coin 34000000000000000) (Coin 100) (Coin 0)),
      testCase "insufficient MIR funds, treasury" $
        testCHAINExample (mirFailFunds TreasuryMIR (Coin 99) (Coin 100) (Coin 99)),
      testCase "end of epoch after MIR - reserves" $
        testCHAINExample (mir2 ReservesMIR),
      testCase "end of epoch after MIR - treasury" $
        testCHAINExample (mir2 TreasuryMIR),
      testCase "apply MIR - reserves" $ testCHAINExample (mir3 ReservesMIR),
      testCase "apply MIR - treasury" $ testCHAINExample (mir3 TreasuryMIR)
    ]
