{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Updates
-- Description : Protocol Parameter Update Example
--
-- Example demonstrating using the protocol parameter update system.
module Test.Shelley.Spec.Ledger.Examples.Updates
  ( updatesExample,
  )
where

import Cardano.Ledger.Era (Crypto (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce,
    StrictMaybe (..),
    mkNonceFromNumber,
    (⭒),
  )
import Shelley.Spec.Ledger.BlockChain (Block, bhHash, bheader)
import Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys (asWitness, hashKey)
import Shelley.Spec.Ledger.LedgerState (emptyRewardUpdate)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    ProposedPPUpdates (..),
    Update (..),
  )
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    SlotNo (..),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxBody
  ( TxBody (..),
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
import Test.Shelley.Spec.Ledger.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
    coreNodeVK,
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
    genesisId,
    mkBlockFakeVRF,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils (getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Era era => UTxO era
initUTxO =
  genesisCoins
    [ TxOut Cast.aliceAddr aliceInitCoin,
      TxOut Cast.bobAddr bobInitCoin
    ]

initStUpdates :: forall era. Era era => ChainState era
initStUpdates = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

ppVoteA :: PParamsUpdate
ppVoteA =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SJust $ Coin 200,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SJust (mkNonceFromNumber 123),
      _protocolVersion = SNothing,
      _minUTxOValue = SNothing,
      _minPoolCost = SNothing
    }

collectVotes :: Era era => PParamsUpdate -> [Int] -> ProposedPPUpdates era
collectVotes vote =
  ProposedPPUpdates . Map.fromList . (fmap (\n -> (hashKey $ coreNodeVK n, vote)))

ppVotes1 :: Era era => ProposedPPUpdates era
ppVotes1 = collectVotes ppVoteA [0, 3, 4]

feeTx1 :: Coin
feeTx1 = Coin 1

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin Val.~~ feeTx1

txbodyEx1 :: Era era => TxBody era
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx1)
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    (SJust (Update ppVotes1 (EpochNo 0)))
    SNothing

txEx1 :: (Era era, ExMock (Crypto era)) => Tx era
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx1)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeIssuerKeys 0,
                     asWitness . cold $ coreNodeIssuerKeys 3,
                     asWitness . cold $ coreNodeIssuerKeys 4
                   ]
            )
      }
    SNothing

blockEx1 :: forall era. (Era era, ExMock (Crypto era)) => Block era
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
    . C.feesAndDeposits feeTx1 (Coin 0)
    . C.newUTxO txbodyEx1
    . C.setCurrentProposals ppVotes1
    $ initStUpdates

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, three genesis keys vote on the same new parameters.
updates1 :: (Era era, ExMock (Crypto era)) => CHAINExample era
updates1 = CHAINExample initStUpdates blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 20, Epoch 0
--

ppVotes2 :: Era era => ProposedPPUpdates era
ppVotes2 = collectVotes ppVoteA [1, 5]

updateEx3B :: Era era => Update era
updateEx3B = Update ppVotes2 (EpochNo 0)

feeTx2 :: Coin
feeTx2 = Coin 1

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 Val.~~ feeTx2

txbodyEx2 :: Era era => TxBody era
txbodyEx2 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx1) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx2)
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 31)
    (SJust updateEx3B)
    SNothing

txEx2 :: (Era era, ExMock (Crypto era)) => Tx era
txEx2 =
  Tx
    txbodyEx2
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx2)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeIssuerKeys 1,
                     asWitness . cold $ coreNodeIssuerKeys 5
                   ]
            )
      }
    SNothing

blockEx2 :: forall era. (Era era, ExMock (Crypto era)) => Block era
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule ppEx 20)
    [txEx2]
    (SlotNo 20)
    (BlockNo 2)
    (nonce0 @era)
    (NatNonce 2)
    zero
    1
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 20) 0 (KESPeriod 0))

expectedStEx2 :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @era))
    . C.newLab blockEx2
    . C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO txbodyEx2
    . C.setCurrentProposals (collectVotes ppVoteA [0, 1, 3, 4, 5])
    $ expectedStEx1

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block, two more genesis keys vote for the same new parameters.
updates2 :: (Era era, ExMock (Crypto era)) => CHAINExample era
updates2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Block 3, Slot 80, Epoch 0
--

ppVoteB :: PParamsUpdate
ppVoteB =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SNothing,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minUTxOValue = SJust $ Coin 99,
      _minPoolCost = SNothing
    }

ppVotes3 :: Era era => ProposedPPUpdates era
ppVotes3 = collectVotes ppVoteB [1]

feeTx3 :: Coin
feeTx3 = Coin 1

aliceCoinEx3 :: Coin
aliceCoinEx3 = aliceCoinEx2 Val.~~ feeTx3

txbodyEx3 :: Era era => TxBody era
txbodyEx3 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx3)
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx3
    (SlotNo 81)
    (SJust (Update ppVotes3 (EpochNo 1)))
    SNothing

txEx3 :: (Era era, ExMock (Crypto era)) => Tx era
txEx3 =
  Tx
    txbodyEx3
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx3)
            [asWitness Cast.alicePay, asWitness . cold $ coreNodeIssuerKeys 1]
      }
    SNothing

blockEx3 :: forall era. (Era era, ExMock (Crypto era)) => Block era
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx2)
    (coreNodeKeysBySchedule ppEx 80)
    [txEx3]
    (SlotNo 80)
    (BlockNo 3)
    (nonce0 @era)
    (NatNonce 3)
    zero
    4
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 80) 0 (KESPeriod 0))

expectedStEx3 :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx3 =
  C.evolveNonceFrozen (getBlockNonce (blockEx3 @era))
    . C.newLab blockEx3
    . C.feesAndDeposits feeTx3 (Coin 0)
    . C.newUTxO txbodyEx3
    . C.rewardUpdate emptyRewardUpdate
    . C.setFutureProposals (collectVotes ppVoteB [1])
    $ expectedStEx2

-- === Block 3, Slot 80, Epoch 0
--
-- In the third block, one genesis keys votes for the next epoch
updates3 :: (Era era, ExMock (Crypto era)) => CHAINExample era
updates3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 110, Epoch 1
--

epoch1Nonce :: forall era. (Era era, ExMock (Crypto era)) => Nonce
epoch1Nonce = (chainCandidateNonce (expectedStEx3 @era)) ⭒ mkNonceFromNumber 123

blockEx4 :: forall era. (Era era, ExMock (Crypto era)) => Block era
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx3)
    (coreNodeKeysBySchedule ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 4)
    (epoch1Nonce @era)
    (NatNonce 4)
    zero
    5
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 110) 0 (KESPeriod 0))

ppExUpdated :: PParams
ppExUpdated = ppEx {_poolDeposit = Coin 200, _extraEntropy = mkNonceFromNumber 123}

expectedStEx4 :: forall era. (Era era, ExMock (Crypto era)) => ChainState era
expectedStEx4 =
  C.newEpoch blockEx4
    . C.newSnapshot EB.emptySnapShot (feeTx1 <> feeTx2 <> feeTx3)
    . C.applyRewardUpdate emptyRewardUpdate
    . C.setCurrentProposals (collectVotes ppVoteB [1])
    . C.setFutureProposals (ProposedPPUpdates Map.empty)
    . C.setPParams ppExUpdated
    $ expectedStEx3

-- === Block 4, Slot 110, Epoch 1
--
-- In the fourth block, the new protocol parameters are adopted,
-- and the future vote becomes a current vote.
-- Since the extra entropy was voted on, notice that it is a part
-- of the new epoch nonce.
updates4 :: (Era era, ExMock (Crypto era)) => CHAINExample era
updates4 = CHAINExample expectedStEx3 blockEx4 (Right expectedStEx4)

--
-- Updates Test Group
--

updatesExample :: TestTree
updatesExample =
  testGroup
    "protocol parameter updates"
    [ testCase "get 3/7 votes for a pparam update" $ testCHAINExample updates1,
      testCase "get 5/7 votes for a pparam update" $ testCHAINExample updates2,
      testCase "votes for the next epoch" $ testCHAINExample updates3,
      testCase "processes a pparam update" $ testCHAINExample updates4
    ]
