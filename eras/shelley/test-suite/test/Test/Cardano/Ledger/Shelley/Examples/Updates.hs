{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Updates
-- Description : Protocol Parameter Update Example
--
-- Example demonstrating using the protocol parameter update system.
module Test.Cardano.Ledger.Shelley.Examples.Updates
  ( updatesExample,
  )
where

import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Nonce,
    StrictMaybe (..),
    mkNonceFromNumber,
    (⭒),
  )
import Cardano.Ledger.Block (Block, bheader, txid)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as Cr
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Keys (asWitness, hashKey)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.EpochBoundary as EB
import Cardano.Ledger.Shelley.LedgerState (PulsingRewUpdate, emptyRewardUpdate)
import Cardano.Ledger.Shelley.PParams
  ( ProposedPPUpdates (..),
    ShelleyPParams,
    ShelleyPParamsHKD (..),
    ShelleyPParamsUpdate,
    Update (..),
  )
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx (..),
    WitnessSetHKD (..),
  )
import Cardano.Ledger.Shelley.TxBody
  ( ShelleyTxBody (..),
    ShelleyTxOut (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessesVKey)
import Cardano.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    SlotNo (..),
  )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import qualified Cardano.Protocol.HeaderCrypto as Cr
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
    coreNodeVK,
  )
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
import Test.Cardano.Ledger.Shelley.Utils (getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Cr.Crypto c => UTxO (ShelleyEra c)
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin),
      ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStUpdates :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ChainState (ShelleyEra c)
initStUpdates = initSt @(ShelleyEra c) @hc initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

ppVoteA :: ShelleyPParamsUpdate (ShelleyEra c)
ppVoteA =
  ShelleyPParams
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

collectVotes ::
  forall c hc.
  ( Era (ShelleyEra c),
    Cr.HeaderCrypto hc
  ) =>
  ShelleyPParamsUpdate (ShelleyEra c) ->
  [Int] ->
  ProposedPPUpdates (ShelleyEra c)
collectVotes vote =
  ProposedPPUpdates . Map.fromList . (fmap (\n -> (hashKey $ coreNodeVK @c @hc n, vote)))

ppVotes1 ::
  forall c hc.
  ( Era (ShelleyEra c),
    Cr.HeaderCrypto hc
  ) =>
  ProposedPPUpdates (ShelleyEra c)
ppVotes1 = collectVotes @c @hc ppVoteA [0, 3, 4]

feeTx1 :: Coin
feeTx1 = Coin 1

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> feeTx1

txbodyEx1 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1))
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    (SJust (Update (ppVotes1 @c @hc) (EpochNo 0)))
    SNothing

txEx1 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx1 =
  ShelleyTx
    (txbodyEx1 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @c @hc)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeIssuerKeys @c @hc 0,
                     asWitness . cold $ coreNodeIssuerKeys @c @hc 3,
                     asWitness . cold $ coreNodeIssuerKeys @c @hc 4
                   ]
            )
      }
    SNothing

blockEx1 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1 @c @hc]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @c @hc))
    . C.newLab (blockEx1 @c @hc)
    . C.feesAndDeposits feeTx1 (Coin 0)
    . C.newUTxO (txbodyEx1 @c @hc)
    . C.setCurrentProposals (ppVotes1 @c @hc)
    $ initStUpdates @c @hc

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, three genesis keys vote on the same new parameters.
updates1 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
updates1 = CHAINExample (initStUpdates @c @hc) blockEx1 (Right $ expectedStEx1 @c @hc)

--
-- Block 2, Slot 20, Epoch 0
--

ppVotes2 ::
  forall c hc.
  ( Era (ShelleyEra c),
    Cr.HeaderCrypto hc
  ) =>
  ProposedPPUpdates (ShelleyEra c)
ppVotes2 = collectVotes @c @hc ppVoteA [1, 5]

updateEx3B ::
  forall c hc.
  ( Era (ShelleyEra c),
    Cr.HeaderCrypto hc
  ) =>
  Update (ShelleyEra c)
updateEx3B = Update (ppVotes2 @c @hc) (EpochNo 0)

feeTx2 :: Coin
feeTx2 = Coin 1

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

txbodyEx2 ::
  forall c hc.
  (Cr.Crypto c, Cr.HeaderCrypto hc) =>
  ShelleyTxBody (ShelleyEra c)
txbodyEx2 =
  ShelleyTxBody
    (Set.fromList [TxIn (txid (txbodyEx1 @c @hc)) minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx2))
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 31)
    (SJust (updateEx3B @c @hc))
    SNothing

txEx2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx2 =
  ShelleyTx
    (txbodyEx2 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx2 @c @hc)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeIssuerKeys @c @hc 1,
                     asWitness . cold $ coreNodeIssuerKeys @c @hc 5
                   ]
            )
      }
    SNothing

blockEx2 :: forall c hc. ExMock (Crypto (ShelleyEra c)) hc => Block (BHeader c hc) (ShelleyEra c)
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx1)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 20)
    [txEx2 @c @hc]
    (SlotNo 20)
    (BlockNo 2)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 2)
    minBound
    1
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 20) 0 (KESPeriod 0))

expectedStEx2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @c @hc))
    . C.newLab (blockEx2 @c @hc)
    . C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO (txbodyEx2 @c @hc)
    . C.setCurrentProposals (collectVotes @c @hc ppVoteA [0, 1, 3, 4, 5])
    $ expectedStEx1 @c @hc

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block, two more genesis keys vote for the same new parameters.
updates2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
updates2 = CHAINExample (expectedStEx1 @c @hc) blockEx2 (Right $ expectedStEx2 @c @hc)

--
-- Block 3, Slot 80, Epoch 0
--

ppVoteB :: ShelleyPParamsUpdate (ShelleyEra c)
ppVoteB =
  ShelleyPParams
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

ppVotes3 ::
  forall c hc.
  (Era (ShelleyEra c), Cr.HeaderCrypto hc) =>
  ProposedPPUpdates (ShelleyEra c)
ppVotes3 = collectVotes @c @hc ppVoteB [1]

feeTx3 :: Coin
feeTx3 = Coin 1

aliceCoinEx3 :: Coin
aliceCoinEx3 = aliceCoinEx2 <-> feeTx3

txbodyEx3 ::
  forall c hc.
  (Cr.Crypto c, Cr.HeaderCrypto hc) =>
  ShelleyTxBody (ShelleyEra c)
txbodyEx3 =
  ShelleyTxBody
    (Set.fromList [TxIn (txid (txbodyEx2 @c @hc)) minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx3))
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx3
    (SlotNo 81)
    (SJust (Update (ppVotes3 @c @hc) (EpochNo 1)))
    SNothing

txEx3 :: forall c hc. (Cr.Crypto c, ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx3 =
  ShelleyTx
    (txbodyEx3 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx3 @c @hc)
            [asWitness Cast.alicePay, asWitness . cold $ coreNodeIssuerKeys @c @hc 1]
      }
    SNothing

blockEx3 :: forall c hc. ExMock (Crypto (ShelleyEra c)) hc => Block (BHeader c hc) (ShelleyEra c)
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx2)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 80)
    [txEx3 @c @hc]
    (SlotNo 80)
    (BlockNo 3)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 3)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 80) 0 (KESPeriod 0))

pulserEx3 :: forall c hc. (ExMock c hc) => PulsingRewUpdate c
pulserEx3 = makeCompletedPulser (BlocksMade mempty) (expectedStEx2 @c @hc)

expectedStEx3 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx3 =
  C.evolveNonceFrozen (getBlockNonce (blockEx3 @c @hc))
    . C.newLab (blockEx3 @c @hc)
    . C.feesAndDeposits feeTx3 (Coin 0)
    . C.newUTxO (txbodyEx3 @c @hc)
    . C.pulserUpdate (pulserEx3 @c @hc)
    . C.setFutureProposals (collectVotes @c @hc ppVoteB [1])
    $ expectedStEx2 @c @hc

-- === Block 3, Slot 80, Epoch 0
--
-- In the third block, one genesis keys votes for the next epoch
updates3 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
updates3 = CHAINExample (expectedStEx2 @c @hc) blockEx3 (Right $ expectedStEx3 @c @hc)

--
-- Block 4, Slot 110, Epoch 1
--

epoch1Nonce :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx3 @c @hc) ⭒ mkNonceFromNumber 123

blockEx4 :: forall c hc. ExMock (Crypto (ShelleyEra c)) hc => Block (BHeader c hc) (ShelleyEra c)
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx3)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 4)
    (epoch1Nonce @c @hc)
    (NatNonce 4)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

ppExUpdated :: ShelleyPParams (ShelleyEra c)
ppExUpdated = ppEx {_poolDeposit = Coin 200, _extraEntropy = mkNonceFromNumber 123}

expectedStEx4 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx4 =
  C.newEpoch (blockEx4 @c @hc)
    . C.newSnapshot EB.emptySnapShot (feeTx1 <+> feeTx2 <+> feeTx3)
    . C.applyRewardUpdate emptyRewardUpdate
    . C.setCurrentProposals (collectVotes @c @hc ppVoteB [1])
    . C.setFutureProposals (ProposedPPUpdates Map.empty)
    . C.setPParams ppExUpdated
    $ expectedStEx3 @c @hc

-- === Block 4, Slot 110, Epoch 1
--
-- In the fourth block, the new protocol parameters are adopted,
-- and the future vote becomes a current vote.
-- Since the extra entropy was voted on, notice that it is a part
-- of the new epoch nonce.
updates4 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
updates4 = CHAINExample (expectedStEx3 @c @hc) blockEx4 (Right (expectedStEx4 @c @hc))

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
