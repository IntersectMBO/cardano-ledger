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
module Test.Cardano.Ledger.Shelley.Examples.Updates (
  updatesExample,
  updates4,
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Nonce,
  StrictMaybe (..),
  mkNonceFromNumber,
  (⭒),
 )
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Shelley (ShelleyEra, Tx (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (PulsingRewUpdate, emptyRewardUpdate)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), Update (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (TxBody (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (addrWits)
import Cardano.Ledger.Slot (
  BlockNo (..),
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation (
  coreNodeIssuerKeys,
  coreNodeKeysBySchedule,
  coreNodeVK,
 )
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
import Test.Cardano.Ledger.Shelley.Utils (getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: UTxO ShelleyEra
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin)
    , ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStUpdates :: ChainState ShelleyEra
initStUpdates = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

ppVoteA :: PParamsUpdate ShelleyEra
ppVoteA =
  emptyPParamsUpdate
    & ppuPoolDepositL .~ SJust (Coin 200)
    & ppuExtraEntropyL .~ SJust (mkNonceFromNumber 123)

collectVotes ::
  PParamsUpdate ShelleyEra ->
  [Int] ->
  ProposedPPUpdates ShelleyEra
collectVotes vote =
  ProposedPPUpdates . Map.fromList . (fmap (\n -> (hashKey $ coreNodeVK n, vote)))

ppVotes1 :: ProposedPPUpdates ShelleyEra
ppVotes1 = collectVotes ppVoteA [0, 3, 4]

feeTx1 :: Coin
feeTx1 = Coin 1

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> feeTx1

txbodyEx1 :: TxBody ShelleyEra
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1))
    StrictSeq.empty
    (Withdrawals Map.empty)
    feeTx1
    (SlotNo 10)
    (SJust (Update ppVotes1 (EpochNo 0)))
    SNothing

txEx1 :: ShelleyTx ShelleyEra
txEx1 =
  ShelleyTx
    txbodyEx1
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx1)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . aikCold $ coreNodeIssuerKeys 0
                   , asWitness . aikCold $ coreNodeIssuerKeys 3
                   , asWitness . aikCold $ coreNodeIssuerKeys 4
                   ]
            )
      }
    SNothing

blockEx1 :: Block (BHeader MockCrypto) ShelleyEra
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
    . C.feesAndDeposits ppEx feeTx1 [] []
    . C.newUTxO txbodyEx1
    . C.setCurrentProposals ppVotes1
    $ initStUpdates

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, three genesis keys vote on the same new parameters.
updates1 :: CHAINExample ShelleyEra
updates1 = CHAINExample initStUpdates blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 20, Epoch 0
--

ppVotes2 :: ProposedPPUpdates ShelleyEra
ppVotes2 = collectVotes ppVoteA [1, 5]

updateEx3B :: Update ShelleyEra
updateEx3B = Update ppVotes2 (EpochNo 0)

feeTx2 :: Coin
feeTx2 = Coin 1

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

txbodyEx2 :: TxBody ShelleyEra
txbodyEx2 =
  ShelleyTxBody
    (Set.fromList [TxIn (txIdTxBody txbodyEx1) minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx2))
    StrictSeq.empty
    (Withdrawals Map.empty)
    (Coin 1)
    (SlotNo 31)
    (SJust updateEx3B)
    SNothing

txEx2 :: ShelleyTx ShelleyEra
txEx2 =
  ShelleyTx
    txbodyEx2
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx2)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . aikCold $ coreNodeIssuerKeys 1
                   , asWitness . aikCold $ coreNodeIssuerKeys 5
                   ]
            )
      }
    SNothing

blockEx2 :: Block (BHeader MockCrypto) ShelleyEra
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 20)
    [MkShelleyTx txEx2]
    (SlotNo 20)
    (BlockNo 2)
    nonce0
    (NatNonce 2)
    minBound
    1
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 20) 0 (KESPeriod 0))

expectedStEx2 :: ChainState ShelleyEra
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce blockEx2)
    . C.newLab blockEx2
    . C.feesAndDeposits ppEx feeTx2 [] []
    . C.newUTxO txbodyEx2
    . C.setCurrentProposals (collectVotes ppVoteA [0, 1, 3, 4, 5])
    $ expectedStEx1

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block, two more genesis keys vote for the same new parameters.
updates2 :: CHAINExample ShelleyEra
updates2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Block 3, Slot 80, Epoch 0
--

ppVoteB :: PParamsUpdate ShelleyEra
ppVoteB =
  emptyPParamsUpdate
    & ppuMinUTxOValueL .~ SJust (Coin 99)

ppVotes3 :: ProposedPPUpdates ShelleyEra
ppVotes3 = collectVotes ppVoteB [1]

feeTx3 :: Coin
feeTx3 = Coin 1

aliceCoinEx3 :: Coin
aliceCoinEx3 = aliceCoinEx2 <-> feeTx3

txbodyEx3 :: TxBody ShelleyEra
txbodyEx3 =
  ShelleyTxBody
    (Set.fromList [TxIn (txIdTxBody txbodyEx2) minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx3))
    StrictSeq.empty
    (Withdrawals Map.empty)
    feeTx3
    (SlotNo 81)
    (SJust (Update ppVotes3 (EpochNo 1)))
    SNothing

txEx3 :: ShelleyTx ShelleyEra
txEx3 =
  ShelleyTx
    txbodyEx3
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx3)
            [asWitness Cast.alicePay, asWitness . aikCold $ coreNodeIssuerKeys 1]
      }
    SNothing

blockEx3 :: Block (BHeader MockCrypto) ShelleyEra
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx2)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 80)
    [MkShelleyTx txEx3]
    (SlotNo 80)
    (BlockNo 3)
    nonce0
    (NatNonce 3)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 80) 0 (KESPeriod 0))

pulserEx3 :: PulsingRewUpdate
pulserEx3 = makeCompletedPulser (BlocksMade mempty) expectedStEx2

expectedStEx3 :: ChainState ShelleyEra
expectedStEx3 =
  C.evolveNonceFrozen (getBlockNonce blockEx3)
    . C.newLab blockEx3
    . C.feesAndDeposits ppEx feeTx3 [] []
    . C.newUTxO txbodyEx3
    . C.pulserUpdate pulserEx3
    . C.setFutureProposals (collectVotes ppVoteB [1])
    . C.solidifyProposals
    $ expectedStEx2

-- === Block 3, Slot 80, Epoch 0
--
-- In the third block, one genesis keys votes for the next epoch
updates3 :: CHAINExample ShelleyEra
updates3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 110, Epoch 1
--

epoch1Nonce :: Nonce
epoch1Nonce = chainCandidateNonce expectedStEx3 ⭒ mkNonceFromNumber 123

blockEx4 :: Block (BHeader MockCrypto) ShelleyEra
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx3)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 4)
    epoch1Nonce
    (NatNonce 4)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 110) 0 (KESPeriod 0))

ppExUpdated :: PParams ShelleyEra
ppExUpdated =
  ppEx
    & ppPoolDepositL .~ Coin 200
    & ppExtraEntropyL .~ mkNonceFromNumber 123

expectedStEx4 :: ChainState ShelleyEra
expectedStEx4 =
  C.newEpoch blockEx4
    . C.newSnapshot emptySnapShot (feeTx1 <+> feeTx2 <+> feeTx3)
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
updates4 :: CHAINExample ShelleyEra
updates4 = CHAINExample expectedStEx3 blockEx4 (Right expectedStEx4)

--
-- Updates Test Group
--

updatesExample :: TestTree
updatesExample =
  testGroup
    "protocol parameter updates"
    [ testCase "get 3/7 votes for a pparam update" $ testCHAINExample updates1
    , testCase "get 5/7 votes for a pparam update" $ testCHAINExample updates2
    , testCase "votes for the next epoch" $ testCHAINExample updates3
    , testCase "processes a pparam update" $ testCHAINExample updates4
    ]
