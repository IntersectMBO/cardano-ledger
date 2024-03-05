{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.TwoPools
-- Description : Two Pools Example
--
-- Example demonstrating a particular delegation scenario involving
-- two pools. Both pools select a reward account which is *not*
-- a pool owner, and which delegates to one of the pools.
module Test.Cardano.Ledger.Shelley.Examples.TwoPools (
  twoPoolsExample,
  twoPoolsExampleExtended,
)
where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  BoundedRational (..),
  Globals (..),
  Network (..),
  Nonce,
  ProtVer (..),
  StrictMaybe (..),
  activeSlotVal,
  mkCertIxPartial,
  natVersion,
  (⭒),
 )
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
  rationalToCoinViaFloor,
  toDeltaCoin,
 )
import Cardano.Ledger.Credential (Credential, Ptr (..))
import Cardano.Ledger.Crypto
import qualified Cardano.Ledger.EpochBoundary as EB
import Cardano.Ledger.Keys (KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.PoolDistr (
  IndividualPoolStake (..),
  PoolDistr (..),
 )
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  PulsingRewUpdate (..),
  RewardUpdate (..),
  completeStep,
  emptyRewardUpdate,
 )
import Cardano.Ledger.Shelley.PoolRank (
  Likelihood (..),
  NonMyopic (..),
  leaderProbability,
  likelihood,
 )
import Cardano.Ledger.Shelley.Rewards (
  StakeShare (..),
  aggregateRewards,
  leaderRew,
  memberRew,
  mkApparentPerformance,
  sumRewards,
 )
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
 )
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..), ShelleyTxBody (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (
  addrWits,
 )
import Cardano.Ledger.Slot (
  BlockNo (..),
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash, hashHeaderToNonce)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Data.Default.Class (def)
import Data.Group (invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto, ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation (
  coreNodeKeysBySchedule,
 )
import Test.Cardano.Ledger.Shelley.Examples.Init (
  initSt,
  lastByronHeaderHash,
  nonce0,
  ppEx,
 )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makeCompletedPulser, mkStake)
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
import Test.Cardano.Ledger.Shelley.Utils (
  epochSize,
  getBlockNonce,
  maxLLSupply,
  runShelleyBase,
  testGlobals,
  unsafeBoundRational,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

aliceInitCoin :: Coin
aliceInitCoin = Coin 10_000_000_000_000_000

bobInitCoin :: Coin
bobInitCoin = Coin 1_000_000_000_000_000

carlInitCoin :: Coin
carlInitCoin = Coin 5_000_000_000_000_000

initUTxO :: UTxO C
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin)
    , ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin)
    , ShelleyTxOut Cast.carlAddr (Val.inject carlInitCoin)
    ]

initStTwoPools :: ChainState C
initStTwoPools = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

aliceCoinEx1 :: Coin
aliceCoinEx1 =
  aliceInitCoin
    <-> ((2 :: Integer) <×> Coin 250)
    <-> ((3 :: Integer) <×> Coin 7)
    <-> feeTx1

feeTx1 :: Coin
feeTx1 = Coin 3

alicePoolParams' :: PureGenCrypto c => PoolParams c
alicePoolParams' = Cast.alicePoolParams {ppRewardAccount = RewardAccount Testnet Cast.carlSHK}

bobPoolParams' :: PureGenCrypto c => PoolParams c
bobPoolParams' = Cast.bobPoolParams {ppRewardAccount = RewardAccount Testnet Cast.carlSHK}

txbodyEx1 :: TxBody C
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    ( StrictSeq.fromList
        [ RegTxCert Cast.aliceSHK
        , RegTxCert Cast.bobSHK
        , RegTxCert Cast.carlSHK
        , RegPoolTxCert alicePoolParams'
        , RegPoolTxCert bobPoolParams'
        , DelegStakeTxCert Cast.aliceSHK (aikColdKeyHash Cast.alicePoolKeys)
        , DelegStakeTxCert Cast.bobSHK (aikColdKeyHash Cast.bobPoolKeys)
        , DelegStakeTxCert Cast.carlSHK (aikColdKeyHash Cast.alicePoolKeys)
        ]
    )
    (Withdrawals Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: ShelleyTx C
txEx1 =
  ShelleyTx
    txbodyEx1
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx1)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake, Cast.bobStake, Cast.carlStake])
                <> (asWitness <$> [aikCold Cast.alicePoolKeys, aikCold Cast.bobPoolKeys])
            )
      }
    SNothing

blockEx1 :: HasCallStack => Block (BHeader C_Crypto) C
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @C ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @C_Crypto)
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @C ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: ChainState C
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce blockEx1)
    . C.newLab blockEx1
    . C.feesAndDeposits
      ppEx
      feeTx1
      [Cast.aliceSHK, Cast.bobSHK, Cast.carlSHK]
      [alicePoolParams', bobPoolParams']
    . C.newUTxO txbodyEx1
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 0))
    . C.newStakeCred Cast.bobSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 1))
    . C.newStakeCred Cast.carlSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 2))
    . C.newPool alicePoolParams'
    . C.newPool bobPoolParams'
    . C.delegation Cast.aliceSHK (ppId alicePoolParams')
    . C.delegation Cast.bobSHK (ppId bobPoolParams')
    . C.delegation Cast.carlSHK (ppId alicePoolParams')
    $ initStTwoPools

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block of this example, Alice, Bob, and Carl register
-- stake credentials, Alice and Bob register stake pools,
-- Alice and Carl delegate to Alice's pool, and Bob delegates to Bob's pool.
--
-- This is the only block in this example that includes a transaction,
-- and after this block is processed, the UTxO will consist entirely
-- of Alice's new coin aliceCoinEx1, and Bob and Carls initial genesis coins.
twoPools1 :: CHAINExample (BHeader C_Crypto) C
twoPools1 = CHAINExample initStTwoPools blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 90, Epoch 0
--
blockEx2 :: Block (BHeader C_Crypto) C
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule @C ppEx 90)
    []
    (SlotNo 90)
    (BlockNo 2)
    (nonce0 @C_Crypto)
    (NatNonce 2)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @C ppEx 90) 0 (KESPeriod 0))

expectedStEx2 :: ChainState C
expectedStEx2 =
  C.evolveNonceFrozen (getBlockNonce blockEx2)
    . C.newLab blockEx2
    . C.rewardUpdate emptyRewardUpdate
    $ expectedStEx1

-- === Block 2, Slot 90, Epoch 0
--
-- Create an empty block near the end of epoch 0 to close out the epoch.
twoPools2 :: CHAINExample (BHeader C_Crypto) C
twoPools2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: Nonce
epoch1Nonce = chainCandidateNonce expectedStEx2

blockEx3 :: Block (BHeader C_Crypto) C
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx2)
    (coreNodeKeysBySchedule @C ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    epoch1Nonce
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @C ppEx 110) 0 (KESPeriod 0))

snapEx3 :: ExMock c => EB.SnapShot c
snapEx3 =
  EB.SnapShot
    { EB.ssStake =
        mkStake
          [ (Cast.aliceSHK, aliceCoinEx1)
          , (Cast.bobSHK, bobInitCoin)
          , (Cast.carlSHK, carlInitCoin)
          ]
    , EB.ssDelegations =
        [ (Cast.aliceSHK, aikColdKeyHash Cast.alicePoolKeys)
        , (Cast.bobSHK, aikColdKeyHash Cast.bobPoolKeys)
        , (Cast.carlSHK, aikColdKeyHash Cast.alicePoolKeys)
        ]
    , EB.ssPoolParams =
        [ (aikColdKeyHash Cast.alicePoolKeys, alicePoolParams')
        , (aikColdKeyHash Cast.bobPoolKeys, bobPoolParams')
        ]
    }

expectedStEx3 :: ChainState C
expectedStEx3 =
  C.newEpoch blockEx3
    . C.newSnapshot snapEx3 feeTx1
    . C.applyRewardUpdate emptyRewardUpdate
    $ expectedStEx2

-- === Block 3, Slot 110, Epoch 1
--
-- Create an empty block at the begining of epoch 1 to trigger the epoch change.
twoPools3 :: CHAINExample (BHeader C_Crypto) C
twoPools3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 190, Epoch 1
--

blockEx4 :: Block (BHeader C_Crypto) C
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx3)
    (coreNodeKeysBySchedule @C ppEx 190)
    []
    (SlotNo 190)
    (BlockNo 4)
    epoch1Nonce
    (NatNonce 4)
    minBound
    9
    0
    (mkOCert (coreNodeKeysBySchedule @C ppEx 190) 0 (KESPeriod 0))

deltaREx4 :: Coin
deltaREx4 = Coin 3

rewardUpdateEx4 :: forall c. RewardUpdate c
rewardUpdateEx4 =
  RewardUpdate
    { deltaT = DeltaCoin 0
    , deltaR = toDeltaCoin deltaREx4 -- No rewards paid out, fees go to reserves
    , rs = Map.empty
    , deltaF = invert $ toDeltaCoin feeTx1
    , nonMyopic = def {rewardPotNM = feeTx1}
    }

expectedStEx4 :: ChainState C
expectedStEx4 =
  C.evolveNonceFrozen (getBlockNonce blockEx4)
    . C.newLab blockEx4
    . C.rewardUpdate rewardUpdateEx4
    $ expectedStEx3

-- === Block 4, Slot 190, Epoch 1
--
-- Create an empty block near the end of epoch 0 to close out the epoch,
-- preparing the way for the first non-empty pool distribution.
twoPools4 :: CHAINExample (BHeader C_Crypto) C
twoPools4 = CHAINExample expectedStEx3 blockEx4 (Right expectedStEx4)

epoch2Nonce :: Nonce
epoch2Nonce =
  chainCandidateNonce expectedStEx4
    ⭒ hashHeaderToNonce (bhHash $ bheader blockEx2)

--
-- Block 5, Slot 221, Epoch 2
--

blockEx5 :: Block (BHeader C_Crypto) C
blockEx5 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx4)
    Cast.alicePoolKeys
    []
    (SlotNo 221) -- odd slots open for decentralization
    (BlockNo 5)
    epoch2Nonce
    (NatNonce 5)
    minBound
    11
    10
    (mkOCert Cast.alicePoolKeys 0 (KESPeriod 10))

activeStakeEx5 :: Integer
activeStakeEx5 = sum $ map unCoin [aliceCoinEx1, bobInitCoin, carlInitCoin]

alicePoolStake :: Rational
alicePoolStake = (unCoin aliceCoinEx1 + unCoin carlInitCoin) % activeStakeEx5

bobPoolStake :: Rational
bobPoolStake = unCoin bobInitCoin % activeStakeEx5

pdEx5 :: forall c. PureGenCrypto c => PoolDistr c
pdEx5 =
  PoolDistr $
    Map.fromList
      [
        ( aikColdKeyHash $ Cast.alicePoolKeys @c
        , IndividualPoolStake alicePoolStake (Cast.aliceVRFKeyHash @c)
        )
      ,
        ( aikColdKeyHash $ Cast.bobPoolKeys @c
        , IndividualPoolStake bobPoolStake (Cast.bobVRFKeyHash @c)
        )
      ]

expectedStEx5 :: ChainState C
expectedStEx5 =
  C.incrBlockCount (aikColdKeyHash Cast.alicePoolKeys)
    . C.newSnapshot snapEx3 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx4
    . C.setPoolDistr pdEx5
    . C.setOCertCounter (coerceKeyRole $ aikColdKeyHash Cast.alicePoolKeys) 0
    . C.newEpoch blockEx5 -- This must be processed before the incrBlockCount
    $ expectedStEx4

-- === Block 5, Slot 220, Epoch 2
--
-- Create the first non-empty pool distribution by starting the epoch 2.
-- Moreover, Alice's pool produces the block.
twoPools5 :: CHAINExample (BHeader C_Crypto) C
twoPools5 = CHAINExample expectedStEx4 blockEx5 (Right expectedStEx5)

--
-- Block 6, Slot 295, Epoch 2
--

blockEx6 :: Block (BHeader C_Crypto) C
blockEx6 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx5)
    Cast.alicePoolKeys
    []
    (SlotNo 295) -- odd slots open for decentralization
    (BlockNo 6)
    epoch2Nonce
    (NatNonce 6)
    minBound
    14
    14
    (mkOCert Cast.alicePoolKeys 0 (KESPeriod 14))

expectedStEx6 :: ChainState C
expectedStEx6 =
  C.evolveNonceFrozen (getBlockNonce blockEx6)
    . C.newLab blockEx6
    . C.setOCertCounter (coerceKeyRole $ aikColdKeyHash Cast.alicePoolKeys) 0
    . C.incrBlockCount (aikColdKeyHash Cast.alicePoolKeys)
    . C.rewardUpdate emptyRewardUpdate
    $ expectedStEx5

-- === Block 6, Slot 295, Epoch 2
--
-- Alice's pool produces a second block.
twoPools6 :: CHAINExample (BHeader C_Crypto) C
twoPools6 = CHAINExample expectedStEx5 blockEx6 (Right expectedStEx6)

--
-- Block 7, Slot 297, Epoch 2
--

blockEx7 :: Block (BHeader C_Crypto) C
blockEx7 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx6)
    Cast.bobPoolKeys
    []
    (SlotNo 297) -- odd slots open for decentralization
    (BlockNo 7)
    epoch2Nonce
    (NatNonce 7)
    minBound
    14
    14
    (mkOCert Cast.bobPoolKeys 0 (KESPeriod 14))

expectedStEx7 :: ChainState C
expectedStEx7 =
  C.evolveNonceFrozen (getBlockNonce blockEx7)
    . C.newLab blockEx7
    . C.setOCertCounter (coerceKeyRole $ aikColdKeyHash Cast.bobPoolKeys) 0
    . C.incrBlockCount (aikColdKeyHash Cast.bobPoolKeys)
    $ expectedStEx6

-- === Block 7, Slot 295, Epoch 2
--
-- Bob's pool produces a block.
twoPools7 :: CHAINExample (BHeader C_Crypto) C
twoPools7 = CHAINExample expectedStEx6 blockEx7 (Right expectedStEx7)

--
-- Block 8, Slot 310, Epoch 3
--

epoch3Nonce :: Nonce
epoch3Nonce =
  chainCandidateNonce expectedStEx7
    ⭒ hashHeaderToNonce (bhHash $ bheader blockEx4)

blockEx8 :: Block (BHeader C_Crypto) C
blockEx8 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx7)
    (coreNodeKeysBySchedule @C ppEx 310)
    []
    (SlotNo 310)
    (BlockNo 8)
    epoch3Nonce
    (NatNonce 8)
    minBound
    15
    15
    (mkOCert (coreNodeKeysBySchedule @C ppEx 310) 1 (KESPeriod 15))

expectedStEx8 :: ChainState C
expectedStEx8 =
  C.newEpoch blockEx8
    . C.newSnapshot snapEx3 (Coin 0)
    . C.setOCertCounter coreNodeHK 1
    . C.applyRewardUpdate emptyRewardUpdate
    $ expectedStEx7
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @C ppEx 310

-- === Block 8, Slot 310, Epoch 3
--
-- Create an empty block to start epoch 3.
twoPools8 :: CHAINExample (BHeader C_Crypto) C
twoPools8 = CHAINExample expectedStEx7 blockEx8 (Right expectedStEx8)

--
-- Block 9, Slot 390, Epoch 3
--

blockEx9 :: Block (BHeader C_Crypto) C
blockEx9 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader C_Crypto) blockEx8)
    (coreNodeKeysBySchedule @C ppEx 390)
    []
    (SlotNo 390)
    (BlockNo 9)
    epoch3Nonce
    (NatNonce 9)
    minBound
    19
    19
    (mkOCert (coreNodeKeysBySchedule @C ppEx 390) 2 (KESPeriod 19))

blocksMadeEpoch3 :: Integer
blocksMadeEpoch3 = 3

expectedBlocks :: Integer
expectedBlocks =
  floor $
    0.5
      * unboundRational (activeSlotVal $ activeSlotCoeff testGlobals)
      * fromIntegral (epochSize $ EpochNo 3)

reserves9 :: Coin
reserves9 = maxLLSupply <+> deltaREx4 <-> (aliceInitCoin <+> bobInitCoin <+> carlInitCoin)

deltaR1Ex9 :: Coin
deltaR1Ex9 =
  rationalToCoinViaFloor $
    (blocksMadeEpoch3 % expectedBlocks)
      * 0.0021
      * (fromIntegral . unCoin $ reserves9)

rPot :: Integer
rPot = unCoin deltaR1Ex9 -- There were no fees

deltaTEx9 :: Integer
deltaTEx9 = floor $ (0.2 :: Double) * fromIntegral rPot

bigR :: Coin
bigR = Coin $ rPot - deltaTEx9

circulation :: Integer
circulation = unCoin $ maxLLSupply <-> reserves9

aliceStakeShareTot :: Rational
aliceStakeShareTot = (unCoin aliceCoinEx1 + unCoin carlInitCoin) % circulation

bobStakeShareTot :: Rational
bobStakeShareTot = unCoin bobInitCoin % circulation

alicePoolRewards :: forall c. ExMock c => Coin
alicePoolRewards = rationalToCoinViaFloor (appPerf * (fromIntegral . unCoin $ maxP))
  where
    appPerf = mkApparentPerformance (ppEx @(ShelleyEra c) ^. ppDL) alicePoolStake 2 3
    pledge = fromIntegral . unCoin . ppPledge $ alicePoolParams' @c
    pr = pledge % circulation
    maxP = EB.maxPool @(ShelleyEra c) ppEx bigR aliceStakeShareTot pr

carlMemberRewardsFromAlice :: forall c. ExMock c => Coin
carlMemberRewardsFromAlice =
  memberRew
    (alicePoolRewards @c)
    (alicePoolParams' @c)
    (StakeShare $ unCoin carlInitCoin % circulation)
    (StakeShare aliceStakeShareTot)

carlLeaderRewardsFromAlice :: forall c. ExMock c => Coin
carlLeaderRewardsFromAlice =
  leaderRew
    (alicePoolRewards @c)
    (alicePoolParams' @c)
    (StakeShare $ unCoin aliceCoinEx1 % circulation)
    (StakeShare aliceStakeShareTot)

bobPoolRewards :: forall c. ExMock c => Coin
bobPoolRewards = rationalToCoinViaFloor (appPerf * (fromIntegral . unCoin $ maxP))
  where
    appPerf = mkApparentPerformance (ppEx @(ShelleyEra c) ^. ppDL) bobPoolStake 1 3
    pledge = fromIntegral . unCoin . ppPledge $ bobPoolParams' @c
    pr = pledge % circulation
    maxP = EB.maxPool @(ShelleyEra c) ppEx bigR bobStakeShareTot pr

carlLeaderRewardsFromBob :: forall c. ExMock c => Coin
carlLeaderRewardsFromBob =
  leaderRew
    (bobPoolRewards @c)
    (bobPoolParams' @c)
    (StakeShare $ unCoin bobInitCoin % circulation)
    (StakeShare bobStakeShareTot)

alicePerfEx9 :: Likelihood
alicePerfEx9 = likelihood blocks t (epochSize $ EpochNo 3)
  where
    blocks = 2
    t = leaderProbability f alicePoolStake (unsafeBoundRational 0.5)
    f = activeSlotCoeff testGlobals

bobPerfEx9 :: Likelihood
bobPerfEx9 = likelihood blocks t (epochSize $ EpochNo 3)
  where
    blocks = 1
    t = leaderProbability f bobPoolStake (unsafeBoundRational 0.5)
    f = activeSlotCoeff testGlobals

nonMyopicEx9 :: forall c. ExMock c => NonMyopic c
nonMyopicEx9 =
  NonMyopic
    ( Map.fromList
        [ (aikColdKeyHash Cast.alicePoolKeys, alicePerfEx9)
        , (aikColdKeyHash Cast.bobPoolKeys, bobPerfEx9)
        ]
    )
    bigR

rewardUpdateEx9 ::
  PParams C ->
  Map (Credential 'Staking C_Crypto) (Set (Reward C_Crypto)) ->
  RewardUpdate C_Crypto
rewardUpdateEx9 pp rewards =
  RewardUpdate
    { deltaT = DeltaCoin deltaTEx9
    , deltaR = invert (toDeltaCoin deltaR1Ex9) <> toDeltaCoin deltaR2Ex9
    , rs = rewards
    , deltaF = DeltaCoin 0
    , nonMyopic = nonMyopicEx9
    }
  where
    pv = pp ^. ppProtocolVersionL
    deltaR2Ex9 = bigR <-> sumRewards pv rewards

pulserEx9 :: PParams C -> PulsingRewUpdate (EraCrypto C)
pulserEx9 pp =
  makeCompletedPulser
    ( BlocksMade $
        Map.fromList
          [(aikColdKeyHash Cast.alicePoolKeys, 2), (aikColdKeyHash Cast.bobPoolKeys, 1)]
    )
    expectedStEx8'
  where
    expectedStEx8' = C.setPrevPParams pp expectedStEx8

expectedStEx9 :: PParams C -> ChainState C
expectedStEx9 pp =
  C.evolveNonceFrozen (getBlockNonce blockEx9)
    . C.newLab blockEx9
    . C.setOCertCounter coreNodeHK 2
    . C.pulserUpdate (pulserEx9 pp)
    $ expectedStEx8
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @C ppEx 390

-- === Block 9, Slot 390, Epoch 3
--
-- Create the first non-trivial reward update. The rewards demonstrate the
-- results of the delegation scenario that was constructed in the first and only transaction.
twoPools9 :: CHAINExample (BHeader (EraCrypto C)) C
twoPools9 = CHAINExample expectedStEx8 blockEx9 (Right $ expectedStEx9 ppEx)

--
-- Now test with Aggregation
--
carlsRewards :: forall c. ExMock c => Set (Reward c)
carlsRewards =
  Set.fromList
    [ Reward MemberReward (aikColdKeyHash Cast.alicePoolKeys) (carlMemberRewardsFromAlice @c)
    , Reward LeaderReward (aikColdKeyHash Cast.alicePoolKeys) (carlLeaderRewardsFromAlice @c)
    , Reward LeaderReward (aikColdKeyHash Cast.bobPoolKeys) (carlLeaderRewardsFromBob @c)
    ]

rsEx9Agg :: forall c. ExMock c => Map (Credential 'Staking c) (Set (Reward c))
rsEx9Agg = Map.singleton Cast.carlSHK carlsRewards

ppProtVer3 :: PParams C
ppProtVer3 = ppEx & ppProtocolVersionL .~ ProtVer (natVersion @3) 0

expectedStEx8Agg :: ChainState C
expectedStEx8Agg = C.setPrevPParams ppProtVer3 expectedStEx8

expectedStEx9Agg :: ChainState C
expectedStEx9Agg = C.setPrevPParams ppProtVer3 (expectedStEx9 ppProtVer3)

-- Create the first non-trivial reward update. The rewards demonstrate the
-- results of the delegation scenario that was constructed in the first and only transaction.
twoPools9Agg :: CHAINExample (BHeader C_Crypto) C
twoPools9Agg = CHAINExample expectedStEx8Agg blockEx9 (Right expectedStEx9Agg)

testAggregateRewardsLegacy :: HasCallStack => Assertion
testAggregateRewardsLegacy = do
  let expectedReward = carlLeaderRewardsFromBob @(EraCrypto C)
  expectedReward @?= rewardAmount (minimum (carlsRewards @(EraCrypto C)))
  aggregateRewards @C_Crypto (ppEx ^. ppProtocolVersionL @C) rsEx9Agg
    @?= Map.singleton Cast.carlSHK expectedReward

testAggregateRewardsNew :: Assertion
testAggregateRewardsNew =
  aggregateRewards @C_Crypto (ppProtVer3 ^. ppProtocolVersionL @C) rsEx9Agg
    @?= Map.singleton Cast.carlSHK (foldMap rewardAmount (carlsRewards @(EraCrypto C)))

--
-- Two Pools Test Group
--

twoPoolsExample :: TestTree
twoPoolsExample =
  testGroup
    "two pools"
    [ testCase "create non-aggregated pulser" $ testCHAINExample twoPools9
    , testCase "non-aggregated pulser is correct" $
        Complete (rewardUpdateEx9 ppEx rsEx9Agg)
          @?= (fst . runShelleyBase . completeStep $ pulserEx9 ppEx)
    , testCase "aggregated pulser is correct" $
        Complete (rewardUpdateEx9 ppProtVer3 rsEx9Agg)
          @?= (fst . runShelleyBase . completeStep $ pulserEx9 ppProtVer3)
    , testCase "create aggregated pulser" $ testCHAINExample twoPools9Agg
    , testCase "create legacy aggregatedRewards" testAggregateRewardsLegacy
    , testCase "create new aggregatedRewards" testAggregateRewardsNew
    ]

-- This test group tests each block individually, which is really only
-- helpful for debugging purposes.
twoPoolsExampleExtended :: TestTree
twoPoolsExampleExtended =
  testGroup
    "two pools extended"
    [ testCase "initial registrations" $ testCHAINExample twoPools1
    , testCase "delegate stake and create reward update" $ testCHAINExample twoPools2
    , testCase "new epoch changes" $ testCHAINExample twoPools3
    , testCase "second reward update" $ testCHAINExample twoPools4
    , testCase "nonempty pool distr" $ testCHAINExample twoPools5
    , testCase "alice produces a block" $ testCHAINExample twoPools6
    , testCase "bob produces a block" $ testCHAINExample twoPools7
    , testCase "prelude to the first nontrivial rewards" $ testCHAINExample twoPools8
    , testCase "create non-aggregated rewards" $ testCHAINExample twoPools9
    , testCase "create aggregated rewards" $ testCHAINExample twoPools9Agg
    ]
