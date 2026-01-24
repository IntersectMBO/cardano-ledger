{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Example demonstrating the creation of a new stake pool,
-- block production under Praos, rewards, and pool retirement.
module Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (
  makePulser,
  makePulser',
  makeCompletedPulser,
  poolLifetimeExample,
  mkStake,
) where

import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Globals (..),
  Network (..),
  Nonce,
  StrictMaybe (..),
  mkCertIxPartial,
  (⭒),
 )
import Cardano.Ledger.Block (Block (blockHeader))
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (CompactCoin),
  DeltaCoin (..),
  addDeltaCoin,
  knownNonZeroCoin,
  toDeltaCoin,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential, Ptr (..), SlotNo32 (..))
import Cardano.Ledger.Keys (asWitness, coerceKeyRole)
import Cardano.Ledger.Shelley (ShelleyEra, Tx (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  PulsingRewUpdate (..),
  RewardUpdate (..),
  completeRupd,
  decayFactor,
  emptyRewardUpdate,
  startStep,
 )
import Cardano.Ledger.Shelley.PoolRank (
  Likelihood (..),
  NonMyopic (..),
  applyDecay,
  leaderProbability,
  likelihood,
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (
  TxBody (..),
 )
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (
  addrWits,
 )
import Cardano.Ledger.Slot (
  BlockNo (..),
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash, hashHeaderToNonce)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Data.Default (def)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import GHC.Exts (fromList)
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import Test.Cardano.Ledger.Shelley.Examples.Chain (CHAINExample (..), testCHAINExample)
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
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  NatNonce (..),
  genesisCoins,
  mkBlockFakeVRF,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rewards (mkSnapShot)
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
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin 10_000_000_000_000_000

bobInitCoin :: Coin
bobInitCoin = Coin 1_000_000_000_000_000

toCompactCoinError :: Coin -> CompactForm Coin
toCompactCoinError c =
  case toCompact c of
    Nothing -> error $ "Invalid coin: " <> show c
    Just compactCoin -> compactCoin

mkStake :: [(Credential Staking, Coin)] -> Stake
mkStake = Stake . GHC.Exts.fromList . map (fmap toCompactCoinError)

initUTxO :: UTxO ShelleyEra
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin)
    , ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStPoolLifetime :: ChainState ShelleyEra
initStPoolLifetime = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

aliceCoinEx1 :: Coin
aliceCoinEx1 =
  aliceInitCoin
    <-> Coin 250
    <-> ((3 :: Integer) <×> Coin 7)
    <-> Coin 3

carlMIR :: Coin
carlMIR = Coin 110

dariaMIR :: Coin
dariaMIR = Coin 99

feeTx1 :: Coin
feeTx1 = Coin 3

txbodyEx1 :: TxBody TopTx ShelleyEra
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    ( StrictSeq.fromList
        ( [ RegTxCert Cast.aliceSHK
          , RegTxCert Cast.bobSHK
          , RegTxCert Cast.carlSHK
          , RegPoolTxCert Cast.aliceStakePoolParams
          ]
            ++ [ ShelleyTxCertMir
                   ( MIRCert
                       ReservesMIR
                       ( StakeAddressesMIR $
                           Map.fromList
                             [ (Cast.carlSHK, toDeltaCoin carlMIR)
                             , (Cast.dariaSHK, toDeltaCoin dariaMIR)
                             ]
                       )
                   )
               ]
        )
    )
    (Withdrawals Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: ShelleyTx TopTx ShelleyEra
txEx1 =
  ShelleyTx
    txbodyEx1
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx1)
            ( (asWitness <$> [Cast.alicePay, Cast.carlPay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ aikCold Cast.alicePoolKeys]
                <> ( asWitness
                       <$> [ aikCold (coreNodeIssuerKeys 0)
                           , aikCold (coreNodeIssuerKeys 1)
                           , aikCold (coreNodeIssuerKeys 2)
                           , aikCold (coreNodeIssuerKeys 3)
                           , aikCold (coreNodeIssuerKeys 4)
                           ]
                   )
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
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo32 10) minBound (mkCertIxPartial 0))
    . C.newStakeCred Cast.bobSHK (Ptr (SlotNo32 10) minBound (mkCertIxPartial 1))
    . C.newStakeCred Cast.carlSHK (Ptr (SlotNo32 10) minBound (mkCertIxPartial 2))
    . C.regPool Cast.aliceStakePoolParams
    . C.mir Cast.carlSHK ReservesMIR carlMIR
    . C.mir Cast.dariaSHK ReservesMIR dariaMIR
    $ initStPoolLifetime

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block of this example, Alice, Bob, and Carl
-- all register stake credentials, and Alice registers a stake pool.
-- Additionally, a MIR certificate is issued to draw from the reserves
-- and give Carl and Daria (who is unregistered) rewards.
poolLifetime1 :: CHAINExample ShelleyEra
poolLifetime1 = CHAINExample initStPoolLifetime blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 90, Epoch 0
--

feeTx2 :: Coin
feeTx2 = Coin 4

aliceCoinEx2Base :: Coin
aliceCoinEx2Base = Coin $ 5 * 1000 * 1000 * 1000 * 1000 * 1000

aliceCoinEx2Ptr :: Coin
aliceCoinEx2Ptr = aliceCoinEx1 <-> (aliceCoinEx2Base <+> feeTx2)

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2 :: TxBody TopTx ShelleyEra
txbodyEx2 =
  ShelleyTxBody
    { stbInputs = Set.fromList [TxIn (txIdTxBody txbodyEx1) minBound]
    , stbOutputs =
        StrictSeq.fromList
          [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx2Base)
          , ShelleyTxOut Cast.alicePtrAddr (Val.inject aliceCoinEx2Ptr)
          ]
    , stbCerts =
        StrictSeq.fromList
          [ DelegStakeTxCert Cast.aliceSHK (aikColdKeyHash Cast.alicePoolKeys)
          , DelegStakeTxCert Cast.bobSHK (aikColdKeyHash Cast.alicePoolKeys)
          ]
    , stbWithdrawals = Withdrawals Map.empty
    , stbTxFee = feeTx2
    , stbTTL = SlotNo 90
    , stbUpdate = SNothing
    , stbMDHash = SNothing
    }

txEx2 :: ShelleyTx TopTx ShelleyEra
txEx2 =
  ShelleyTx
    txbodyEx2
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx2)
            [ asWitness Cast.alicePay
            , asWitness Cast.aliceStake
            , asWitness Cast.bobStake
            ]
      }
    SNothing

blockEx2 :: Block (BHeader MockCrypto) ShelleyEra
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx1)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 90)
    [MkShelleyTx txEx2]
    (SlotNo 90)
    (BlockNo 2)
    nonce0
    (NatNonce 2)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 90) 0 (KESPeriod 0))

makePulser :: (EraGov era, EraCertState era) => BlocksMade -> ChainState era -> PulsingRewUpdate
makePulser bs cs =
  startStep
    (epochSize $ EpochNo 0)
    bs
    (nesEs $ chainNes cs)
    maxLLSupply
    (activeSlotCoeff testGlobals)
    (securityParameter testGlobals)

makePulser' :: (EraGov era, EraCertState era) => ChainState era -> PulsingRewUpdate
makePulser' = makePulser (BlocksMade mempty)

makeCompletedPulser ::
  (EraGov era, EraCertState era) => BlocksMade -> ChainState era -> PulsingRewUpdate
makeCompletedPulser bs cs = Complete . fst . runShelleyBase . completeRupd $ makePulser bs cs

pulserEx2 :: PulsingRewUpdate
pulserEx2 = makeCompletedPulser (BlocksMade mempty) expectedStEx1

expectedStEx2 :: ChainState ShelleyEra
expectedStEx2 =
  C.evolveNonceFrozen (getBlockNonce blockEx2)
    . C.newLab blockEx2
    . C.addFees feeTx2
    . C.newUTxO txbodyEx2
    . C.delegation Cast.aliceSHK (sppId Cast.aliceStakePoolParams)
    . C.delegation Cast.bobSHK (sppId Cast.aliceStakePoolParams)
    . C.pulserUpdate pulserEx2
    $ expectedStEx1

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice and Bob both delegation to Alice's Pool.
poolLifetime2 :: CHAINExample ShelleyEra
poolLifetime2 = CHAINExample expectedStEx1 blockEx2 (Right (C.solidifyProposals expectedStEx2))

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: Nonce
epoch1Nonce = chainCandidateNonce expectedStEx2

blockEx3 :: Block (BHeader MockCrypto) ShelleyEra
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx2)
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
  let
    stake =
      mkStake
        [ (Cast.aliceSHK, aliceCoinEx2Base <> aliceCoinEx2Ptr)
        , (Cast.bobSHK, bobInitCoin)
        ]
    delegations =
      [ (Cast.aliceSHK, aikColdKeyHash Cast.alicePoolKeys)
      , (Cast.bobSHK, aikColdKeyHash Cast.alicePoolKeys)
      ]
    poolParams = [(aikColdKeyHash Cast.alicePoolKeys, Cast.aliceStakePoolParams)]
   in
    mkSnapShot stake delegations poolParams

expectedStEx3 :: ChainState ShelleyEra
expectedStEx3 =
  C.newEpoch blockEx3
    . C.newSnapshot snapEx3 (feeTx1 <> feeTx2)
    . C.applyMIR ReservesMIR (Map.singleton Cast.carlSHK carlMIR)
    . C.applyRewardUpdate emptyRewardUpdate
    $ expectedStEx2

-- === Block 3, Slot 110, Epoch 1
--
-- In the third block, an empty block in a new epoch, the first snapshot is created.
-- The rewards accounts from the MIR certificate in block 1 are now increased.
poolLifetime3 :: CHAINExample ShelleyEra
poolLifetime3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 190, Epoch 1
--

feeTx4 :: Coin
feeTx4 = Coin 5

aliceCoinEx4Base :: Coin
aliceCoinEx4Base = aliceCoinEx2Base <-> feeTx4

txbodyEx4 :: TxBody TopTx ShelleyEra
txbodyEx4 =
  ShelleyTxBody
    { stbInputs = Set.fromList [TxIn (txIdTxBody txbodyEx2) minBound]
    , stbOutputs = StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx4Base)]
    , stbCerts =
        StrictSeq.fromList
          [DelegStakeTxCert Cast.carlSHK (aikColdKeyHash Cast.alicePoolKeys)]
    , stbWithdrawals = Withdrawals Map.empty
    , stbTxFee = feeTx4
    , stbTTL = SlotNo 500
    , stbUpdate = SNothing
    , stbMDHash = SNothing
    }

txEx4 :: ShelleyTx TopTx ShelleyEra
txEx4 =
  ShelleyTx
    txbodyEx4
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx4)
            [asWitness Cast.alicePay, asWitness Cast.carlStake]
      }
    SNothing

blockEx4 :: Block (BHeader MockCrypto) ShelleyEra
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx3)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 190)
    [MkShelleyTx txEx4]
    (SlotNo 190)
    (BlockNo 4)
    epoch1Nonce
    (NatNonce 4)
    minBound
    9
    0
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 190) 0 (KESPeriod 0))

pulserEx4 :: PulsingRewUpdate
pulserEx4 = makeCompletedPulser (BlocksMade mempty) expectedStEx3

rewardUpdateEx4 :: RewardUpdate
rewardUpdateEx4 =
  RewardUpdate
    { deltaT = DeltaCoin 1
    , deltaR = DeltaCoin 6
    , rs = Map.empty
    , deltaF = DeltaCoin (-7)
    , nonMyopic = def {rewardPotNM = Coin 6}
    }

expectedStEx4 :: ChainState ShelleyEra
expectedStEx4 =
  C.evolveNonceFrozen (getBlockNonce blockEx4)
    . C.newLab blockEx4
    . C.addFees feeTx4
    . C.newUTxO txbodyEx4
    . C.delegation Cast.carlSHK (sppId Cast.aliceStakePoolParams)
    . C.pulserUpdate pulserEx4
    $ expectedStEx3

-- === Block 4, Slot 190, Epoch 1
--
-- We process a block late enough in the epoch in order to create a second reward update,
-- preparing the way for the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change, Carl delegates his stake.
poolLifetime4 :: CHAINExample ShelleyEra
poolLifetime4 = CHAINExample expectedStEx3 blockEx4 (Right (C.solidifyProposals expectedStEx4))

epoch2Nonce :: Nonce
epoch2Nonce =
  chainCandidateNonce expectedStEx4
    ⭒ hashHeaderToNonce (bhHash $ blockHeader blockEx2)

--
-- Block 5, Slot 220, Epoch 2
--

blockEx5 :: Block (BHeader MockCrypto) ShelleyEra
blockEx5 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx4)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 220)
    []
    (SlotNo 220)
    (BlockNo 5)
    epoch2Nonce
    (NatNonce 5)
    minBound
    11
    10
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 220) 1 (KESPeriod 10))

snapEx5 :: SnapShot
snapEx5 =
  let
    stake =
      mkStake
        [ (Cast.aliceSHK, aliceCoinEx4Base <> aliceCoinEx2Ptr)
        , (Cast.carlSHK, carlMIR)
        , (Cast.bobSHK, bobInitCoin)
        ]
    delegations =
      [ (Cast.aliceSHK, aikColdKeyHash Cast.alicePoolKeys)
      , (Cast.carlSHK, aikColdKeyHash Cast.alicePoolKeys)
      , (Cast.bobSHK, aikColdKeyHash Cast.alicePoolKeys)
      ]
    poolParams = [(aikColdKeyHash Cast.alicePoolKeys, Cast.aliceStakePoolParams)]
   in
    mkSnapShot stake delegations poolParams

pdEx5 :: PoolDistr
pdEx5 =
  PoolDistr
    ( Map.singleton
        (aikColdKeyHash Cast.alicePoolKeys)
        ( IndividualPoolStake
            1
            (CompactCoin 1)
            Cast.aliceVRFKeyHash
        )
    )
    (knownNonZeroCoin @1)

expectedStEx5 :: ChainState ShelleyEra
expectedStEx5 =
  C.newEpoch blockEx5
    . C.newSnapshot snapEx5 feeTx4
    . C.applyRewardUpdate rewardUpdateEx4
    . C.setPoolDistr pdEx5
    . C.setOCertCounter coreNodeHK 1
    $ expectedStEx4
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @ShelleyEra ppEx 220

-- === Block 5, Slot 220, Epoch 2
--
-- Create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.
poolLifetime5 :: CHAINExample ShelleyEra
poolLifetime5 = CHAINExample expectedStEx4 blockEx5 (Right expectedStEx5)

--
-- Block 6, Slot 295, Epoch 2
--

blockEx6 :: Block (BHeader MockCrypto) ShelleyEra
blockEx6 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx5)
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

rewardUpdateEx6 :: RewardUpdate
rewardUpdateEx6 =
  RewardUpdate
    { deltaT = DeltaCoin 1
    , deltaR = DeltaCoin 4
    , rs = Map.empty
    , deltaF = invert $ toDeltaCoin feeTx4
    , nonMyopic = def {rewardPotNM = Coin 4}
    }

pulserEx6 :: PulsingRewUpdate
pulserEx6 = makeCompletedPulser (BlocksMade mempty) expectedStEx5

expectedStEx6 :: ChainState ShelleyEra
expectedStEx6 =
  C.evolveNonceFrozen (getBlockNonce blockEx6)
    . C.newLab blockEx6
    . C.setOCertCounter (coerceKeyRole $ aikColdKeyHash Cast.alicePoolKeys) 0
    . C.incrBlockCount (aikColdKeyHash Cast.alicePoolKeys)
    . C.pulserUpdate pulserEx6
    $ expectedStEx5

-- === Block 6, Slot 295, Epoch 2
--
-- Create a decentralized Praos block (ie one not in the overlay schedule)
poolLifetime6 :: CHAINExample ShelleyEra
poolLifetime6 = CHAINExample expectedStEx5 blockEx6 (Right (C.solidifyProposals expectedStEx6))

--
-- Block 7, Slot 310, Epoch 3
--

epoch3Nonce :: Nonce
epoch3Nonce =
  chainCandidateNonce expectedStEx6
    ⭒ hashHeaderToNonce (bhHash $ blockHeader blockEx4)

blockEx7 :: Block (BHeader MockCrypto) ShelleyEra
blockEx7 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx6)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 310)
    []
    (SlotNo 310)
    (BlockNo 7)
    epoch3Nonce
    (NatNonce 7)
    minBound
    15
    15
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 310) 1 (KESPeriod 15))

expectedStEx7 :: ChainState ShelleyEra
expectedStEx7 =
  C.newEpoch blockEx7
    . C.newSnapshot snapEx5 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx6
    . C.setOCertCounter coreNodeHK 1
    $ expectedStEx6
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @ShelleyEra ppEx 310

-- === Block 7, Slot 310, Epoch 3
--
-- Create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update
poolLifetime7 :: CHAINExample ShelleyEra
poolLifetime7 = CHAINExample expectedStEx6 blockEx7 (Right expectedStEx7)

--
-- Block 8, Slot 390, Epoch 3
--

blockEx8 :: Block (BHeader MockCrypto) ShelleyEra
blockEx8 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx7)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 390)
    []
    (SlotNo 390)
    (BlockNo 8)
    epoch3Nonce
    (NatNonce 8)
    minBound
    19
    19
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 390) 2 (KESPeriod 19))

aliceRAcnt8 :: Coin
aliceRAcnt8 = Coin 11654787878

bobRAcnt8 :: Coin
bobRAcnt8 = Coin 1038545454

deltaT8' :: Coin
deltaT8' = Coin 317333333333

deltaT8 :: DeltaCoin
deltaT8 = toDeltaCoin deltaT8'

deltaR8 :: DeltaCoin
deltaR8 = DeltaCoin (-330026666665)

reserves7 :: Coin
reserves7 = Coin 33999999999999900

rewardPot8 :: Coin
rewardPot8 = Coin 1269333333333

alicePerfEx8 :: Likelihood
alicePerfEx8 = likelihood blocks t (epochSize $ EpochNo 3)
  where
    blocks = 1
    t = leaderProbability f relativeStake (unsafeBoundRational 0.5)
    (Coin stake) = aliceCoinEx2Base <> aliceCoinEx2Ptr <> bobInitCoin
    (Coin tot) = maxLLSupply <-> reserves7
    relativeStake = fromRational (stake % tot)
    f = activeSlotCoeff testGlobals

nonMyopicEx8 :: NonMyopic
nonMyopicEx8 =
  NonMyopic
    (VMap.fromMap (Map.singleton (aikColdKeyHash Cast.alicePoolKeys) alicePerfEx8))
    rewardPot8

pulserEx8 :: PulsingRewUpdate
pulserEx8 =
  makeCompletedPulser (BlocksMade $ Map.singleton (aikColdKeyHash Cast.alicePoolKeys) 1) expectedStEx7

rewardUpdateEx8 :: RewardUpdate
rewardUpdateEx8 =
  RewardUpdate
    { deltaT = deltaT8
    , deltaR = deltaR8
    , rs =
        Map.fromList
          [
            ( Cast.aliceSHK
            , Set.singleton $ Reward LeaderReward (aikColdKeyHash Cast.alicePoolKeys) aliceRAcnt8
            )
          ,
            ( Cast.bobSHK
            , Set.singleton $ Reward MemberReward (aikColdKeyHash Cast.alicePoolKeys) bobRAcnt8
            )
          ]
    , deltaF = DeltaCoin 0
    , nonMyopic = nonMyopicEx8
    }

expectedStEx8 :: ChainState ShelleyEra
expectedStEx8 =
  C.evolveNonceFrozen (getBlockNonce blockEx8)
    . C.newLab blockEx8
    . C.setOCertCounter coreNodeHK 2
    . C.pulserUpdate pulserEx8
    $ expectedStEx7
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @ShelleyEra ppEx 390

-- === Block 8, Slot 390, Epoch 3
--
-- Create the first non-trivial reward update.
poolLifetime8 :: CHAINExample ShelleyEra
poolLifetime8 = CHAINExample expectedStEx7 blockEx8 (Right (C.solidifyProposals expectedStEx8))

--
-- Block 9, Slot 410, Epoch 4
--

epoch4Nonce :: Nonce
epoch4Nonce =
  chainCandidateNonce expectedStEx8
    ⭒ hashHeaderToNonce (bhHash $ blockHeader blockEx6)

blockEx9 :: Block (BHeader MockCrypto) ShelleyEra
blockEx9 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx8)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 410)
    []
    (SlotNo 410)
    (BlockNo 9)
    epoch4Nonce
    (NatNonce 9)
    minBound
    20
    20
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 410) 2 (KESPeriod 20))

snapEx9 :: SnapShot
snapEx9 =
  let
    stake =
      mkStake
        [ (Cast.bobSHK, bobInitCoin <> bobRAcnt8)
        , (Cast.aliceSHK, aliceCoinEx4Base <> aliceCoinEx2Ptr <> aliceRAcnt8)
        , (Cast.carlSHK, carlMIR)
        ]
    delegations = ssDelegations snapEx5
    poolParams = ssPoolParams snapEx5
   in
    mkSnapShot stake delegations poolParams

expectedStEx9 :: ChainState ShelleyEra
expectedStEx9 =
  C.newEpoch blockEx9
    . C.newSnapshot snapEx9 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx8
    . C.setOCertCounter coreNodeHK 2
    $ expectedStEx8
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @ShelleyEra ppEx 410

-- === Block 9, Slot 410, Epoch 4
--
-- Apply the first non-trivial reward update.
poolLifetime9 :: CHAINExample ShelleyEra
poolLifetime9 = CHAINExample expectedStEx8 blockEx9 (Right expectedStEx9)

--
-- Block 10, Slot 420, Epoch 4
--

feeTx10 :: Coin
feeTx10 = Coin 9

bobAda10 :: Coin
bobAda10 =
  bobRAcnt8
    <+> bobInitCoin
    <+> Coin 7
    <-> feeTx10

txbodyEx10 :: TxBody TopTx ShelleyEra
txbodyEx10 =
  ShelleyTxBody
    (Set.fromList [mkTxInPartial genesisId 1])
    (StrictSeq.singleton $ ShelleyTxOut Cast.bobAddr (Val.inject bobAda10))
    (StrictSeq.fromList [UnRegTxCert Cast.bobSHK])
    (Withdrawals $ Map.singleton (AccountAddress Testnet (AccountId Cast.bobSHK)) bobRAcnt8)
    feeTx10
    (SlotNo 500)
    SNothing
    SNothing

txEx10 :: ShelleyTx TopTx ShelleyEra
txEx10 =
  ShelleyTx
    txbodyEx10
    mempty
      { addrWits =
          mkWitnessesVKey (hashAnnotated txbodyEx10) [asWitness Cast.bobPay, asWitness Cast.bobStake]
      }
    SNothing

blockEx10 :: Block (BHeader MockCrypto) ShelleyEra
blockEx10 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx9)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 420)
    [MkShelleyTx txEx10]
    (SlotNo 420)
    (BlockNo 10)
    epoch4Nonce
    (NatNonce 10)
    minBound
    21
    19
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 420) 2 (KESPeriod 19))

expectedStEx10 :: ChainState ShelleyEra
expectedStEx10 =
  C.evolveNonceUnfrozen (getBlockNonce blockEx10)
    . C.newLab blockEx10
    . C.deregStakeCred Cast.bobSHK
    . C.addFees feeTx10
    . C.newUTxO txbodyEx10
    $ expectedStEx9

-- === Block 10, Slot 420, Epoch 4
--
-- Drain Bob's account address and de-register Bob's stake key.
poolLifetime10 :: CHAINExample ShelleyEra
poolLifetime10 = CHAINExample expectedStEx9 blockEx10 (Right expectedStEx10)

--
-- Block 11, Slot 490, Epoch 4
--

feeTx11 :: Coin
feeTx11 = Coin 2

aliceCoinEx11Ptr :: Coin
aliceCoinEx11Ptr = aliceCoinEx4Base <-> feeTx11

aliceRetireEpoch :: EpochNo
aliceRetireEpoch = EpochNo 5

txbodyEx11 :: TxBody TopTx ShelleyEra
txbodyEx11 =
  ShelleyTxBody
    (Set.fromList [TxIn (txIdTxBody txbodyEx4) minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.alicePtrAddr (Val.inject aliceCoinEx11Ptr))
    (StrictSeq.fromList [RetirePoolTxCert (aikColdKeyHash Cast.alicePoolKeys) aliceRetireEpoch])
    (Withdrawals Map.empty)
    feeTx11
    (SlotNo 500)
    SNothing
    SNothing

txEx11 :: ShelleyTx TopTx ShelleyEra
txEx11 =
  ShelleyTx
    txbodyEx11
    mempty
      { addrWits =
          mkWitnessesVKey
            (hashAnnotated txbodyEx11)
            ( [asWitness Cast.alicePay]
                <> [asWitness $ aikCold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx11 :: Block (BHeader MockCrypto) ShelleyEra
blockEx11 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx10)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 490)
    [MkShelleyTx txEx11]
    (SlotNo 490)
    (BlockNo 11)
    epoch4Nonce
    (NatNonce 11)
    minBound
    24
    19
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 490) 2 (KESPeriod 19))

reserves12 :: Coin
reserves12 = addDeltaCoin reserves7 deltaR8

alicePerfEx11 :: Likelihood
alicePerfEx11 = applyDecay decayFactor alicePerfEx8 <> epoch4Likelihood
  where
    epoch4Likelihood = likelihood blocks t (epochSize $ EpochNo 4)
    blocks = 0
    t = leaderProbability f relativeStake (unsafeBoundRational 0.5)
    -- everyone has delegated to Alice's Pool
    Coin stake = sumAllStake (ssStake snapEx5)
    relativeStake = fromRational (stake % supply)
    Coin supply = maxLLSupply <-> reserves12
    f = activeSlotCoeff testGlobals

nonMyopicEx11 :: NonMyopic
nonMyopicEx11 =
  NonMyopic
    (VMap.fromMap (Map.singleton (aikColdKeyHash Cast.alicePoolKeys) alicePerfEx11))
    (Coin 0)

pulserEx11 :: PulsingRewUpdate
pulserEx11 = makeCompletedPulser (BlocksMade mempty) expectedStEx10

rewardUpdateEx11 :: RewardUpdate
rewardUpdateEx11 =
  RewardUpdate
    { deltaT = DeltaCoin 0
    , deltaR = DeltaCoin 0
    , rs = Map.empty
    , deltaF = DeltaCoin 0
    , nonMyopic = nonMyopicEx11
    }

expectedStEx11 :: ChainState ShelleyEra
expectedStEx11 =
  C.evolveNonceFrozen (getBlockNonce blockEx11)
    . C.newLab blockEx11
    . C.addFees feeTx11
    . C.newUTxO txbodyEx11
    . C.pulserUpdate pulserEx11
    . C.stageRetirement (aikColdKeyHash Cast.alicePoolKeys) aliceRetireEpoch
    $ expectedStEx10

-- === Block 11, Slot 490, Epoch 4
--
-- Stage the retirement of Alice's stake pool.
poolLifetime11 :: CHAINExample ShelleyEra
poolLifetime11 = CHAINExample expectedStEx10 blockEx11 (Right (C.solidifyProposals expectedStEx11))

--
-- Block 12, Slot 510, Epoch 5
--

epoch5Nonce :: Nonce
epoch5Nonce =
  chainCandidateNonce expectedStEx11
    ⭒ hashHeaderToNonce (bhHash $ blockHeader blockEx8)

blockEx12 :: Block (BHeader MockCrypto) ShelleyEra
blockEx12 =
  mkBlockFakeVRF
    (bhHash $ blockHeader blockEx11)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 510)
    []
    (SlotNo 510)
    (BlockNo 12)
    epoch5Nonce
    (NatNonce 12)
    minBound
    25
    25
    (mkOCert (coreNodeKeysBySchedule @ShelleyEra ppEx 510) 3 (KESPeriod 25))

snapEx12 :: SnapShot
snapEx12 =
  let
    stake =
      mkStake
        [ (Cast.aliceSHK, aliceRAcnt8 <> aliceCoinEx2Ptr <> aliceCoinEx11Ptr)
        , (Cast.carlSHK, carlMIR)
        ]
    delegations =
      [ (Cast.aliceSHK, aikColdKeyHash Cast.alicePoolKeys)
      , (Cast.carlSHK, aikColdKeyHash Cast.alicePoolKeys)
      ]
    poolParams = ssPoolParams snapEx9
   in
    mkSnapShot stake delegations poolParams

expectedStEx12 :: ChainState ShelleyEra
expectedStEx12 =
  C.newEpoch blockEx12
    . C.newSnapshot snapEx12 (Coin 11)
    . C.applyRewardUpdate rewardUpdateEx11
    . C.setOCertCounter coreNodeHK 3
    . C.reapPool Cast.aliceStakePoolParams
    $ expectedStEx11
  where
    coreNodeHK = coerceKeyRole . aikColdKeyHash $ coreNodeKeysBySchedule @ShelleyEra ppEx 510

-- === Block 12, Slot 510, Epoch 5
--
-- Reap Alice's stake pool.
poolLifetime12 :: CHAINExample ShelleyEra
poolLifetime12 = CHAINExample expectedStEx11 blockEx12 (Right expectedStEx12)

--
-- Pool Lifetime Test Group
--

poolLifetimeExample :: TestTree
poolLifetimeExample =
  testGroup
    "pool lifetime"
    [ testCase "initial registrations" $ testCHAINExample poolLifetime1
    , testCase "delegate stake and create reward update" $ testCHAINExample poolLifetime2
    , testCase "new epoch changes" $ testCHAINExample poolLifetime3
    , testCase "second reward update" $ testCHAINExample poolLifetime4
    , testCase "nonempty pool distr" $ testCHAINExample poolLifetime5
    , testCase "decentralized block" $ testCHAINExample poolLifetime6
    , testCase "prelude to the first nontrivial rewards" $ testCHAINExample poolLifetime7
    , testCase "create a nontrivial rewards" $ testCHAINExample poolLifetime8
    , testCase "apply a nontrivial rewards" $ testCHAINExample poolLifetime9
    , testCase "drain staking address and deregister" $ testCHAINExample poolLifetime10
    , testCase "stage stake pool retirement" $ testCHAINExample poolLifetime11
    , testCase "reap stake pool" $ testCHAINExample poolLifetime12
    ]
