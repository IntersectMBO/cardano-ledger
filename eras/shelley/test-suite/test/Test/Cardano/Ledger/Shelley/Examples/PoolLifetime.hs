{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.PoolLifetime
-- Description : Pool Lifetime Example
--
-- Example demonstrating the creation of a new stake pool,
-- block production under Praos, rewards, and pool retirement.
module Test.Cardano.Ledger.Shelley.Examples.PoolLifetime
  ( makePulser,
    makePulser',
    makeCompletedPulser,
    poolLifetimeExample,
  )
where

import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    BoundedRational (..),
    Globals (..),
    Network (..),
    Nonce,
    StrictMaybe (..),
    epochInfo,
    (⭒),
  )
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, toDeltaCoin)
import Cardano.Ledger.Credential (Ptr (..))
import qualified Cardano.Ledger.Crypto as Cr
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Keys (asWitness, coerceKeyRole)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (getRewardProvenance)
import qualified Cardano.Ledger.Shelley.EpochBoundary as EB
import Cardano.Ledger.Shelley.LedgerState
  ( NewEpochState (..),
    PulsingRewUpdate (..),
    RewardUpdate (..),
    completeRupd,
    decayFactor,
    emptyRewardUpdate,
    startStep,
  )
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import qualified Cardano.Ledger.Shelley.RewardProvenance as RP
import Cardano.Ledger.Shelley.Rewards
  ( Likelihood (..),
    NonMyopic (..),
    PerformanceEstimate (unPerformanceEstimate),
    Reward (..),
    RewardType (..),
    applyDecay,
    desirability,
    leaderProbability,
    likelihood,
    percentile',
  )
import Cardano.Ledger.Shelley.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    TxBody (..),
    TxOut (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessesVKey)
import Cardano.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    EpochSize (unEpochSize),
    SlotNo (..),
    epochInfoSize,
  )
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash, hashHeaderToNonce)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Control.Provenance (runProvM)
import Data.Default.Class (def)
import Data.Foldable (fold)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto, ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
  )
import Test.Cardano.Ledger.Shelley.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
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
import Test.Cardano.Ledger.Shelley.Utils
  ( epochSize,
    getBlockNonce,
    maxLLSupply,
    runShelleyBase,
    testGlobals,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Cr.Crypto c => UTxO (ShelleyEra c)
initUTxO =
  genesisCoins
    genesisId
    [ TxOut Cast.aliceAddr (Val.inject aliceInitCoin),
      TxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStPoolLifetime :: forall c. Cr.Crypto c => ChainState (ShelleyEra c)
initStPoolLifetime = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

aliceCoinEx1 :: Coin
aliceCoinEx1 =
  aliceInitCoin <-> _poolDeposit ppEx
    <-> ((3 :: Integer) <×> _keyDeposit ppEx)
    <-> Coin 3

carlMIR :: Coin
carlMIR = Coin 110

dariaMIR :: Coin
dariaMIR = Coin 99

feeTx1 :: Coin
feeTx1 = Coin 3

txbodyEx1 :: Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    ( StrictSeq.fromList
        ( [ DCertDeleg (RegKey Cast.aliceSHK),
            DCertDeleg (RegKey Cast.bobSHK),
            DCertDeleg (RegKey Cast.carlSHK),
            DCertPool (RegPool Cast.alicePoolParams)
          ]
            ++ [ DCertMir
                   ( MIRCert
                       ReservesMIR
                       ( StakeAddressesMIR $
                           Map.fromList
                             [ (Cast.carlSHK, toDeltaCoin carlMIR),
                               (Cast.dariaSHK, toDeltaCoin dariaMIR)
                             ]
                       )
                   )
               ]
        )
    )
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx1 @c))
            ( (asWitness <$> [Cast.alicePay, Cast.carlPay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold Cast.alicePoolKeys]
                <> ( asWitness
                       <$> [ cold (coreNodeIssuerKeys 0),
                             cold (coreNodeIssuerKeys 1),
                             cold (coreNodeIssuerKeys 2),
                             cold (coreNodeIssuerKeys 3),
                             cold (coreNodeIssuerKeys 4)
                           ]
                   )
            )
      }
    SNothing

blockEx1 :: forall c. (HasCallStack, ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: forall c. (ExMock c) => ChainState (ShelleyEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @c))
    . C.newLab blockEx1
    . C.feesAndDeposits feeTx1 (((3 :: Integer) <×> _keyDeposit ppEx) <+> _poolDeposit ppEx)
    . C.newUTxO txbodyEx1
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) 0 0)
    . C.newStakeCred Cast.bobSHK (Ptr (SlotNo 10) 0 1)
    . C.newStakeCred Cast.carlSHK (Ptr (SlotNo 10) 0 2)
    . C.newPool Cast.alicePoolParams
    . C.mir Cast.carlSHK ReservesMIR carlMIR
    . C.mir Cast.dariaSHK ReservesMIR dariaMIR
    $ initStPoolLifetime

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block of this example, Alice, Bob, and Carl
-- all register stake credentials, and Alice registers a stake pool.
-- Additionally, a MIR certificate is issued to draw from the reserves
-- and give Carl and Daria (who is unregistered) rewards.
poolLifetime1 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
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
txbodyEx2 :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx2 =
  TxBody
    { _inputs = Set.fromList [TxIn (txid (txbodyEx1 @c)) 0],
      _outputs =
        StrictSeq.fromList
          [ TxOut Cast.aliceAddr (Val.inject aliceCoinEx2Base),
            TxOut Cast.alicePtrAddr (Val.inject aliceCoinEx2Ptr)
          ],
      _certs =
        StrictSeq.fromList
          [ DCertDeleg (Delegate $ Delegation Cast.aliceSHK (hk Cast.alicePoolKeys)),
            DCertDeleg (Delegate $ Delegation Cast.bobSHK (hk Cast.alicePoolKeys))
          ],
      _wdrls = Wdrl Map.empty,
      _txfee = feeTx2,
      _ttl = SlotNo 90,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txEx2 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx2 =
  Tx
    txbodyEx2
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx2 @c))
            [ asWitness Cast.alicePay,
              asWitness Cast.aliceStake,
              asWitness Cast.bobStake
            ]
      }
    SNothing

blockEx2 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx1)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 90)
    [txEx2]
    (SlotNo 90)
    (BlockNo 2)
    (nonce0 @(Crypto (ShelleyEra c)))
    (NatNonce 2)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 90) 0 (KESPeriod 0))

makePulser ::
  forall era.
  (C.UsesPP era) =>
  BlocksMade (Crypto era) ->
  ChainState era ->
  PulsingRewUpdate (Crypto era)
makePulser bs cs = p
  where
    (p, _) =
      startStep
        (epochSize $ EpochNo 0)
        bs
        (nesEs . chainNes $ cs)
        maxLLSupply
        (activeSlotCoeff testGlobals)
        (securityParameter testGlobals)

makePulser' ::
  forall era.
  (C.UsesPP era) =>
  ChainState era ->
  PulsingRewUpdate (Crypto era)
makePulser' = makePulser (BlocksMade mempty)

makeCompletedPulser ::
  forall era.
  (C.UsesPP era) =>
  BlocksMade (Crypto era) ->
  ChainState era ->
  PulsingRewUpdate (Crypto era)
makeCompletedPulser bs cs = Complete . runShelleyBase . runProvM . completeRupd $ makePulser bs cs

pulserEx2 :: forall c. (ExMock (Crypto (ShelleyEra c))) => PulsingRewUpdate c
pulserEx2 = makeCompletedPulser (BlocksMade mempty) expectedStEx1

expectedStEx2 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  ChainState (ShelleyEra c)
expectedStEx2 =
  C.evolveNonceFrozen (getBlockNonce (blockEx2 @c))
    . C.newLab blockEx2
    . C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO txbodyEx2
    . C.delegation Cast.aliceSHK (_poolId $ Cast.alicePoolParams @c)
    . C.delegation Cast.bobSHK (_poolId $ Cast.alicePoolParams @c)
    . C.pulserUpdate pulserEx2
    $ expectedStEx1

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice and Bob both delegation to Alice's Pool.
poolLifetime2 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: forall c. (ExMock (Crypto (ShelleyEra c))) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2 @c)

blockEx3 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx2)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

snapEx3 :: Cr.Crypto c => EB.SnapShot c
snapEx3 =
  EB.SnapShot
    { EB._stake =
        EB.Stake $
          Map.fromList
            [ (Cast.aliceSHK, aliceCoinEx2Base <> aliceCoinEx2Ptr),
              (Cast.bobSHK, bobInitCoin)
            ],
      EB._delegations =
        Map.fromList
          [ (Cast.aliceSHK, hk Cast.alicePoolKeys),
            (Cast.bobSHK, hk Cast.alicePoolKeys)
          ],
      EB._poolParams = Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams
    }

expectedStEx3 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  ChainState (ShelleyEra c)
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
poolLifetime3 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 190, Epoch 1
--

feeTx4 :: Coin
feeTx4 = Coin 5

aliceCoinEx4Base :: Coin
aliceCoinEx4Base = aliceCoinEx2Base <-> feeTx4

txbodyEx4 :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx4 =
  TxBody
    { _inputs = Set.fromList [TxIn (txid txbodyEx2) 0],
      _outputs = StrictSeq.fromList [TxOut Cast.aliceAddr (Val.inject aliceCoinEx4Base)],
      _certs =
        StrictSeq.fromList
          [DCertDeleg (Delegate $ Delegation Cast.carlSHK (hk Cast.alicePoolKeys))],
      _wdrls = Wdrl Map.empty,
      _txfee = feeTx4,
      _ttl = SlotNo 500,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txEx4 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx4 =
  Tx
    txbodyEx4
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx4 @c))
            [asWitness Cast.alicePay, asWitness Cast.carlStake]
      }
    SNothing

blockEx4 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx3)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 190)
    [txEx4]
    (SlotNo 190)
    (BlockNo 4)
    (epoch1Nonce @c)
    (NatNonce 4)
    minBound
    9
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 190) 0 (KESPeriod 0))

pulserEx4 :: forall c. (ExMock c) => PulsingRewUpdate c
pulserEx4 = makeCompletedPulser (BlocksMade mempty) expectedStEx3

rewardUpdateEx4 :: forall c. RewardUpdate c
rewardUpdateEx4 =
  RewardUpdate
    { deltaT = DeltaCoin 1,
      deltaR = DeltaCoin 6,
      rs = Map.empty,
      deltaF = DeltaCoin (-7),
      nonMyopic = def {rewardPotNM = Coin 6}
    }

expectedStEx4 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  ChainState (ShelleyEra c)
expectedStEx4 =
  C.evolveNonceFrozen (getBlockNonce (blockEx4 @c))
    . C.newLab blockEx4
    . C.feesAndDeposits feeTx4 (Coin 0)
    . C.newUTxO txbodyEx4
    . C.delegation Cast.carlSHK (_poolId $ Cast.alicePoolParams @c)
    . C.pulserUpdate pulserEx4
    $ expectedStEx3

-- === Block 4, Slot 190, Epoch 1
--
-- We process a block late enough in the epoch in order to create a second reward update,
-- preparing the way for the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change, Carl delegates his stake.
poolLifetime4 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime4 = CHAINExample expectedStEx3 blockEx4 (Right expectedStEx4)

epoch2Nonce :: forall c. (ExMock (Crypto (ShelleyEra c))) => Nonce
epoch2Nonce =
  chainCandidateNonce (expectedStEx4 @c)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx2 @c))

--
-- Block 5, Slot 220, Epoch 2
--

blockEx5 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx5 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx4)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 220)
    []
    (SlotNo 220)
    (BlockNo 5)
    (epoch2Nonce @c)
    (NatNonce 5)
    minBound
    11
    10
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 220) 1 (KESPeriod 10))

snapEx5 :: forall c. Cr.Crypto c => EB.SnapShot c
snapEx5 =
  EB.SnapShot
    { EB._stake =
        EB.Stake $
          Map.fromList
            [ (Cast.aliceSHK, aliceCoinEx4Base <> aliceCoinEx2Ptr),
              (Cast.carlSHK, carlMIR),
              (Cast.bobSHK, bobInitCoin)
            ],
      EB._delegations =
        Map.fromList
          [ (Cast.aliceSHK, hk Cast.alicePoolKeys),
            (Cast.carlSHK, hk Cast.alicePoolKeys),
            (Cast.bobSHK, hk Cast.alicePoolKeys)
          ],
      EB._poolParams = Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams
    }

pdEx5 :: forall c. Cr.Crypto c => PoolDistr c
pdEx5 =
  PoolDistr $
    Map.singleton
      (hk $ Cast.alicePoolKeys @c)
      (IndividualPoolStake 1 (Cast.aliceVRFKeyHash @c))

expectedStEx5 ::
  forall c.
  (ExMock (Crypto (ShelleyEra c))) =>
  ChainState (ShelleyEra c)
expectedStEx5 =
  C.newEpoch blockEx5
    . C.newSnapshot snapEx5 feeTx4
    . C.applyRewardUpdate rewardUpdateEx4
    . C.setPoolDistr pdEx5
    . C.setOCertCounter coreNodeHK 1
    $ expectedStEx4
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) ppEx 220

-- === Block 5, Slot 220, Epoch 2
--
-- Create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.
poolLifetime5 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime5 = CHAINExample expectedStEx4 blockEx5 (Right expectedStEx5)

--
-- Block 6, Slot 295, Epoch 2
--

blockEx6 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx6 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx5)
    Cast.alicePoolKeys
    []
    (SlotNo 295) -- odd slots open for decentralization
    (BlockNo 6)
    (epoch2Nonce @c)
    (NatNonce 6)
    minBound
    14
    14
    (mkOCert Cast.alicePoolKeys 0 (KESPeriod 14))

rewardUpdateEx6 :: forall c. RewardUpdate c
rewardUpdateEx6 =
  RewardUpdate
    { deltaT = DeltaCoin 1,
      deltaR = DeltaCoin 4,
      rs = Map.empty,
      deltaF = invert $ toDeltaCoin feeTx4,
      nonMyopic = def {rewardPotNM = Coin 4}
    }

pulserEx6 :: forall c. (ExMock c) => PulsingRewUpdate c
pulserEx6 = makeCompletedPulser (BlocksMade mempty) expectedStEx5

expectedStEx6 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx6 =
  C.evolveNonceFrozen (getBlockNonce (blockEx6 @c))
    . C.newLab blockEx6
    . C.setOCertCounter (coerceKeyRole $ hk Cast.alicePoolKeys) 0
    . C.incrBlockCount (hk Cast.alicePoolKeys)
    . C.pulserUpdate pulserEx6
    $ expectedStEx5

-- === Block 6, Slot 295, Epoch 2
--
-- Create a decentralized Praos block (ie one not in the overlay schedule)
poolLifetime6 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime6 = CHAINExample expectedStEx5 blockEx6 (Right expectedStEx6)

--
-- Block 7, Slot 310, Epoch 3
--

epoch3Nonce :: forall c. (ExMock (Crypto (ShelleyEra c))) => Nonce
epoch3Nonce =
  chainCandidateNonce (expectedStEx6 @c)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx4 @c))

blockEx7 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx7 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx6)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 310)
    []
    (SlotNo 310)
    (BlockNo 7)
    (epoch3Nonce @c)
    (NatNonce 7)
    minBound
    15
    15
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 310) 1 (KESPeriod 15))

expectedStEx7 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx7 =
  C.newEpoch blockEx7
    . C.newSnapshot snapEx5 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx6
    . C.setOCertCounter coreNodeHK 1
    $ expectedStEx6
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) ppEx 310

-- === Block 7, Slot 310, Epoch 3
--
-- Create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update
poolLifetime7 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime7 = CHAINExample expectedStEx6 blockEx7 (Right expectedStEx7)

--
-- Block 8, Slot 390, Epoch 3
--

blockEx8 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx8 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx7)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 390)
    []
    (SlotNo 390)
    (BlockNo 8)
    (epoch3Nonce @c)
    (NatNonce 8)
    minBound
    19
    19
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 390) 2 (KESPeriod 19))

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
    t = leaderProbability f relativeStake (_d ppEx)
    (Coin stake) = aliceCoinEx2Base <> aliceCoinEx2Ptr <> bobInitCoin
    (Coin tot) = maxLLSupply <-> reserves7
    relativeStake = fromRational (stake % tot)
    f = activeSlotCoeff testGlobals

nonMyopicEx8 :: forall c. Cr.Crypto c => NonMyopic c
nonMyopicEx8 =
  NonMyopic
    (Map.singleton (hk Cast.alicePoolKeys) alicePerfEx8)
    rewardPot8

pulserEx8 :: forall c. (ExMock c) => PulsingRewUpdate c
pulserEx8 = makeCompletedPulser (BlocksMade $ Map.singleton (hk Cast.alicePoolKeys) 1) expectedStEx7

rewardUpdateEx8 :: forall c. Cr.Crypto c => RewardUpdate c
rewardUpdateEx8 =
  RewardUpdate
    { deltaT = deltaT8,
      deltaR = deltaR8,
      rs =
        Map.fromList
          [ ( Cast.aliceSHK,
              Set.singleton $ Reward LeaderReward (hk Cast.alicePoolKeys) aliceRAcnt8
            ),
            ( Cast.bobSHK,
              Set.singleton $ Reward MemberReward (hk Cast.alicePoolKeys) bobRAcnt8
            )
          ],
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx8
    }

expectedStEx8 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx8 =
  C.evolveNonceFrozen (getBlockNonce (blockEx8 @c))
    . C.newLab blockEx8
    . C.setOCertCounter coreNodeHK 2
    . C.pulserUpdate pulserEx8
    $ expectedStEx7
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) ppEx 390

-- === Block 8, Slot 390, Epoch 3
--
-- Create the first non-trivial reward update.
poolLifetime8 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime8 = CHAINExample expectedStEx7 blockEx8 (Right expectedStEx8)

rewardInfoEx8 :: RP.RewardProvenance C_Crypto
rewardInfoEx8 = snd $ getRewardProvenance testGlobals (chainNes expectedStEx8)

rewardInfoTest :: Assertion
rewardInfoTest = rewardInfoEx8 @?= expected
  where
    expected =
      RP.RewardProvenance
        { RP.spe = unEpochSize . runShelleyBase $ epochInfoSize (epochInfo testGlobals) (EpochNo 0),
          RP.blocks =
            BlocksMade $
              Map.singleton (_poolId $ Cast.alicePoolParams) 1,
          RP.maxLL = supply,
          RP.deltaR1 = rpot,
          RP.deltaR2 = Coin 1256640000001,
          RP.r = rewardPot8,
          RP.totalStake = totstake,
          RP.blocksCount = 1,
          RP.d = unboundRational $ _d ppEx,
          RP.expBlocks = 45,
          RP.eta = 1 % 45,
          RP.rPot = rpot,
          RP.deltaT1 = deltaT8',
          RP.activeStake = activestake,
          RP.pools =
            Map.singleton
              (_poolId $ Cast.alicePoolParams)
              ( RP.RewardProvenancePool
                  { RP.poolBlocksP = 1,
                    RP.sigmaP = (unCoin activestake) % (unCoin totstake),
                    RP.sigmaAP = 1,
                    RP.ownerStakeP = aliceCoinEx2Base <> aliceCoinEx2Ptr,
                    RP.poolParamsP = Cast.alicePoolParams,
                    RP.pledgeRatioP =
                      (unCoin . _poolPledge $ Cast.alicePoolParams @C_Crypto)
                        % (unCoin totstake),
                    RP.maxPP = Coin 12693333333,
                    RP.appPerfP = 1,
                    RP.poolRP = Coin 12693333333,
                    RP.lRewardP = aliceRAcnt8
                  }
              ),
          RP.desirabilities =
            Map.singleton
              (_poolId $ Cast.alicePoolParams)
              ( RP.Desirability
                  { RP.hitRateEstimate = unPerformanceEstimate estimate,
                    RP.desirabilityScore =
                      desirability
                        (_a0 ppEx, _nOpt ppEx)
                        rpot
                        (Cast.alicePoolParams @C_Crypto)
                        estimate
                        totstake
                  }
              )
        }
    rpot = Coin 1586666666666
    estimate = percentile' alicePerfEx8
    supply = Coin . fromIntegral . maxLovelaceSupply $ testGlobals
    totstake = supply <-> reserves7
    activestake = aliceCoinEx2Base <> aliceCoinEx2Ptr <> bobInitCoin

--
-- Block 9, Slot 410, Epoch 4
--

epoch4Nonce :: forall c. (ExMock (Crypto (ShelleyEra c))) => Nonce
epoch4Nonce =
  chainCandidateNonce (expectedStEx8 @c)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx6 @c))

blockEx9 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx9 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx8)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 410)
    []
    (SlotNo 410)
    (BlockNo 9)
    (epoch4Nonce @c)
    (NatNonce 9)
    minBound
    20
    20
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 410) 2 (KESPeriod 20))

snapEx9 :: forall c. Cr.Crypto c => EB.SnapShot c
snapEx9 =
  snapEx5
    { EB._stake =
        EB.Stake $
          Map.fromList
            [ (Cast.bobSHK, bobInitCoin <> bobRAcnt8),
              (Cast.aliceSHK, aliceCoinEx4Base <> aliceCoinEx2Ptr <> aliceRAcnt8),
              (Cast.carlSHK, carlMIR)
            ]
    }

expectedStEx9 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx9 =
  C.newEpoch blockEx9
    . C.newSnapshot snapEx9 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx8
    . C.setOCertCounter coreNodeHK 2
    $ expectedStEx8
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) ppEx 410

-- === Block 9, Slot 410, Epoch 4
--
-- Apply the first non-trivial reward update.
poolLifetime9 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
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
    <+> _keyDeposit ppEx
    <-> feeTx10

txbodyEx10 :: Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx10 =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.singleton $ TxOut Cast.bobAddr (Val.inject bobAda10))
    (StrictSeq.fromList [DCertDeleg (DeRegKey Cast.bobSHK)])
    (Wdrl $ Map.singleton (RewardAcnt Testnet Cast.bobSHK) bobRAcnt8)
    feeTx10
    (SlotNo 500)
    SNothing
    SNothing

txEx10 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx10 =
  Tx
    txbodyEx10
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated (txbodyEx10 @c)) [asWitness Cast.bobPay, asWitness Cast.bobStake]
      }
    SNothing

blockEx10 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx10 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx9)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 420)
    [txEx10]
    (SlotNo 420)
    (BlockNo 10)
    (epoch4Nonce @c)
    (NatNonce 10)
    minBound
    21
    19
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 420) 2 (KESPeriod 19))

expectedStEx10 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx10 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx10 @c))
    . C.newLab blockEx10
    . C.feesAndDeposits feeTx10 (invert (_keyDeposit ppEx))
    . C.deregStakeCred Cast.bobSHK
    . C.newUTxO txbodyEx10
    $ expectedStEx9

-- === Block 10, Slot 420, Epoch 4
--
-- Drain Bob's reward account and de-register Bob's stake key.
poolLifetime10 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
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

txbodyEx11 :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbodyEx11 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx4) 0])
    (StrictSeq.singleton $ TxOut Cast.alicePtrAddr (Val.inject aliceCoinEx11Ptr))
    (StrictSeq.fromList [DCertPool (RetirePool (hk Cast.alicePoolKeys) aliceRetireEpoch)])
    (Wdrl Map.empty)
    feeTx11
    (SlotNo 500)
    SNothing
    SNothing

txEx11 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Tx (ShelleyEra c)
txEx11 =
  Tx
    txbodyEx11
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx11 @c))
            ( [asWitness Cast.alicePay]
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx11 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx11 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx10)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 490)
    [txEx11]
    (SlotNo 490)
    (BlockNo 11)
    (epoch4Nonce @c)
    (NatNonce 11)
    minBound
    24
    19
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 490) 2 (KESPeriod 19))

reserves12 :: Coin
reserves12 = addDeltaCoin reserves7 deltaR8

alicePerfEx11 :: forall c. Cr.Crypto c => Likelihood
alicePerfEx11 = applyDecay decayFactor alicePerfEx8 <> epoch4Likelihood
  where
    epoch4Likelihood = likelihood blocks t (epochSize $ EpochNo 4)
    blocks = 0
    t = leaderProbability f relativeStake (_d ppEx)
    (Coin stake) = fold (EB.unStake . EB._stake $ snapEx5 @c) -- everyone has delegated to Alice's Pool
    relativeStake = fromRational (stake % supply)
    (Coin supply) = maxLLSupply <-> reserves12
    f = activeSlotCoeff testGlobals

nonMyopicEx11 :: forall c. Cr.Crypto c => NonMyopic c
nonMyopicEx11 =
  NonMyopic
    (Map.singleton (hk Cast.alicePoolKeys) (alicePerfEx11 @c))
    (Coin 0)

pulserEx11 :: forall c. (ExMock c) => PulsingRewUpdate c
pulserEx11 = makeCompletedPulser (BlocksMade mempty) expectedStEx10

rewardUpdateEx11 :: forall c. Cr.Crypto c => RewardUpdate c
rewardUpdateEx11 =
  RewardUpdate
    { deltaT = DeltaCoin 0,
      deltaR = DeltaCoin 0,
      rs = Map.empty,
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx11
    }

expectedStEx11 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx11 =
  C.evolveNonceFrozen (getBlockNonce (blockEx11 @c))
    . C.newLab blockEx11
    . C.feesAndDeposits feeTx11 (Coin 0)
    . C.newUTxO txbodyEx11
    . C.pulserUpdate pulserEx11
    . C.stageRetirement (hk Cast.alicePoolKeys) aliceRetireEpoch
    $ expectedStEx10

-- === Block 11, Slot 490, Epoch 4
--
-- Stage the retirement of Alice's stake pool.
poolLifetime11 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime11 = CHAINExample expectedStEx10 blockEx11 (Right expectedStEx11)

--
-- Block 12, Slot 510, Epoch 5
--

epoch5Nonce :: forall c. (ExMock (Crypto (ShelleyEra c))) => Nonce
epoch5Nonce =
  chainCandidateNonce (expectedStEx11 @c)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx8 @c))

blockEx12 :: forall c. (ExMock (Crypto (ShelleyEra c))) => Block BHeader (ShelleyEra c)
blockEx12 =
  mkBlockFakeVRF
    (bhHash $ bheader @BHeader @(ShelleyEra c) blockEx11)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 510)
    []
    (SlotNo 510)
    (BlockNo 12)
    (epoch5Nonce @c)
    (NatNonce 12)
    minBound
    25
    25
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 510) 3 (KESPeriod 25))

snapEx12 :: forall c. Cr.Crypto c => EB.SnapShot c
snapEx12 =
  snapEx9
    { EB._stake =
        EB.Stake $
          Map.fromList
            [ (Cast.aliceSHK, aliceRAcnt8 <> aliceCoinEx2Ptr <> aliceCoinEx11Ptr),
              (Cast.carlSHK, carlMIR)
            ],
      EB._delegations =
        Map.fromList
          [ (Cast.aliceSHK, hk Cast.alicePoolKeys),
            (Cast.carlSHK, hk Cast.alicePoolKeys)
          ]
    }

expectedStEx12 :: forall c. (ExMock (Crypto (ShelleyEra c))) => ChainState (ShelleyEra c)
expectedStEx12 =
  C.newEpoch blockEx12
    . C.newSnapshot snapEx12 (Coin 11)
    . C.applyRewardUpdate rewardUpdateEx11
    . C.setOCertCounter coreNodeHK 3
    . C.reapPool Cast.alicePoolParams
    $ expectedStEx11
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) ppEx 510

-- === Block 12, Slot 510, Epoch 5
--
-- Reap Alice's stake pool.
poolLifetime12 :: (ExMock (Crypto (ShelleyEra c))) => CHAINExample BHeader (ShelleyEra c)
poolLifetime12 = CHAINExample expectedStEx11 blockEx12 (Right expectedStEx12)

--
-- Pool Lifetime Test Group
--

poolLifetimeExample :: TestTree
poolLifetimeExample =
  testGroup
    "pool lifetime"
    [ testCase "initial registrations" $ testCHAINExample poolLifetime1,
      testCase "delegate stake and create reward update" $ testCHAINExample poolLifetime2,
      testCase "new epoch changes" $ testCHAINExample poolLifetime3,
      testCase "second reward update" $ testCHAINExample poolLifetime4,
      testCase "nonempty pool distr" $ testCHAINExample poolLifetime5,
      testCase "decentralized block" $ testCHAINExample poolLifetime6,
      testCase "prelude to the first nontrivial rewards" $ testCHAINExample poolLifetime7,
      testCase "create a nontrivial rewards" $ testCHAINExample poolLifetime8,
      testCase "apply a nontrivial rewards" $ testCHAINExample poolLifetime9,
      testCase "drain reward account and deregister" $ testCHAINExample poolLifetime10,
      testCase "stage stake pool retirement" $ testCHAINExample poolLifetime11,
      testCase "reap stake pool" $ testCHAINExample poolLifetime12,
      testCase "reward info" rewardInfoTest
    ]
