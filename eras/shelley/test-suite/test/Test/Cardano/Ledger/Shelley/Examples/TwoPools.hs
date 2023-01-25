{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.TwoPools
-- Description : Two Pools Example
--
-- Example demonstrating a particular delegation scenario involving
-- two pools. Both pools select a reward account which is *not*
-- a pool owner, and which delegates to one of the pools.
module Test.Cardano.Ledger.Shelley.Examples.TwoPools
  ( twoPoolsExample,
    twoPoolsExampleExtended,
  )
where

import Cardano.Binary (ToCBOR)
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    BoundedRational (..),
    Globals (..),
    Network (..),
    Nonce,
    ProtVer (..),
    StrictMaybe (..),
    activeSlotVal,
    mkCertIxPartial,
    (⭒),
  )
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin
  ( Coin (..),
    DeltaCoin (..),
    rationalToCoinViaFloor,
    toDeltaCoin,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential, Ptr (..))
import qualified Cardano.Ledger.Crypto as CryptoClass
import qualified Cardano.Protocol.HeaderCrypto as CryptoClass
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Keys (KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.PoolDistr
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import qualified Cardano.Ledger.Shelley.EpochBoundary as EB
import Cardano.Ledger.Shelley.LedgerState
  ( PulsingRewUpdate (..),
    RewardUpdate (..),
    completeStep,
    emptyRewardUpdate,
  )
import Cardano.Ledger.Shelley.PParams
  ( ShelleyPParams,
    ShelleyPParamsHKD (..),
  )
import Cardano.Ledger.Shelley.PoolRank
  ( Likelihood (..),
    NonMyopic (..),
    leaderProbability,
    likelihood,
  )
import Cardano.Ledger.Shelley.Rewards
  ( Reward (..),
    RewardType (..),
    StakeShare (..),
    aggregateRewards,
    leaderRew,
    memberRew,
    mkApparentPerformance,
    sumRewards,
  )
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx (..),
    WitnessSetHKD (..),
  )
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    ShelleyTxBody (..),
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
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash, hashHeaderToNonce)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Protocol.TPraos.Rules.Overlay (toPoolStakeVRF)
import Data.Default.Class (def)
import Data.Group (invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto, ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation
  ( coreNodeKeysBySchedule,
  )
import Test.Cardano.Ledger.Shelley.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makeCompletedPulser, mkStake)
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( AllIssuerKeys (..),
    NatNonce (..),
    PreAlonzo,
    genesisCoins,
    mkBlockFakeVRF,
    mkOCert,
  )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils
  ( ShelleyTest,
    epochSize,
    getBlockNonce,
    maxLLSupply,
    runShelleyBase,
    testGlobals,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

-- | Type local to this module expressing the various constraints assumed
-- amongst all tests in this module.
type TwoPoolsConstraints era hc =
  ( ShelleyTest era,
    ExMock (Crypto era) hc,
    Core.TxBody era ~ ShelleyTxBody era,
    Core.Tx era ~ ShelleyTx era,
    -- Core.PParams (Crypto era) ~ ShelleyPParams (Crypto era),
    Core.PParams era ~ ShelleyPParams era,
    PreAlonzo era,
    Core.EraSegWits era,
    ToCBOR (Core.Script era)
  )

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

carlInitCoin :: Coin
carlInitCoin = Coin $ 5 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: forall era hc. TwoPoolsConstraints era hc => UTxO era
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin),
      ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin),
      ShelleyTxOut Cast.carlAddr (Val.inject carlInitCoin)
    ]

initStTwoPools :: forall era hc. (TwoPoolsConstraints era hc) => ChainState era
initStTwoPools = initSt @era @hc (initUTxO @era @hc)

--
-- Block 1, Slot 10, Epoch 0
--

aliceCoinEx1 :: Coin
aliceCoinEx1 =
  aliceInitCoin
    <-> ((2 :: Integer) <×> _poolDeposit ppEx)
    <-> ((3 :: Integer) <×> _keyDeposit ppEx)
    <-> feeTx1

feeTx1 :: Coin
feeTx1 = Coin 3

alicePoolParams' ::
  forall c hc.
  (CryptoClass.Crypto c, CryptoClass.HeaderCrypto hc) =>
  PoolParams c
alicePoolParams' = (Cast.alicePoolParams @c @hc) {_poolRAcnt = RewardAcnt Testnet Cast.carlSHK}

bobPoolParams' ::
  forall c hc.
  (CryptoClass.Crypto c, CryptoClass.HeaderCrypto hc) =>
  PoolParams c
bobPoolParams' = (Cast.bobPoolParams @c @hc) {_poolRAcnt = RewardAcnt Testnet Cast.carlSHK}

txbodyEx1 :: forall era hc. TwoPoolsConstraints era hc => ShelleyTxBody era
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    ( StrictSeq.fromList
        [ DCertDeleg (RegKey Cast.aliceSHK),
          DCertDeleg (RegKey Cast.bobSHK),
          DCertDeleg (RegKey Cast.carlSHK),
          DCertPool (RegPool $ alicePoolParams' @(Crypto era) @hc),
          DCertPool (RegPool $ bobPoolParams' @(Crypto era) @hc),
          DCertDeleg (Delegate $ Delegation Cast.aliceSHK (hk $ Cast.alicePoolKeys @(Crypto era) @hc)),
          DCertDeleg (Delegate $ Delegation Cast.bobSHK (hk $ Cast.bobPoolKeys @(Crypto era) @hc)),
          DCertDeleg (Delegate $ Delegation Cast.carlSHK (hk $ Cast.alicePoolKeys @(Crypto era) @hc))
        ]
    )
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    SNothing
    SNothing

txEx1 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  ShelleyTx era
txEx1 =
  ShelleyTx
    (txbodyEx1 @era @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @era @hc)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake, Cast.bobStake, Cast.carlStake])
                <> (asWitness <$> [cold $ Cast.alicePoolKeys @(Crypto era) @hc, cold $ Cast.bobPoolKeys @(Crypto era) @hc])
            )
      }
    SNothing

blockEx1 ::
  forall era hc.
  ( HasCallStack,
    TwoPoolsConstraints era hc
  ) =>
  Block (BHeader (Crypto era) hc) era
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @era ppEx 10)
    [txEx1 @era @hc]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto era))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 10) 0 (KESPeriod 0))

expectedStEx1 ::
  forall era hc.
  ( TwoPoolsConstraints era hc) =>
  ChainState era
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @era @hc))
    . C.newLab (blockEx1 @era @hc)
    . C.feesAndDeposits feeTx1 (((3 :: Integer) <×> _keyDeposit ppEx) <+> ((2 :: Integer) <×> _poolDeposit ppEx))
    . C.newUTxO (txbodyEx1 @era @hc)
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 0))
    . C.newStakeCred Cast.bobSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 1))
    . C.newStakeCred Cast.carlSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 2))
    . C.newPool (alicePoolParams' @(Crypto era) @hc)
    . C.newPool (bobPoolParams' @(Crypto era) @hc)
    . C.delegation Cast.aliceSHK (_poolId $ alicePoolParams' @(Crypto era) @hc)
    . C.delegation Cast.bobSHK (_poolId $ bobPoolParams' @(Crypto era) @hc)
    . C.delegation Cast.carlSHK (_poolId $ alicePoolParams' @(Crypto era) @hc)
    $ (initStTwoPools @era @hc)

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block of this example, Alice, Bob, and Carl register
-- stake credentials, Alice and Bob register stake pools,
-- Alice and Carl delegate to Alice's pool, and Bob delegates to Bob's pool.
--
-- This is the only block in this example that includes a transaction,
-- and after this block is processed, the UTxO will consist entirely
-- of Alice's new coin aliceCoinEx1, and Bob and Carls initial genesis coins.
twoPools1 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools1 = CHAINExample (initStTwoPools @era @hc) blockEx1 (Right $ expectedStEx1 @era @hc)

--
-- Block 2, Slot 90, Epoch 0
--

blockEx2 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  Block (BHeader (Crypto era) hc) era
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx1)
    (coreNodeKeysBySchedule @era ppEx 90)
    []
    (SlotNo 90)
    (BlockNo 2)
    (nonce0 @(Crypto era))
    (NatNonce 2)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 90) 0 (KESPeriod 0))

expectedStEx2 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  ChainState era
expectedStEx2 =
  C.evolveNonceFrozen (getBlockNonce (blockEx2 @era @hc))
    . C.newLab (blockEx2 @era @hc)
    . C.rewardUpdate emptyRewardUpdate
    $ (expectedStEx1 @era @hc)

-- === Block 2, Slot 90, Epoch 0
--
-- Create an empty block near the end of epoch 0 to close out the epoch.
twoPools2 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  CHAINExample (BHeader (Crypto era) hc) era hc
twoPools2 = CHAINExample (expectedStEx1 @era @hc) blockEx2 (Right $ expectedStEx2 @era @hc)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2 @era @hc)

blockEx3 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  Block (BHeader (Crypto era) hc) era
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx2)
    (coreNodeKeysBySchedule @era ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @era @hc)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 110) 0 (KESPeriod 0))

snapEx3 :: forall c hc. ExMock c hc => EB.SnapShot c
snapEx3 =
  EB.SnapShot
    { EB._stake =
        mkStake
          [ (Cast.aliceSHK, aliceCoinEx1),
            (Cast.bobSHK, bobInitCoin),
            (Cast.carlSHK, carlInitCoin)
          ],
      EB._delegations =
        [ (Cast.aliceSHK, hk (Cast.alicePoolKeys @c @hc)),
          (Cast.bobSHK, hk (Cast.bobPoolKeys @c @hc)),
          (Cast.carlSHK, hk (Cast.alicePoolKeys @c @hc))
        ],
      EB._poolParams =
        [ (hk (Cast.alicePoolKeys @c @hc), alicePoolParams' @c @hc),
          (hk (Cast.bobPoolKeys @c @hc), bobPoolParams' @c @hc)
        ]
    }

expectedStEx3 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  ChainState era
expectedStEx3 =
  C.newEpoch (blockEx3 @era @hc)
    . C.newSnapshot (snapEx3 @(Crypto era) @hc) feeTx1
    . C.applyRewardUpdate emptyRewardUpdate
    $ (expectedStEx2 @era @hc)

-- === Block 3, Slot 110, Epoch 1
--
-- Create an empty block at the begining of epoch 1 to trigger the epoch change.
twoPools3 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  CHAINExample (BHeader (Crypto era) hc) era hc
twoPools3 = CHAINExample (expectedStEx2 @era @hc) blockEx3 (Right $ expectedStEx3 @era @hc)

--
-- Block 4, Slot 190, Epoch 1
--

blockEx4 ::
  forall era hc.
  ( TwoPoolsConstraints era hc
  ) =>
  Block (BHeader (Crypto era) hc) era
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx3)
    (coreNodeKeysBySchedule @era ppEx 190)
    []
    (SlotNo 190)
    (BlockNo 4)
    (epoch1Nonce @era @hc)
    (NatNonce 4)
    minBound
    9
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 190) 0 (KESPeriod 0))

deltaREx4 :: Coin
deltaREx4 = Coin 3

rewardUpdateEx4 :: forall crypto. RewardUpdate crypto
rewardUpdateEx4 =
  RewardUpdate
    { deltaT = DeltaCoin 0,
      deltaR = toDeltaCoin deltaREx4, -- No rewards paid out, fees go to reserves
      rs = Map.empty,
      deltaF = invert $ toDeltaCoin feeTx1,
      nonMyopic = def {rewardPotNM = feeTx1}
    }

expectedStEx4 ::
  forall era hc.
  (TwoPoolsConstraints era hc) =>
  ChainState era
expectedStEx4 =
  C.evolveNonceFrozen (getBlockNonce (blockEx4 @era @hc))
    . C.newLab (blockEx4 @era @hc)
    . C.rewardUpdate rewardUpdateEx4
    $ (expectedStEx3 @era @hc)

-- === Block 4, Slot 190, Epoch 1
--
-- Create an empty block near the end of epoch 0 to close out the epoch,
-- preparing the way for the first non-empty pool distribution.
twoPools4 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools4 = CHAINExample (expectedStEx3 @era @hc) blockEx4 (Right $ expectedStEx4 @era @hc)

epoch2Nonce :: forall era hc. (TwoPoolsConstraints era hc) => Nonce
epoch2Nonce =
  chainCandidateNonce (expectedStEx4 @era @hc)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx2 @era @hc))

--
-- Block 5, Slot 221, Epoch 2
--

blockEx5 :: forall era hc. (TwoPoolsConstraints era hc) => Block (BHeader (Crypto era) hc) era
blockEx5 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx4)
    Cast.alicePoolKeys
    []
    (SlotNo 221) -- odd slots open for decentralization
    (BlockNo 5)
    (epoch2Nonce @era @hc)
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

pdEx5 ::
  forall c hc.
  (CryptoClass.Crypto c, CryptoClass.HeaderCrypto hc) =>
  PoolDistr c
pdEx5 =
  PoolDistr $
    Map.fromList
      [ ( hk $ Cast.alicePoolKeys @c @hc,
          IndividualPoolStake alicePoolStake (toPoolStakeVRF $ Cast.aliceVRFKeyHash @c @hc)
        ),
        ( hk $ Cast.bobPoolKeys @c @hc,
          IndividualPoolStake bobPoolStake (toPoolStakeVRF $ Cast.bobVRFKeyHash @c @hc)
        )
      ]

expectedStEx5 ::
  forall era hc.
  (TwoPoolsConstraints era hc) =>
  ChainState era
expectedStEx5 =
  C.incrBlockCount (hk $ Cast.alicePoolKeys @(Crypto era) @hc)
    . C.newSnapshot (snapEx3 @(Crypto era) @hc) (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx4
    . C.setPoolDistr (pdEx5 @(Crypto era) @hc)
    . C.setOCertCounter (coerceKeyRole $ hk (Cast.alicePoolKeys @(Crypto era) @hc)) 0
    . C.newEpoch (blockEx5 @era @hc) -- This must be processed before the incrBlockCount
    $ (expectedStEx4 @era @hc)

-- === Block 5, Slot 220, Epoch 2
--
-- Create the first non-empty pool distribution by starting the epoch 2.
-- Moreover, Alice's pool produces the block.
twoPools5 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools5 = CHAINExample (expectedStEx4 @era @hc) blockEx5 (Right $ expectedStEx5 @era @hc)

--
-- Block 6, Slot 295, Epoch 2
--

blockEx6 :: forall era hc. (TwoPoolsConstraints era hc) => Block (BHeader (Crypto era) hc) era
blockEx6 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx5)
    Cast.alicePoolKeys
    []
    (SlotNo 295) -- odd slots open for decentralization
    (BlockNo 6)
    (epoch2Nonce @era @hc)
    (NatNonce 6)
    minBound
    14
    14
    (mkOCert Cast.alicePoolKeys 0 (KESPeriod 14))

expectedStEx6 :: forall era hc. (TwoPoolsConstraints era hc) => ChainState era
expectedStEx6 =
  C.evolveNonceFrozen (getBlockNonce (blockEx6 @era @hc))
    . C.newLab (blockEx6 @era @hc)
    . C.setOCertCounter (coerceKeyRole $ hk (Cast.alicePoolKeys @(Crypto era) @hc)) 0
    . C.incrBlockCount (hk (Cast.alicePoolKeys @(Crypto era) @hc))
    . C.rewardUpdate emptyRewardUpdate
    $ (expectedStEx5 @era @hc)

-- === Block 6, Slot 295, Epoch 2
--
-- Alice's pool produces a second block.
twoPools6 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools6 = CHAINExample (expectedStEx5 @era @hc) blockEx6 (Right $ expectedStEx6 @era @hc)

--
-- Block 7, Slot 297, Epoch 2
--

blockEx7 :: forall era hc. (TwoPoolsConstraints era hc) => Block (BHeader (Crypto era) hc) era
blockEx7 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx6)
    Cast.bobPoolKeys
    []
    (SlotNo 297) -- odd slots open for decentralization
    (BlockNo 7)
    (epoch2Nonce @era @hc)
    (NatNonce 7)
    minBound
    14
    14
    (mkOCert Cast.bobPoolKeys 0 (KESPeriod 14))

expectedStEx7 :: forall era hc. (TwoPoolsConstraints era hc) => ChainState era
expectedStEx7 =
  C.evolveNonceFrozen (getBlockNonce (blockEx7 @era @hc))
    . C.newLab (blockEx7 @era @hc)
    . C.setOCertCounter (coerceKeyRole $ hk (Cast.bobPoolKeys @(Crypto era) @hc)) 0
    . C.incrBlockCount (hk (Cast.bobPoolKeys @(Crypto era) @hc))
    $ (expectedStEx6 @era @hc)

-- === Block 7, Slot 295, Epoch 2
--
-- Bob's pool produces a block.
twoPools7 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools7 = CHAINExample (expectedStEx6 @era @hc) blockEx7 (Right $ expectedStEx7 @era @hc)

--
-- Block 8, Slot 310, Epoch 3
--

epoch3Nonce :: forall era hc. (TwoPoolsConstraints era hc) => Nonce
epoch3Nonce =
  chainCandidateNonce (expectedStEx7 @era @hc)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx4 @era @hc))

blockEx8 :: forall era hc. (TwoPoolsConstraints era hc) => Block (BHeader (Crypto era) hc) era
blockEx8 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx7)
    (coreNodeKeysBySchedule @era ppEx 310)
    []
    (SlotNo 310)
    (BlockNo 8)
    (epoch3Nonce @era @hc)
    (NatNonce 8)
    minBound
    15
    15
    (mkOCert (coreNodeKeysBySchedule @era ppEx 310) 1 (KESPeriod 15))

expectedStEx8 :: forall era hc. (TwoPoolsConstraints era hc) => ChainState era
expectedStEx8 =
  C.newEpoch (blockEx8 @era @hc)
    . C.newSnapshot (snapEx3 @(Crypto era) @hc) (Coin 0)
    . C.setOCertCounter coreNodeHK 1
    . C.applyRewardUpdate emptyRewardUpdate
    $ (expectedStEx7 @era @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era @hc ppEx 310

-- === Block 8, Slot 310, Epoch 3
--
-- Create an empty block to start epoch 3.
twoPools8 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools8 = CHAINExample (expectedStEx7 @era @hc) blockEx8 (Right (expectedStEx8 @era @hc))

--
-- Block 9, Slot 390, Epoch 3
--

blockEx9 :: forall era hc. (TwoPoolsConstraints era hc) => Block (BHeader (Crypto era) hc) era
blockEx9 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader (Crypto era) hc) @era blockEx8)
    (coreNodeKeysBySchedule @era ppEx 390)
    []
    (SlotNo 390)
    (BlockNo 9)
    (epoch3Nonce @era @hc)
    (NatNonce 9)
    minBound
    19
    19
    (mkOCert (coreNodeKeysBySchedule @era ppEx 390) 2 (KESPeriod 19))

blocksMadeEpoch3 :: Integer
blocksMadeEpoch3 = 3

expectedBlocks :: Integer
expectedBlocks =
  floor $
    (1 - (unboundRational . _d $ ppEx))
      * unboundRational (activeSlotVal $ activeSlotCoeff testGlobals)
      * fromIntegral (epochSize $ EpochNo 3)

reserves9 :: Coin
reserves9 = maxLLSupply <+> deltaREx4 <-> (aliceInitCoin <+> bobInitCoin <+> carlInitCoin)

deltaR1Ex9 :: Coin
deltaR1Ex9 =
  rationalToCoinViaFloor $
    (blocksMadeEpoch3 % expectedBlocks)
      * (unboundRational $ _rho ppEx)
      * (fromIntegral . unCoin $ reserves9)

rPot :: Integer
rPot = unCoin deltaR1Ex9 -- There were no fees

deltaTEx9 :: Integer
deltaTEx9 = floor $ unboundRational (_tau ppEx) * fromIntegral rPot

bigR :: Coin
bigR = Coin $ rPot - deltaTEx9

circulation :: Integer
circulation = unCoin $ maxLLSupply <-> reserves9

aliceStakeShareTot :: Rational
aliceStakeShareTot = (unCoin aliceCoinEx1 + unCoin carlInitCoin) % circulation

bobStakeShareTot :: Rational
bobStakeShareTot = unCoin bobInitCoin % circulation

alicePoolRewards :: forall c hc. ExMock c hc => Coin
alicePoolRewards = rationalToCoinViaFloor (appPerf * (fromIntegral . unCoin $ maxP))
  where
    appPerf = mkApparentPerformance (_d ppEx) alicePoolStake 2 3
    pledge = fromIntegral . unCoin . _poolPledge $ alicePoolParams' @c @hc
    pr = pledge % circulation
    maxP = EB.maxPool ppEx bigR aliceStakeShareTot pr

carlMemberRewardsFromAlice :: forall c hc. ExMock c hc => Coin
carlMemberRewardsFromAlice =
  memberRew
    (alicePoolRewards @c @hc)
    (alicePoolParams' @c @hc)
    (StakeShare $ unCoin carlInitCoin % circulation)
    (StakeShare aliceStakeShareTot)

carlLeaderRewardsFromAlice :: forall c hc. ExMock c hc => Coin
carlLeaderRewardsFromAlice =
  leaderRew
    (alicePoolRewards @c @hc)
    (alicePoolParams' @c @hc)
    (StakeShare $ unCoin aliceCoinEx1 % circulation)
    (StakeShare aliceStakeShareTot)

bobPoolRewards :: forall c hc. ExMock c hc => Coin
bobPoolRewards = rationalToCoinViaFloor (appPerf * (fromIntegral . unCoin $ maxP))
  where
    appPerf = mkApparentPerformance (_d ppEx) bobPoolStake 1 3
    pledge = fromIntegral . unCoin . _poolPledge $ bobPoolParams' @c @hc
    pr = pledge % circulation
    maxP = EB.maxPool ppEx bigR bobStakeShareTot pr

carlLeaderRewardsFromBob :: forall c hc. ExMock c hc => Coin
carlLeaderRewardsFromBob =
  leaderRew
    (bobPoolRewards @c @hc)
    (bobPoolParams' @c @hc)
    (StakeShare $ unCoin bobInitCoin % circulation)
    (StakeShare bobStakeShareTot)

alicePerfEx9 :: Likelihood
alicePerfEx9 = likelihood blocks t (epochSize $ EpochNo 3)
  where
    blocks = 2
    t = leaderProbability f alicePoolStake (_d ppEx)
    f = activeSlotCoeff testGlobals

bobPerfEx9 :: Likelihood
bobPerfEx9 = likelihood blocks t (epochSize $ EpochNo 3)
  where
    blocks = 1
    t = leaderProbability f bobPoolStake (_d ppEx)
    f = activeSlotCoeff testGlobals

nonMyopicEx9 :: forall c hc. ExMock c hc => NonMyopic c
nonMyopicEx9 =
  NonMyopic
    ( Map.fromList
        [ (hk $ Cast.alicePoolKeys @c @hc, alicePerfEx9),
          (hk $ Cast.bobPoolKeys @c @hc, bobPerfEx9)
        ]
    )
    bigR

rewardUpdateEx9 ::
  forall era hc.
  ExMock (Crypto era) hc =>
  ShelleyPParams era ->
  Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))) ->
  RewardUpdate (Crypto era)
rewardUpdateEx9 pp rewards =
  RewardUpdate
    { deltaT = DeltaCoin deltaTEx9,
      deltaR = invert (toDeltaCoin deltaR1Ex9) <> toDeltaCoin deltaR2Ex9,
      rs = rewards,
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx9 @(Crypto era) @hc
    }
  where
    deltaR2Ex9 = bigR <-> (sumRewards pp rewards)

pulserEx9 ::
  forall era hc.
  TwoPoolsConstraints era hc =>
  ShelleyPParams era ->
  PulsingRewUpdate (Crypto era)
pulserEx9 pp =
  makeCompletedPulser
    ( BlocksMade $
        Map.fromList
          [(hk $ Cast.alicePoolKeys @(Crypto era) @hc, 2), (hk $ Cast.bobPoolKeys @(Crypto era) @hc, 1)]
    )
    expectedStEx8'
  where
    expectedStEx8' = C.setPrevPParams pp (expectedStEx8 @era @hc)

expectedStEx9 ::
  forall era hc.
  (TwoPoolsConstraints era hc) =>
  ShelleyPParams era ->
  ChainState era
expectedStEx9 pp =
  C.evolveNonceFrozen (getBlockNonce (blockEx9 @era @hc))
    . C.newLab (blockEx9 @era @hc)
    . C.setOCertCounter coreNodeHK 2
    . C.pulserUpdate (pulserEx9 @era @hc pp)
    $ (expectedStEx8 @era @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era @hc ppEx 390

-- === Block 9, Slot 390, Epoch 3
--
-- Create the first non-trivial reward update. The rewards demonstrate the
-- results of the delegation scenario that was constructed in the first and only transaction.
twoPools9 :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools9 = CHAINExample (expectedStEx8 @era @hc) blockEx9 (Right $ expectedStEx9 @era @hc ppEx)

--
-- Now test with Aggregation
--
carlsRewards :: forall c hc. ExMock c hc => Set (Reward c)
carlsRewards =
  Set.fromList
    [ Reward MemberReward (hk $ Cast.alicePoolKeys @c @hc) (carlMemberRewardsFromAlice @c @hc),
      Reward LeaderReward (hk $ Cast.alicePoolKeys @c @hc) (carlLeaderRewardsFromAlice @c @hc),
      Reward LeaderReward (hk $ Cast.bobPoolKeys @c @hc) (carlLeaderRewardsFromBob @c @hc)
    ]

rsEx9Agg :: forall c hc. ExMock c hc => Map (Credential 'Staking c) (Set (Reward c))
rsEx9Agg = Map.singleton Cast.carlSHK (carlsRewards @c @hc)

ppProtVer3 :: ShelleyPParams era
ppProtVer3 = ppEx {_protocolVersion = ProtVer 3 0}

expectedStEx8Agg :: forall era hc. (TwoPoolsConstraints era hc) => ChainState era
expectedStEx8Agg = C.setPrevPParams ppProtVer3 (expectedStEx8 @era @hc)

expectedStEx9Agg :: forall era hc. (TwoPoolsConstraints era hc) => ChainState era
expectedStEx9Agg = C.setPrevPParams ppProtVer3 (expectedStEx9 @era @hc ppProtVer3)

-- Create the first non-trivial reward update. The rewards demonstrate the
-- results of the delegation scenario that was constructed in the first and only transaction.
twoPools9Agg :: forall era hc. (TwoPoolsConstraints era hc) => CHAINExample (BHeader (Crypto era) hc) era hc
twoPools9Agg = CHAINExample (expectedStEx8Agg @era @hc) blockEx9 (Right (expectedStEx9Agg @era @hc))

testAggregateRewardsLegacy :: HasCallStack => Assertion
testAggregateRewardsLegacy = do
  let expectedReward = carlLeaderRewardsFromBob @(Crypto C) @C_Crypto
  expectedReward @?= rewardAmount (minimum (carlsRewards @(Crypto C) @C_Crypto))
  aggregateRewards @C_Crypto ppEx (rsEx9Agg @(Crypto C) @C_Crypto) @?= Map.singleton Cast.carlSHK expectedReward

testAggregateRewardsNew :: Assertion
testAggregateRewardsNew =
  aggregateRewards @C_Crypto ppProtVer3 (rsEx9Agg @(Crypto C) @C_Crypto)
    @?= Map.singleton Cast.carlSHK (foldMap rewardAmount (carlsRewards @(Crypto C) @C_Crypto))

--
-- Two Pools Test Group
--

twoPoolsExample :: TestTree
twoPoolsExample =
  testGroup
    "two pools"
    [ testCase "create non-aggregated pulser" $ testCHAINExample twoPools9,
      testCase "non-aggregated pulser is correct" $
        ( (Complete (rewardUpdateEx9 @C @C_Crypto ppEx (rsEx9Agg @(Crypto C) @C_Crypto)))
            @?= (fst . runShelleyBase . completeStep $ pulserEx9 @C @C_Crypto ppEx)
        ),
      testCase "aggregated pulser is correct" $
        ( (Complete (rewardUpdateEx9 @C @C_Crypto ppProtVer3 (rsEx9Agg @(Crypto C) @C_Crypto)))
            @?= (fst . runShelleyBase . completeStep $ pulserEx9 @C @C_Crypto ppProtVer3)
        ),
      testCase "create aggregated pulser" $ testCHAINExample twoPools9Agg,
      testCase "create legacy aggregatedRewards" testAggregateRewardsLegacy,
      testCase "create new aggregatedRewards" testAggregateRewardsNew
    ]

-- This test group tests each block individually, which is really only
-- helpful for debugging purposes.
twoPoolsExampleExtended :: TestTree
twoPoolsExampleExtended =
  testGroup
    "two pools extended"
    [ testCase "initial registrations" $ testCHAINExample twoPools1,
      testCase "delegate stake and create reward update" $ testCHAINExample twoPools2,
      testCase "new epoch changes" $ testCHAINExample twoPools3,
      testCase "second reward update" $ testCHAINExample twoPools4,
      testCase "nonempty pool distr" $ testCHAINExample twoPools5,
      testCase "alice produces a block" $ testCHAINExample twoPools6,
      testCase "bob produces a block" $ testCHAINExample twoPools7,
      testCase "prelude to the first nontrivial rewards" $ testCHAINExample twoPools8,
      testCase "create non-aggregated rewards" $ testCHAINExample twoPools9,
      testCase "create aggregated rewards" $ testCHAINExample twoPools9Agg
    ]
