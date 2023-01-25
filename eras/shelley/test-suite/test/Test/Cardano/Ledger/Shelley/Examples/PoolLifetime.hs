{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
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
    mkStake,
  )
where

import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Globals (..),
    Network (..),
    Nonce,
    StrictMaybe (..),
    mkCertIxPartial,
    (⭒),
  )
import Cardano.Ledger.Block (Block, bheader, txid)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, toDeltaCoin)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential, Ptr (..))
import qualified Cardano.Ledger.Crypto as Cr
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Keys (KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.PoolDistr
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
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
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.PoolRank
  ( Likelihood (..),
    NonMyopic (..),
    applyDecay,
    leaderProbability,
    likelihood,
  )
import Cardano.Ledger.Shelley.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx (..),
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
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.HeaderCrypto as Cr
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash, hashHeaderToNonce)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Protocol.TPraos.Rules.Overlay (toPoolStakeVRF)
import Data.Default.Class (def)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Exts (fromList)
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
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

mkStake ::
  [ ( Credential 'Staking crypto,
      Coin
    )
  ] ->
  EB.Stake crypto
mkStake = EB.Stake . GHC.Exts.fromList . map (fmap toCompactCoinError)

initUTxO :: Cr.Crypto c => UTxO (ShelleyEra c)
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin),
      ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStPoolLifetime :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ChainState (ShelleyEra c)
initStPoolLifetime = (initSt @(ShelleyEra c) @hc) initUTxO

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

txbodyEx1 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx1)])
    ( StrictSeq.fromList
        ( [ DCertDeleg (RegKey Cast.aliceSHK),
            DCertDeleg (RegKey Cast.bobSHK),
            DCertDeleg (RegKey Cast.carlSHK),
            DCertPool (RegPool (Cast.alicePoolParams @c @hc))
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

txEx1 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx1 =
  ShelleyTx
    (txbodyEx1 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx1 @c @hc))
            ( (asWitness <$> [Cast.alicePay, Cast.carlPay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold (Cast.alicePoolKeys @c @hc)]
                <> ( asWitness
                       <$> [ cold (coreNodeIssuerKeys @c @hc 0),
                             cold (coreNodeIssuerKeys @c @hc 1),
                             cold (coreNodeIssuerKeys @c @hc 2),
                             cold (coreNodeIssuerKeys @c @hc 3),
                             cold (coreNodeIssuerKeys @c @hc 4)
                           ]
                   )
            )
      }
    SNothing

blockEx1 :: forall c hc. (HasCallStack, ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
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

expectedStEx1 :: forall c hc. (ExMock c hc) => ChainState (ShelleyEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce blockEx1')
    . C.newLab blockEx1'
    . C.feesAndDeposits feeTx1 (((3 :: Integer) <×> _keyDeposit ppEx) <+> _poolDeposit ppEx)
    . C.newUTxO (txbodyEx1 @c @hc)
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 0))
    . C.newStakeCred Cast.bobSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 1))
    . C.newStakeCred Cast.carlSHK (Ptr (SlotNo 10) minBound (mkCertIxPartial 2))
    . C.newPool (Cast.alicePoolParams @c @hc)
    . C.mir Cast.carlSHK ReservesMIR carlMIR
    . C.mir Cast.dariaSHK ReservesMIR dariaMIR
    $ initStPoolLifetime @c @hc
  where
    blockEx1' = blockEx1 @c @hc

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block of this example, Alice, Bob, and Carl
-- all register stake credentials, and Alice registers a stake pool.
-- Additionally, a MIR certificate is issued to draw from the reserves
-- and give Carl and Daria (who is unregistered) rewards.
poolLifetime1 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime1 = CHAINExample (initStPoolLifetime @c @hc) blockEx1 (Right $ expectedStEx1 @(Crypto (ShelleyEra c)) @hc)

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
txbodyEx2 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx2 =
  ShelleyTxBody
    { _inputs = Set.fromList [TxIn (txid (txbodyEx1 @c @hc)) minBound],
      _outputs =
        StrictSeq.fromList
          [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx2Base),
            ShelleyTxOut Cast.alicePtrAddr (Val.inject aliceCoinEx2Ptr)
          ],
      _certs =
        StrictSeq.fromList
          [ DCertDeleg (Delegate $ Delegation Cast.aliceSHK (hk (Cast.alicePoolKeys @c @hc))),
            DCertDeleg (Delegate $ Delegation Cast.bobSHK (hk (Cast.alicePoolKeys @c @hc)))
          ],
      _wdrls = Wdrl Map.empty,
      _txfee = feeTx2,
      _ttl = SlotNo 90,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txEx2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx2 =
  ShelleyTx
    (txbodyEx2 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx2 @c @hc))
            [ asWitness Cast.alicePay,
              asWitness Cast.aliceStake,
              asWitness Cast.bobStake
            ]
      }
    SNothing

blockEx2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx1)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 90)
    [txEx2 @c @hc]
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
    p =
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
makeCompletedPulser bs cs = Complete . fst . runShelleyBase . completeRupd $ makePulser bs cs

pulserEx2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => PulsingRewUpdate c
pulserEx2 = makeCompletedPulser (BlocksMade mempty) $ expectedStEx1 @c @hc

expectedStEx2 ::
  forall c hc.
  (ExMock (Crypto (ShelleyEra c)) hc) =>
  ChainState (ShelleyEra c)
expectedStEx2 =
  C.evolveNonceFrozen (getBlockNonce blockEx2')
    . C.newLab blockEx2'
    . C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO (txbodyEx2 @c @hc)
    . C.delegation Cast.aliceSHK (_poolId $ Cast.alicePoolParams @c @hc)
    . C.delegation Cast.bobSHK (_poolId $ Cast.alicePoolParams @c @hc)
    . C.pulserUpdate (pulserEx2 @c @hc)
    $ (expectedStEx1 @c @hc)
  where
    blockEx2' = blockEx2 @c @hc

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice and Bob both delegation to Alice's Pool.
poolLifetime2 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime2 = CHAINExample (expectedStEx1 @c @hc) (blockEx2 @c @hc) (Right (expectedStEx2 @c @hc))

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2 @c @hc)

blockEx3 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx2)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c @hc)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 110) 0 (KESPeriod 0))

snapEx3 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => EB.SnapShot c
snapEx3 =
  EB.SnapShot
    { EB._stake =
        mkStake
          [ (Cast.aliceSHK, aliceCoinEx2Base <> aliceCoinEx2Ptr),
            (Cast.bobSHK, bobInitCoin)
          ],
      EB._delegations =
        [ (Cast.aliceSHK, hk alicePoolKeys'),
          (Cast.bobSHK, hk alicePoolKeys')
        ],
      EB._poolParams = [(hk alicePoolKeys', alicePoolParams')]
    }
  where
    alicePoolKeys' = Cast.alicePoolKeys @c @hc
    alicePoolParams' = Cast.alicePoolParams @c @hc

expectedStEx3 ::
  forall c hc.
  (ExMock (Crypto (ShelleyEra c)) hc) =>
  ChainState (ShelleyEra c)
expectedStEx3 =
  C.newEpoch (blockEx3 @c @hc)
    . C.newSnapshot (snapEx3 @c @hc) (feeTx1 <> feeTx2)
    . C.applyMIR ReservesMIR (Map.singleton Cast.carlSHK carlMIR)
    . C.applyRewardUpdate emptyRewardUpdate
    $ (expectedStEx2 @c @hc)

-- === Block 3, Slot 110, Epoch 1
--
-- In the third block, an empty block in a new epoch, the first snapshot is created.
-- The rewards accounts from the MIR certificate in block 1 are now increased.
poolLifetime3 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime3 = CHAINExample (expectedStEx2 @c @hc) (blockEx3 @c @hc) (Right $ expectedStEx3 @c @hc)

--
-- Block 4, Slot 190, Epoch 1
--

feeTx4 :: Coin
feeTx4 = Coin 5

aliceCoinEx4Base :: Coin
aliceCoinEx4Base = aliceCoinEx2Base <-> feeTx4

txbodyEx4 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx4 =
  ShelleyTxBody
    { _inputs = Set.fromList [TxIn (txid txbodyEx2') minBound],
      _outputs = StrictSeq.fromList [ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinEx4Base)],
      _certs =
        StrictSeq.fromList
          [DCertDeleg (Delegate $ Delegation Cast.carlSHK (hk alicePoolKeys'))],
      _wdrls = Wdrl Map.empty,
      _txfee = feeTx4,
      _ttl = SlotNo 500,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }
  where
    txbodyEx2' = txbodyEx2 @c @hc
    alicePoolKeys' = Cast.alicePoolKeys @c @hc

txEx4 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx4 =
  ShelleyTx
    (txbodyEx4 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx4 @c @hc))
            [asWitness Cast.alicePay, asWitness Cast.carlStake]
      }
    SNothing

blockEx4 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx3)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 190)
    [txEx4 @c @hc]
    (SlotNo 190)
    (BlockNo 4)
    (epoch1Nonce @c @hc)
    (NatNonce 4)
    minBound
    9
    0
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 190) 0 (KESPeriod 0))

pulserEx4 :: forall c hc. (ExMock c hc) => PulsingRewUpdate c
pulserEx4 = makeCompletedPulser (BlocksMade mempty) (expectedStEx3 @c @hc)

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
  forall c hc.
  (ExMock (Crypto (ShelleyEra c)) hc) =>
  ChainState (ShelleyEra c)
expectedStEx4 =
  C.evolveNonceFrozen (getBlockNonce (blockEx4 @c @hc))
    . C.newLab (blockEx4 @c @hc)
    . C.feesAndDeposits feeTx4 (Coin 0)
    . C.newUTxO (txbodyEx4 @c @hc)
    . C.delegation Cast.carlSHK (_poolId $ Cast.alicePoolParams @c @hc)
    . C.pulserUpdate (pulserEx4 @c @hc)
    $ (expectedStEx3 @c @hc)

-- === Block 4, Slot 190, Epoch 1
--
-- We process a block late enough in the epoch in order to create a second reward update,
-- preparing the way for the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change, Carl delegates his stake.
poolLifetime4 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime4 = CHAINExample (expectedStEx3 @c @hc) (blockEx4 @c @hc) (Right $ expectedStEx4 @c @hc)

epoch2Nonce :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Nonce
epoch2Nonce =
  chainCandidateNonce (expectedStEx4 @c @hc)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx2 @c @hc))

--
-- Block 5, Slot 220, Epoch 2
--

blockEx5 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx5 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx4)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 220)
    []
    (SlotNo 220)
    (BlockNo 5)
    (epoch2Nonce @c @hc)
    (NatNonce 5)
    minBound
    11
    10
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 220) 1 (KESPeriod 10))

snapEx5 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => EB.SnapShot c
snapEx5 =
  EB.SnapShot
    { EB._stake =
        mkStake
          [ (Cast.aliceSHK, aliceCoinEx4Base <> aliceCoinEx2Ptr),
            (Cast.carlSHK, carlMIR),
            (Cast.bobSHK, bobInitCoin)
          ],
      EB._delegations =
        [ (Cast.aliceSHK, hk alicePoolKeys'),
          (Cast.carlSHK, hk alicePoolKeys'),
          (Cast.bobSHK, hk alicePoolKeys')
        ],
      EB._poolParams = [(hk alicePoolKeys', alicePoolParams')]
    }
  where
    alicePoolKeys' = Cast.alicePoolKeys @c @hc
    alicePoolParams' = Cast.alicePoolParams @c @hc

pdEx5 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => PoolDistr c
pdEx5 =
  PoolDistr $
    Map.singleton
      (hk $ Cast.alicePoolKeys @c @hc)
      (IndividualPoolStake 1 (toPoolStakeVRF $ Cast.aliceVRFKeyHash @c @hc))

expectedStEx5 ::
  forall c hc.
  (ExMock (Crypto (ShelleyEra c)) hc) =>
  ChainState (ShelleyEra c)
expectedStEx5 =
  C.newEpoch (blockEx5 @c @hc)
    . C.newSnapshot (snapEx5 @c @hc) feeTx4
    . C.applyRewardUpdate rewardUpdateEx4
    . C.setPoolDistr (pdEx5 @c @hc)
    . C.setOCertCounter coreNodeHK 1
    $ (expectedStEx4 @c @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) @hc ppEx 220

-- === Block 5, Slot 220, Epoch 2
--
-- Create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.
poolLifetime5 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime5 = CHAINExample (expectedStEx4 @c @hc) (blockEx5 @c @hc) (Right $ expectedStEx5 @c @hc)

--
-- Block 6, Slot 295, Epoch 2
--

blockEx6 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx6 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx5)
    Cast.alicePoolKeys
    []
    (SlotNo 295) -- odd slots open for decentralization
    (BlockNo 6)
    (epoch2Nonce @c @hc)
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

pulserEx6 :: forall c hc. (ExMock c hc) => PulsingRewUpdate c
pulserEx6 = makeCompletedPulser (BlocksMade mempty) (expectedStEx5 @c @hc)

expectedStEx6 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx6 =
  C.evolveNonceFrozen (getBlockNonce (blockEx6 @c @hc))
    . C.newLab (blockEx6 @c @hc)
    . C.setOCertCounter (coerceKeyRole $ hk alicePoolKeys') 0
    . C.incrBlockCount (hk alicePoolKeys')
    . C.pulserUpdate (pulserEx6 @c @hc)
    $ (expectedStEx5 @c @hc)
  where
    alicePoolKeys' = Cast.alicePoolKeys @c @hc

-- === Block 6, Slot 295, Epoch 2
--
-- Create a decentralized Praos block (ie one not in the overlay schedule)
poolLifetime6 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime6 = CHAINExample (expectedStEx5 @c @hc) (blockEx6 @c @hc) (Right $ expectedStEx6 @c @hc)

--
-- Block 7, Slot 310, Epoch 3
--

epoch3Nonce :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Nonce
epoch3Nonce =
  chainCandidateNonce (expectedStEx6 @c @hc)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx4 @c @hc))

blockEx7 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx7 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx6)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 310)
    []
    (SlotNo 310)
    (BlockNo 7)
    (epoch3Nonce @c @hc)
    (NatNonce 7)
    minBound
    15
    15
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 310) 1 (KESPeriod 15))

expectedStEx7 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx7 =
  C.newEpoch (blockEx7 @c @hc)
    . C.newSnapshot (snapEx5 @c @hc) (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx6
    . C.setOCertCounter coreNodeHK 1
    $ (expectedStEx6 @c @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) @hc ppEx 310

-- === Block 7, Slot 310, Epoch 3
--
-- Create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update
poolLifetime7 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime7 = CHAINExample (expectedStEx6 @c @hc) blockEx7 (Right $ expectedStEx7 @c @hc)

--
-- Block 8, Slot 390, Epoch 3
--

blockEx8 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx8 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx7)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 390)
    []
    (SlotNo 390)
    (BlockNo 8)
    (epoch3Nonce @c @hc)
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

nonMyopicEx8 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => NonMyopic c
nonMyopicEx8 =
  NonMyopic
    (Map.singleton (hk (Cast.alicePoolKeys @c @hc)) alicePerfEx8)
    rewardPot8

pulserEx8 :: forall c hc. (ExMock c hc) => PulsingRewUpdate c
pulserEx8 = makeCompletedPulser (BlocksMade $ Map.singleton (hk $ Cast.alicePoolKeys @c @hc) 1) (expectedStEx7 @c @hc)

rewardUpdateEx8 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => RewardUpdate c
rewardUpdateEx8 =
  RewardUpdate
    { deltaT = deltaT8,
      deltaR = deltaR8,
      rs =
        Map.fromList
          [ ( Cast.aliceSHK,
              Set.singleton $ Reward LeaderReward (hk alicePoolKeys') aliceRAcnt8
            ),
            ( Cast.bobSHK,
              Set.singleton $ Reward MemberReward (hk alicePoolKeys') bobRAcnt8
            )
          ],
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx8 @c @hc
    }
  where
    alicePoolKeys' = Cast.alicePoolKeys @c @hc

expectedStEx8 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx8 =
  C.evolveNonceFrozen (getBlockNonce (blockEx8 @c @hc))
    . C.newLab (blockEx8 @c @hc)
    . C.setOCertCounter coreNodeHK 2
    . C.pulserUpdate (pulserEx8 @c @hc)
    $ (expectedStEx7 @c @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) @hc ppEx 390

-- === Block 8, Slot 390, Epoch 3
--
-- Create the first non-trivial reward update.
poolLifetime8 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime8 = CHAINExample (expectedStEx7 @c @hc) blockEx8 (Right $ expectedStEx8 @c @hc)

--
-- Block 9, Slot 410, Epoch 4
--

epoch4Nonce :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Nonce
epoch4Nonce =
  chainCandidateNonce (expectedStEx8 @c @hc)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx6 @c @hc))

blockEx9 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx9 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx8)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 410)
    []
    (SlotNo 410)
    (BlockNo 9)
    (epoch4Nonce @c @hc)
    (NatNonce 9)
    minBound
    20
    20
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 410) 2 (KESPeriod 20))

snapEx9 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => EB.SnapShot c
snapEx9 =
  (snapEx5 @c @hc)
    { EB._stake =
        mkStake
          [ (Cast.bobSHK, bobInitCoin <> bobRAcnt8),
            (Cast.aliceSHK, aliceCoinEx4Base <> aliceCoinEx2Ptr <> aliceRAcnt8),
            (Cast.carlSHK, carlMIR)
          ]
    }

expectedStEx9 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx9 =
  C.newEpoch (blockEx9 @c @hc)
    . C.newSnapshot (snapEx9 @c @hc) (Coin 0)
    . C.applyRewardUpdate (rewardUpdateEx8 @c @hc)
    . C.setOCertCounter coreNodeHK 2
    $ (expectedStEx8 @c @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) @hc ppEx 410

-- === Block 9, Slot 410, Epoch 4
--
-- Apply the first non-trivial reward update.
poolLifetime9 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime9 = CHAINExample (expectedStEx8 @c @hc) blockEx9 (Right $ expectedStEx9 @c @hc)

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

txbodyEx10 :: Cr.Crypto c => ShelleyTxBody (ShelleyEra c)
txbodyEx10 =
  ShelleyTxBody
    (Set.fromList [mkTxInPartial genesisId 1])
    (StrictSeq.singleton $ ShelleyTxOut Cast.bobAddr (Val.inject bobAda10))
    (StrictSeq.fromList [DCertDeleg (DeRegKey Cast.bobSHK)])
    (Wdrl $ Map.singleton (RewardAcnt Testnet Cast.bobSHK) bobRAcnt8)
    feeTx10
    (SlotNo 500)
    SNothing
    SNothing

txEx10 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ShelleyTx (ShelleyEra c)
txEx10 =
  ShelleyTx
    txbodyEx10
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated (txbodyEx10 @c)) [asWitness Cast.bobPay, asWitness Cast.bobStake]
      }
    SNothing

blockEx10 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx10 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx9)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 420)
    [txEx10 @c @hc]
    (SlotNo 420)
    (BlockNo 10)
    (epoch4Nonce @c @hc)
    (NatNonce 10)
    minBound
    21
    19
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 420) 2 (KESPeriod 19))

expectedStEx10 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx10 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx10 @c @hc))
    . C.newLab (blockEx10 @c @hc)
    . C.feesAndDeposits feeTx10 (invert (_keyDeposit ppEx))
    . C.deregStakeCred Cast.bobSHK
    . C.newUTxO txbodyEx10
    $ (expectedStEx9 @c @hc)

-- === Block 10, Slot 420, Epoch 4
--
-- Drain Bob's reward account and de-register Bob's stake key.
poolLifetime10 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime10 = CHAINExample (expectedStEx9 @c @hc) blockEx10 (Right $ expectedStEx10 @c @hc)

--
-- Block 11, Slot 490, Epoch 4
--

feeTx11 :: Coin
feeTx11 = Coin 2

aliceCoinEx11Ptr :: Coin
aliceCoinEx11Ptr = aliceCoinEx4Base <-> feeTx11

aliceRetireEpoch :: EpochNo
aliceRetireEpoch = EpochNo 5

txbodyEx11 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => ShelleyTxBody (ShelleyEra c)
txbodyEx11 =
  ShelleyTxBody
    (Set.fromList [TxIn (txid $ txbodyEx4 @c @hc) minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.alicePtrAddr (Val.inject aliceCoinEx11Ptr))
    (StrictSeq.fromList [DCertPool (RetirePool (hk $ Cast.alicePoolKeys @c @hc) aliceRetireEpoch)])
    (Wdrl Map.empty)
    feeTx11
    (SlotNo 500)
    SNothing
    SNothing

txEx11 :: forall c hc. ExMock (Crypto (ShelleyEra c)) hc => ShelleyTx (ShelleyEra c)
txEx11 =
  ShelleyTx
    (txbodyEx11 @c @hc)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated (txbodyEx11 @c @hc))
            ( [asWitness Cast.alicePay]
                <> [asWitness $ cold (Cast.alicePoolKeys @c @hc)]
            )
      }
    SNothing

blockEx11 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx11 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx10)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 490)
    [txEx11 @c @hc]
    (SlotNo 490)
    (BlockNo 11)
    (epoch4Nonce @c @hc)
    (NatNonce 11)
    minBound
    24
    19
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 490) 2 (KESPeriod 19))

reserves12 :: Coin
reserves12 = addDeltaCoin reserves7 deltaR8

alicePerfEx11 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => Likelihood
alicePerfEx11 = applyDecay decayFactor alicePerfEx8 <> epoch4Likelihood
  where
    epoch4Likelihood = likelihood blocks t (epochSize $ EpochNo 4)
    blocks = 0
    t = leaderProbability f relativeStake (_d ppEx)
    -- everyone has delegated to Alice's Pool
    Coin stake = EB.sumAllStake (EB._stake $ snapEx5 @c @hc)
    relativeStake = fromRational (stake % supply)
    Coin supply = maxLLSupply <-> reserves12
    f = activeSlotCoeff testGlobals

nonMyopicEx11 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => NonMyopic c
nonMyopicEx11 =
  NonMyopic
    (Map.singleton (hk $ Cast.alicePoolKeys @c @hc) (alicePerfEx11 @c @hc))
    (Coin 0)

pulserEx11 :: forall c hc. (ExMock c hc) => PulsingRewUpdate c
pulserEx11 = makeCompletedPulser (BlocksMade mempty) (expectedStEx10 @c @hc)

rewardUpdateEx11 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => RewardUpdate c
rewardUpdateEx11 =
  RewardUpdate
    { deltaT = DeltaCoin 0,
      deltaR = DeltaCoin 0,
      rs = Map.empty,
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx11 @c @hc
    }

expectedStEx11 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx11 =
  C.evolveNonceFrozen (getBlockNonce (blockEx11 @c @hc))
    . C.newLab (blockEx11 @c @hc)
    . C.feesAndDeposits feeTx11 (Coin 0)
    . C.newUTxO (txbodyEx11 @c @hc)
    . C.pulserUpdate (pulserEx11 @c @hc)
    . C.stageRetirement (hk $ Cast.alicePoolKeys @c @hc) aliceRetireEpoch
    $ (expectedStEx10 @c @hc)

-- === Block 11, Slot 490, Epoch 4
--
-- Stage the retirement of Alice's stake pool.
poolLifetime11 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime11 = CHAINExample (expectedStEx10 @c @hc) blockEx11 (Right $ expectedStEx11 @c @hc)

--
-- Block 12, Slot 510, Epoch 5
--

epoch5Nonce :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Nonce
epoch5Nonce =
  chainCandidateNonce (expectedStEx11 @c @hc)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx8 @c @hc))

blockEx12 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => Block (BHeader c hc) (ShelleyEra c)
blockEx12 =
  mkBlockFakeVRF
    (bhHash $ bheader @(BHeader c hc) @(ShelleyEra c) blockEx11)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 510)
    []
    (SlotNo 510)
    (BlockNo 12)
    (epoch5Nonce @c @hc)
    (NatNonce 12)
    minBound
    25
    25
    (mkOCert (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 510) 3 (KESPeriod 25))

snapEx12 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => EB.SnapShot c
snapEx12 =
  (snapEx9 @c @hc)
    { EB._stake =
        mkStake
          [ (Cast.aliceSHK, aliceRAcnt8 <> aliceCoinEx2Ptr <> aliceCoinEx11Ptr),
            (Cast.carlSHK, carlMIR)
          ],
      EB._delegations =
        [ (Cast.aliceSHK, hk (Cast.alicePoolKeys @c @hc)),
          (Cast.carlSHK, hk (Cast.alicePoolKeys @c @hc))
        ]
    }

expectedStEx12 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => ChainState (ShelleyEra c)
expectedStEx12 =
  C.newEpoch (blockEx12 @c @hc)
    . C.newSnapshot (snapEx12 @c @hc) (Coin 11)
    . C.applyRewardUpdate (rewardUpdateEx11 @c @hc)
    . C.setOCertCounter coreNodeHK 3
    . C.reapPool (Cast.alicePoolParams @c @hc)
    $ (expectedStEx11 @c @hc)
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @(ShelleyEra c) @hc ppEx 510

-- === Block 12, Slot 510, Epoch 5
--
-- Reap Alice's stake pool.
poolLifetime12 :: forall c hc. (ExMock (Crypto (ShelleyEra c)) hc) => CHAINExample (BHeader c hc) (ShelleyEra c) hc
poolLifetime12 = CHAINExample (expectedStEx11 @c @hc) blockEx12 (Right $ expectedStEx12 @c @hc)

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
      testCase "reap stake pool" $ testCHAINExample poolLifetime12
    ]
