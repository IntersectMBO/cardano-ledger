{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.PoolLifetime
-- Description : Pool Lifetime Example
--
-- Example demonstrating the creation of a new stake pool,
-- block production under Praos, rewards, and pool retirement.
module Test.Shelley.Spec.Ledger.Examples.PoolLifetime
  ( poolLifetimeExample,
  )
where

import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import qualified Cardano.Ledger.Val as Val
import Data.Foldable (fold)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    Nonce,
    StrictMaybe (..),
    (⭒),
  )
import Shelley.Spec.Ledger.BlockChain
  ( Block,
    bhHash,
    bheader,
    hashHeaderToNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..), DeltaCoin (..), toDelta, addDelta)
import Shelley.Spec.Ledger.Credential (Ptr (..))
import Shelley.Spec.Ledger.Delegation.Certificates
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import Shelley.Spec.Ledger.Keys (asWitness, coerceKeyRole)
import Shelley.Spec.Ledger.LedgerState
  ( RewardUpdate (..),
    emptyRewardUpdate,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.Rewards
  ( Likelihood (..),
    NonMyopic (..),
    emptyNonMyopic,
    leaderProbability,
    likelihood,
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
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.Hashing(HashAnnotated(hashAnnotated))
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ExMock)
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
    genesisId,
    mkBlockFakeVRF,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( ShelleyTest,
    epochSize,
    getBlockNonce,
    maxLLSupply,
    testGlobals,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Cardano.Ledger.Crypto as CryptoClass

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: ShelleyTest era => UTxO era
initUTxO =
  genesisCoins
    [ TxOut Cast.aliceAddr (Val.inject aliceInitCoin),
      TxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStPoolLifetime :: forall era. ShelleyTest era => ChainState era
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

txbodyEx1 :: ShelleyTest era => TxBody era
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
                       ( Map.fromList
                           [ (Cast.carlSHK, carlMIR),
                             (Cast.dariaSHK, dariaMIR)
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

txEx1 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Tx era
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @era)
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

blockEx1 :: forall era. (HasCallStack, ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx1 =
  mkBlockFakeVRF
    lastByronHeaderHash
    (coreNodeKeysBySchedule @era ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto era))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @era))
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
poolLifetime1 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
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
txbodyEx2 :: forall era. ShelleyTest era => TxBody era
txbodyEx2 =
  TxBody
    { _inputs = Set.fromList [TxIn (txid txbodyEx1) 0],
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

txEx2 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Tx era
txEx2 =
  Tx
    txbodyEx2
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx2 @era)
            [ asWitness Cast.alicePay,
              asWitness Cast.aliceStake,
              asWitness Cast.bobStake
            ]
      }
    SNothing

blockEx2 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx1)
    (coreNodeKeysBySchedule @era ppEx 90)
    [txEx2]
    (SlotNo 90)
    (BlockNo 2)
    (nonce0 @(Crypto era))
    (NatNonce 2)
    zero
    4
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 90) 0 (KESPeriod 0))

expectedStEx2 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  ChainState era
expectedStEx2 =
  C.evolveNonceFrozen (getBlockNonce (blockEx2 @era))
    . C.newLab blockEx2
    . C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO txbodyEx2
    . C.delegation Cast.aliceSHK (_poolPubKey $ Cast.alicePoolParams @era)
    . C.delegation Cast.bobSHK (_poolPubKey $ Cast.alicePoolParams @era)
    . C.rewardUpdate emptyRewardUpdate
    $ expectedStEx1

-- === Block 2, Slot 90, Epoch 0
--
-- In the second block Alice and Bob both delegation to Alice's Pool.
poolLifetime2 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Nonce
epoch1Nonce = chainCandidateNonce (expectedStEx2 @era)

blockEx3 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx2)
    (coreNodeKeysBySchedule @era ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @era)
    (NatNonce 3)
    zero
    5
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 110) 0 (KESPeriod 0))

snapEx3 :: Era era => EB.SnapShot era
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
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  ChainState era
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
poolLifetime3 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 190, Epoch 1
--

feeTx4 :: Coin
feeTx4 = Coin 5

aliceCoinEx4Base :: Coin
aliceCoinEx4Base = aliceCoinEx2Base <-> feeTx4

txbodyEx4 :: forall era. ShelleyTest era => TxBody era
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

txEx4 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Tx era
txEx4 =
  Tx
    txbodyEx4
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx4 @era)
            [asWitness Cast.alicePay, asWitness Cast.carlStake]
      }
    SNothing

blockEx4 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx3)
    (coreNodeKeysBySchedule @era ppEx 190)
    [txEx4]
    (SlotNo 190)
    (BlockNo 4)
    (epoch1Nonce @era)
    (NatNonce 4)
    zero
    9
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 190) 0 (KESPeriod 0))

rewardUpdateEx4 :: forall era. RewardUpdate era
rewardUpdateEx4 =
  RewardUpdate
    { deltaT = Coin 1,
      deltaR = DeltaCoin 6,
      rs = Map.empty,
      deltaF = DeltaCoin (-7),
      nonMyopic = emptyNonMyopic {rewardPotNM = Coin 6}
    }

expectedStEx4 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  ChainState era
expectedStEx4 =
  C.evolveNonceFrozen (getBlockNonce (blockEx4 @era))
    . C.newLab blockEx4
    . C.feesAndDeposits feeTx4 (Coin 0)
    . C.newUTxO txbodyEx4
    . C.delegation Cast.carlSHK (_poolPubKey $ Cast.alicePoolParams @era)
    . C.rewardUpdate rewardUpdateEx4
    $ expectedStEx3

-- === Block 4, Slot 190, Epoch 1
--
-- We process a block late enough in the epoch in order to create a second reward update,
-- preparing the way for the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change, Carl delegates his stake.
poolLifetime4 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime4 = CHAINExample expectedStEx3 blockEx4 (Right expectedStEx4)

epoch2Nonce :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Nonce
epoch2Nonce =
  chainCandidateNonce (expectedStEx4 @era)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx2 @era))

--
-- Block 5, Slot 220, Epoch 2
--

blockEx5 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx5 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx4)
    (coreNodeKeysBySchedule @era ppEx 220)
    []
    (SlotNo 220)
    (BlockNo 5)
    (epoch2Nonce @era)
    (NatNonce 5)
    zero
    11
    10
    (mkOCert (coreNodeKeysBySchedule @era ppEx 220) 1 (KESPeriod 10))

snapEx5 :: forall era. ShelleyTest era => EB.SnapShot era
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

pdEx5 :: forall c. CryptoClass.Crypto c => PoolDistr c
pdEx5 =
  PoolDistr $
    Map.singleton
      (hk $ Cast.alicePoolKeys @c)
      (IndividualPoolStake 1 (Cast.aliceVRFKeyHash @c))

expectedStEx5 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  ChainState era
expectedStEx5 =
  C.newEpoch blockEx5
    . C.newSnapshot snapEx5 feeTx4
    . C.applyRewardUpdate rewardUpdateEx4
    . C.setPoolDistr pdEx5
    . C.setOCertCounter coreNodeHK 1
    $ expectedStEx4
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era ppEx 220

-- === Block 5, Slot 220, Epoch 2
--
-- Create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.
poolLifetime5 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime5 = CHAINExample expectedStEx4 blockEx5 (Right expectedStEx5)

--
-- Block 6, Slot 295, Epoch 2
--

blockEx6 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx6 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx5)
    Cast.alicePoolKeys
    []
    (SlotNo 295) -- odd slots open for decentralization
    (BlockNo 6)
    (epoch2Nonce @era)
    (NatNonce 6)
    zero
    14
    14
    (mkOCert Cast.alicePoolKeys 0 (KESPeriod 14))

rewardUpdateEx6 :: forall era. RewardUpdate era
rewardUpdateEx6 =
  RewardUpdate
    { deltaT = Coin 1,
      deltaR = DeltaCoin 4,
      rs = Map.empty,
      deltaF = invert $ toDelta feeTx4,
      nonMyopic = emptyNonMyopic {rewardPotNM = Coin 4}
    }

expectedStEx6 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx6 =
  C.evolveNonceFrozen (getBlockNonce (blockEx6 @era))
    . C.newLab blockEx6
    . C.setOCertCounter (coerceKeyRole $ hk Cast.alicePoolKeys) 0
    . C.incrBlockCount (hk Cast.alicePoolKeys)
    . C.rewardUpdate rewardUpdateEx6
    $ expectedStEx5

-- === Block 6, Slot 295, Epoch 2
--
-- Create a decentralized Praos block (ie one not in the overlay schedule)
poolLifetime6 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime6 = CHAINExample expectedStEx5 blockEx6 (Right expectedStEx6)

--
-- Block 7, Slot 310, Epoch 3
--

epoch3Nonce :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Nonce
epoch3Nonce =
  chainCandidateNonce (expectedStEx6 @era)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx4 @era))

blockEx7 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx7 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx6)
    (coreNodeKeysBySchedule @era ppEx 310)
    []
    (SlotNo 310)
    (BlockNo 7)
    (epoch3Nonce @era)
    (NatNonce 7)
    zero
    15
    15
    (mkOCert (coreNodeKeysBySchedule @era ppEx 310) 1 (KESPeriod 15))

expectedStEx7 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx7 =
  C.newEpoch blockEx7
    . C.newSnapshot snapEx5 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx6
    . C.setOCertCounter coreNodeHK 1
    $ expectedStEx6
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era ppEx 310

-- === Block 7, Slot 310, Epoch 3
--
-- Create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update
poolLifetime7 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime7 = CHAINExample expectedStEx6 blockEx7 (Right expectedStEx7)

--
-- Block 8, Slot 390, Epoch 3
--

blockEx8 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx8 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx7)
    (coreNodeKeysBySchedule @era ppEx 390)
    []
    (SlotNo 390)
    (BlockNo 8)
    (epoch3Nonce @era)
    (NatNonce 8)
    zero
    19
    19
    (mkOCert (coreNodeKeysBySchedule @era ppEx 390) 2 (KESPeriod 19))

aliceRAcnt8 :: Coin
aliceRAcnt8 = Coin 11654787878

bobRAcnt8 :: Coin
bobRAcnt8 = Coin 1038545454

deltaT8 :: Coin
deltaT8 = Coin 317333333333

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

nonMyopicEx8 :: forall era. Era era => NonMyopic era
nonMyopicEx8 =
  NonMyopic
    (Map.singleton (hk Cast.alicePoolKeys) alicePerfEx8)
    rewardPot8

rewardUpdateEx8 :: forall era. Era era => RewardUpdate era
rewardUpdateEx8 =
  RewardUpdate
    { deltaT = deltaT8,
      deltaR = deltaR8,
      rs =
        Map.fromList
          [ (Cast.aliceSHK, aliceRAcnt8),
            (Cast.bobSHK, bobRAcnt8)
          ],
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx8
    }

expectedStEx8 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx8 =
  C.evolveNonceFrozen (getBlockNonce (blockEx8 @era))
    . C.newLab blockEx8
    . C.setOCertCounter coreNodeHK 2
    . C.rewardUpdate rewardUpdateEx8
    $ expectedStEx7
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era ppEx 390

-- === Block 8, Slot 390, Epoch 3
--
-- Create the first non-trivial reward update.
poolLifetime8 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime8 = CHAINExample expectedStEx7 blockEx8 (Right expectedStEx8)

--
-- Block 9, Slot 410, Epoch 4
--

epoch4Nonce :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Nonce
epoch4Nonce =
  chainCandidateNonce (expectedStEx8 @era)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx6 @era))

blockEx9 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx9 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx8)
    (coreNodeKeysBySchedule @era ppEx 410)
    []
    (SlotNo 410)
    (BlockNo 9)
    (epoch4Nonce @era)
    (NatNonce 9)
    zero
    20
    20
    (mkOCert (coreNodeKeysBySchedule @era ppEx 410) 2 (KESPeriod 20))

snapEx9 :: forall era. ShelleyTest era => EB.SnapShot era
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

expectedStEx9 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx9 =
  C.newEpoch blockEx9
    . C.newSnapshot snapEx9 (Coin 0)
    . C.applyRewardUpdate rewardUpdateEx8
    . C.setOCertCounter coreNodeHK 2
    $ expectedStEx8
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era ppEx 410

-- === Block 9, Slot 410, Epoch 4
--
-- Apply the first non-trivial reward update.
poolLifetime9 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
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

txbodyEx10 :: ShelleyTest era => TxBody era
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

txEx10 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Tx era
txEx10 =
  Tx
    txbodyEx10
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated $ txbodyEx10 @era) [asWitness Cast.bobPay, asWitness Cast.bobStake]
      }
    SNothing

blockEx10 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx10 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx9)
    (coreNodeKeysBySchedule @era ppEx 420)
    [txEx10]
    (SlotNo 420)
    (BlockNo 10)
    (epoch4Nonce @era)
    (NatNonce 10)
    zero
    21
    19
    (mkOCert (coreNodeKeysBySchedule @era ppEx 420) 2 (KESPeriod 19))

expectedStEx10 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx10 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx10 @era))
    . C.newLab blockEx10
    . C.feesAndDeposits feeTx10 (invert (_keyDeposit ppEx))
    . C.deregStakeCred Cast.bobSHK
    . C.newUTxO txbodyEx10
    $ expectedStEx9

-- === Block 10, Slot 420, Epoch 4
--
-- Drain Bob's reward account and de-register Bob's stake key.
poolLifetime10 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
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

txbodyEx11 :: ShelleyTest era => TxBody era
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

txEx11 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Tx era
txEx11 =
  Tx
    txbodyEx11
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx11 @era)
            ( [asWitness Cast.alicePay]
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx11 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx11 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx10)
    (coreNodeKeysBySchedule @era ppEx 490)
    [txEx11]
    (SlotNo 490)
    (BlockNo 11)
    (epoch4Nonce @era)
    (NatNonce 11)
    zero
    24
    19
    (mkOCert (coreNodeKeysBySchedule @era ppEx 490) 2 (KESPeriod 19))

reserves12 :: Coin
reserves12 = addDelta reserves7 deltaR8

alicePerfEx11 :: forall era. ShelleyTest era => Likelihood
alicePerfEx11 = alicePerfEx8 <> epoch4Likelihood
  where
    epoch4Likelihood = likelihood blocks t (epochSize $ EpochNo 4)
    blocks = 0
    t = leaderProbability f relativeStake (_d ppEx)
    (Coin stake) = fold (EB.unStake . EB._stake $ snapEx5 @era) -- everyone has delegated to Alice's Pool
    relativeStake = fromRational (stake % supply)
    (Coin supply) = maxLLSupply <-> reserves12
    f = activeSlotCoeff testGlobals

nonMyopicEx11 :: forall era. ShelleyTest era => NonMyopic era
nonMyopicEx11 =
  NonMyopic
    (Map.singleton (hk Cast.alicePoolKeys) (alicePerfEx11 @era))
    (Coin 0)

rewardUpdateEx11 :: forall era. ShelleyTest era => RewardUpdate era
rewardUpdateEx11 =
  RewardUpdate
    { deltaT = Coin 0,
      deltaR = DeltaCoin 0,
      rs = Map.empty,
      deltaF = DeltaCoin 0,
      nonMyopic = nonMyopicEx11
    }

expectedStEx11 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx11 =
  C.evolveNonceFrozen (getBlockNonce (blockEx11 @era))
    . C.newLab blockEx11
    . C.feesAndDeposits feeTx11 (Coin 0)
    . C.newUTxO txbodyEx11
    . C.rewardUpdate rewardUpdateEx11
    . C.stageRetirement (hk Cast.alicePoolKeys) aliceRetireEpoch
    $ expectedStEx10

-- === Block 11, Slot 490, Epoch 4
--
-- Stage the retirement of Alice's stake pool.
poolLifetime11 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
poolLifetime11 = CHAINExample expectedStEx10 blockEx11 (Right expectedStEx11)

--
-- Block 12, Slot 510, Epoch 5
--

epoch5Nonce :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Nonce
epoch5Nonce =
  chainCandidateNonce (expectedStEx11 @era)
    ⭒ hashHeaderToNonce (bhHash $ bheader (blockEx8 @era))

blockEx12 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => Block era
blockEx12 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx11)
    (coreNodeKeysBySchedule @era ppEx 510)
    []
    (SlotNo 510)
    (BlockNo 12)
    (epoch5Nonce @era)
    (NatNonce 12)
    zero
    25
    25
    (mkOCert (coreNodeKeysBySchedule @era ppEx 510) 3 (KESPeriod 25))

snapEx12 :: forall era. ShelleyTest era => EB.SnapShot era
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

expectedStEx12 :: forall era. (ShelleyTest era, ExMock (Crypto era)) => ChainState era
expectedStEx12 =
  C.newEpoch blockEx12
    . C.newSnapshot snapEx12 (Coin 11)
    . C.applyRewardUpdate rewardUpdateEx11
    . C.setOCertCounter coreNodeHK 3
    . C.reapPool Cast.alicePoolParams
    $ expectedStEx11
  where
    coreNodeHK = coerceKeyRole . hk $ coreNodeKeysBySchedule @era ppEx 510

-- === Block 12, Slot 510, Epoch 5
--
-- Reap Alice's stake pool.
poolLifetime12 :: (ShelleyTest era, ExMock (Crypto era)) => CHAINExample era
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
      testCase "reap stake pool" $ testCHAINExample poolLifetime12
    ]
