{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.GenesisDelegation
-- Description : Genesis Delegation Example
--
-- Example demonstrating Genesis Delegation
module Test.Shelley.Spec.Ledger.Examples.GenesisDelegation
  ( genesisDelegExample,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.BlockChain (Block, bhHash, bheader)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.LedgerState (FutureGenDeleg (..), emptyRewardUpdate)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (Tx (..), WitnessSetHKD (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    GenesisDelegCert (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.Hashing(HashAnnotated(hashAnnotated))
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ExMock)
import Test.Shelley.Spec.Ledger.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import qualified Test.Shelley.Spec.Ledger.Examples.Combinators as C
import Test.Shelley.Spec.Ledger.Examples.Federation
  ( coreNodeKeysBySchedule,
    coreNodeSK,
    coreNodeVK,
  )
import Test.Shelley.Spec.Ledger.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( NatNonce (..),
    genesisCoins,
    genesisId,
    mkBlockFakeVRF,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( ShelleyTest,
    getBlockNonce,
    mkKeyPair,
    mkVRFKeyPair,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

initUTxO :: ShelleyTest era => UTxO era
initUTxO =
  genesisCoins
    [ TxOut Cast.aliceAddr aliceInitCoin,
      TxOut Cast.bobAddr bobInitCoin
    ]
    where
      aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
      bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStGenesisDeleg :: forall era. ShelleyTest era => ChainState era
initStGenesisDeleg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

newGenDelegate ::
  CryptoClass.Crypto crypto =>
  KeyPair 'GenesisDelegate crypto
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (108, 0, 0, 0, 1)

newGenesisVrfKH ::
  forall h v.
  (HashAlgorithm h, VRF.VRFAlgorithm v) =>
  Hash.Hash h (VRF.VerKeyVRF v)
newGenesisVrfKH = hashVerKeyVRF . snd $ mkVRFKeyPair (9, 8, 7, 6, 5)

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: ShelleyTest era => TxBody era
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertGenesis
            ( GenesisDelegCert
                (hashKey (coreNodeVK 0))
                (hashKey (vKey newGenDelegate))
                newGenesisVrfKH
            )
        ]
    )
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
    where
      aliceCoinEx1 = aliceInitCoin <-> (Val.inject feeTx1)
      aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

txEx1 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  Tx era
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ (txbodyEx1 @era))
            ( [asWitness Cast.alicePay]
                <> [ asWitness $
                       KeyPair
                         (coreNodeVK 0)
                         (coreNodeSK @(Crypto era) 0)
                   ]
            )
      }
    SNothing

blockEx1 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  Block era
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

newGenDeleg ::
  forall crypto.
  CryptoClass.Crypto crypto =>
  (FutureGenDeleg crypto, GenDelegPair crypto)
newGenDeleg =
  ( FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVK 0),
    GenDelegPair (hashKey . vKey $ newGenDelegate) newGenesisVrfKH
  )

expectedStEx1 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  ChainState era
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @era))
    . C.newLab blockEx1
    . C.feesAndDeposits feeTx1 (Coin 0)
    . C.newUTxO txbodyEx1
    . C.setFutureGenDeleg newGenDeleg
    $ initStGenesisDeleg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, stage a new future genesis delegate
genesisDelegation1 ::
  (ShelleyTest era, ExMock (Crypto era)) =>
  CHAINExample era
genesisDelegation1 = CHAINExample initStGenesisDeleg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  Block era
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @era blockEx1)
    (coreNodeKeysBySchedule @era ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 @(Crypto era))
    (NatNonce 2)
    zero
    2
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 50) 0 (KESPeriod 0))

expectedStEx2 ::
  forall era.
  (ShelleyTest era, ExMock (Crypto era)) =>
  ChainState era
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @era))
    . C.newLab blockEx2
    . C.adoptFutureGenDeleg newGenDeleg
    . C.rewardUpdate emptyRewardUpdate
    $ expectedStEx1

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to trigger adopting the genesis delegation.
genesisDelegation2 ::
  (ShelleyTest era, ExMock (Crypto era)) =>
  CHAINExample era
genesisDelegation2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Genesis Delegation Test Group
--

genesisDelegExample :: TestTree
genesisDelegExample =
  testGroup
    "genesis delegation"
    [ testCase "stage genesis key delegation" $ testCHAINExample genesisDelegation1,
      testCase "adopt genesis key delegation" $ testCHAINExample genesisDelegation2
    ]
