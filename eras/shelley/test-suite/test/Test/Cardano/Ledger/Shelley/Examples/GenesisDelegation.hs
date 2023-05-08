{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.GenesisDelegation
-- Description : Genesis Delegation Example
--
-- Example demonstrating Genesis Delegation
module Test.Cardano.Ledger.Shelley.Examples.GenesisDelegation (
  genesisDelegExample,
)
where

import Cardano.Crypto.DSIGN.Class (Signable)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  Hash,
  KeyRole (..),
  VerKeyVRF,
  asWitness,
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (
  ShelleyTxBody (..),
  ShelleyTxOut (..),
 )
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits, addrWits)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Data.Default.Class (Default)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation (
  coreNodeKeysBySchedule,
  coreNodeSK,
  coreNodeVK,
 )
import Test.Cardano.Ledger.Shelley.Examples.Init (
  initSt,
  lastByronHeaderHash,
  nonce0,
  ppEx,
 )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makePulser')
import Test.Cardano.Ledger.Shelley.Generator.Core (
  NatNonce (..),
  VRFKeyPair (..),
  genesisCoins,
  mkBlockFakeVRF,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  getBlockNonce,
  mkKeyPair,
  mkVRFKeyPair,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

initUTxO :: EraTxOut era => UTxO era
initUTxO =
  genesisCoins
    genesisId
    [ mkBasicTxOut Cast.aliceAddr aliceInitCoin
    , mkBasicTxOut Cast.bobAddr bobInitCoin
    ]
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStGenesisDeleg ::
  ( EraTxOut era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  , EraGovernance era
  , Default (StashedAVVMAddresses era)
  ) =>
  ChainState era
initStGenesisDeleg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

newGenDelegate ::
  Crypto c =>
  KeyPair 'GenesisDelegate c
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 108 0 0 0 1)

newGenesisVrfKH :: forall c. Crypto c => Hash c (VerKeyVRF c)
newGenesisVrfKH = hashVerKeyVRF (vrfVerKey (mkVRFKeyPair @c (RawSeed 9 8 7 6 5)))

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: forall c. Crypto c => ShelleyTxBody (ShelleyEra c)
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertGenesis
            ( ConstitutionalDelegCert
                (hashKey (coreNodeVK 0))
                (hashKey (vKey newGenDelegate))
                (newGenesisVrfKH @c)
            )
        ]
    )
    (Withdrawals Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceCoinEx1 = aliceInitCoin <-> Val.inject feeTx1
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

txEx1 ::
  forall c.
  ( Crypto c
  , Signable (DSIGN c) (Hash.Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyTx (ShelleyEra c)
txEx1 = ShelleyTx txbodyEx1 txwits SNothing
  where
    txwits :: ShelleyTxWits (ShelleyEra c)
    txwits =
      mempty
        { addrWits =
            mkWitnessesVKey @c
              (hashAnnotated (txbodyEx1 @c))
              ( [asWitness Cast.alicePay]
                  <> [ asWitness $
                        KeyPair @'Genesis @c
                          (coreNodeVK 0)
                          (coreNodeSK @c 0)
                     ]
              )
        }

blockEx1 ::
  forall c.
  (ExMock (EraCrypto (ShelleyEra c))) =>
  Block (BHeader c) (ShelleyEra c)
blockEx1 =
  mkBlockFakeVRF @(ShelleyEra c)
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @c)
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert @c (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

newGenDeleg ::
  forall c.
  Crypto c =>
  (FutureGenDeleg c, GenDelegPair c)
newGenDeleg =
  ( FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVK 0)
  , GenDelegPair (hashKey . vKey $ newGenDelegate) (newGenesisVrfKH @c)
  )

expectedStEx1 ::
  forall c.
  (ExMock c) =>
  ChainState (ShelleyEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce @(ShelleyEra c) blockEx1)
    . C.newLab blockEx1
    . C.feesAndDeposits ppEx feeTx1 [] []
    . C.newUTxO txbodyEx1
    . C.setFutureGenDeleg newGenDeleg
    $ initStGenesisDeleg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, stage a new future genesis delegate
genesisDelegation1 ::
  (ExMock c) =>
  CHAINExample (BHeader c) (ShelleyEra c)
genesisDelegation1 = CHAINExample initStGenesisDeleg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall c.
  (ExMock (EraCrypto (ShelleyEra c))) =>
  Block (BHeader c) (ShelleyEra c)
blockEx2 =
  mkBlockFakeVRF @(ShelleyEra c)
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 @c)
    (NatNonce 2)
    minBound
    2
    0
    (mkOCert @c (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 50) 0 (KESPeriod 0))

pulserEx2 :: ExMock c => PulsingRewUpdate c
pulserEx2 = makePulser' expectedStEx1

expectedStEx2 ::
  forall c.
  ExMock c =>
  ChainState (ShelleyEra c)
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce @(ShelleyEra c) blockEx2)
    . C.newLab blockEx2
    . C.adoptFutureGenDeleg newGenDeleg
    . C.pulserUpdate pulserEx2
    $ expectedStEx1

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to trigger adopting the genesis delegation.
genesisDelegation2 :: ExMock c => CHAINExample (BHeader c) (ShelleyEra c)
genesisDelegation2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Genesis Delegation Test Group
--

genesisDelegExample :: TestTree
genesisDelegExample =
  testGroup
    "genesis delegation"
    [ testCase "stage genesis key delegation" $ testCHAINExample genesisDelegation1
    , testCase "adopt genesis key delegation" $ testCHAINExample genesisDelegation2
    ]
