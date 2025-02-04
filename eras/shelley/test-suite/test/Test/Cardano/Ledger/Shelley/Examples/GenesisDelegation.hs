{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
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

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (GenDelegPair (..), asWitness)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits, addrWits)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Protocol.Crypto (hashVerKeyVRF)
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Data.Default (Default)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
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
  , EraGov era
  , Default (StashedAVVMAddresses era)
  ) =>
  ChainState era
initStGenesisDeleg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

newGenDelegate :: KeyPair 'GenesisDelegate
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 108 0 0 0 1)

newGenesisVrfKH :: VRFVerKeyHash 'GenDelegVRF
newGenesisVrfKH = hashVerKeyVRF @MockCrypto (vrfVerKey (mkVRFKeyPair @MockCrypto (RawSeed 9 8 7 6 5)))

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: ShelleyTxBody ShelleyEra
txbodyEx1 =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ GenesisDelegTxCert
            (hashKey (coreNodeVK 0))
            (hashKey (vKey newGenDelegate))
            newGenesisVrfKH
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

txEx1 :: ShelleyTx ShelleyEra
txEx1 = ShelleyTx txbodyEx1 txwits SNothing
  where
    txwits :: ShelleyTxWits ShelleyEra
    txwits =
      mempty
        { addrWits =
            mkWitnessesVKey
              (hashAnnotated txbodyEx1)
              ( [asWitness Cast.alicePay]
                  <> [ asWitness $
                        KeyPair @'Genesis
                          (coreNodeVK 0)
                          (coreNodeSK 0)
                     ]
              )
        }

blockEx1 :: Block (BHeader MockCrypto) ShelleyEra
blockEx1 =
  mkBlockFakeVRF @ShelleyEra
    lastByronHeaderHash
    (coreNodeKeysBySchedule @ShelleyEra ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    nonce0
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert @MockCrypto (coreNodeKeysBySchedule @ShelleyEra ppEx 10) 0 (KESPeriod 0))

newGenDeleg :: (FutureGenDeleg, GenDelegPair)
newGenDeleg =
  ( FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVK 0)
  , GenDelegPair (hashKey $ vKey newGenDelegate) newGenesisVrfKH
  )

expectedStEx1 :: ChainState ShelleyEra
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce @ShelleyEra blockEx1)
    . C.newLab blockEx1
    . C.feesAndDeposits ppEx feeTx1 [] []
    . C.newUTxO txbodyEx1
    . C.setFutureGenDeleg newGenDeleg
    $ initStGenesisDeleg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, stage a new future genesis delegate
genesisDelegation1 :: CHAINExample ShelleyEra
genesisDelegation1 = CHAINExample initStGenesisDeleg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 :: Block (BHeader MockCrypto) ShelleyEra
blockEx2 =
  mkBlockFakeVRF @ShelleyEra
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule @ShelleyEra ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    nonce0
    (NatNonce 2)
    minBound
    2
    0
    (mkOCert @MockCrypto (coreNodeKeysBySchedule @ShelleyEra ppEx 50) 0 (KESPeriod 0))

pulserEx2 :: PulsingRewUpdate
pulserEx2 = makePulser' expectedStEx1

expectedStEx2 :: ChainState ShelleyEra
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce @ShelleyEra blockEx2)
    . C.newLab blockEx2
    . C.adoptFutureGenDeleg newGenDeleg
    . C.pulserUpdate pulserEx2
    . C.solidifyProposals
    $ expectedStEx1

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to trigger adopting the genesis delegation.
genesisDelegation2 :: CHAINExample ShelleyEra
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
