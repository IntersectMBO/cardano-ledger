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
module Test.Cardano.Ledger.Shelley.Examples.GenesisDelegation
  ( genesisDelegExample,
  )
where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as Cr
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenesisVRF,
    KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
    hashVerKeyVRF,
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (FutureGenDeleg (..), PulsingRewUpdate)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams, ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), ShelleyWitnesses, WitnessSetHKD (..))
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    GenesisDelegCert (..),
    ShelleyTxBody (..),
    ShelleyTxOut (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessesVKey)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import qualified Cardano.Protocol.HeaderCrypto as Cr
import Cardano.Protocol.HeaderKeys
import Cardano.Protocol.TPraos.BHeader (BHeader, bhHash)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Protocol.TPraos.Rules.Overlay (toGenesisVRF)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto, ExMock)
import Test.Cardano.Ledger.Shelley.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import qualified Test.Cardano.Ledger.Shelley.Examples.Combinators as C
import Test.Cardano.Ledger.Shelley.Examples.Federation
  ( coreNodeKeysBySchedule,
    coreNodeSK,
    coreNodeVK,
  )
import Test.Cardano.Ledger.Shelley.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (makePulser')
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( NatNonce (..),
    genesisCoins,
    mkBlockFakeVRF,
    mkOCert,
  )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils
  ( RawSeed (..),
    ShelleyTest,
    getBlockNonce,
    mkKeyPair,
    mkVRFKeyPair,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

initUTxO :: (ShelleyTest era) => UTxO era
initUTxO =
  genesisCoins
    genesisId
    [ ShelleyTxOut Cast.aliceAddr aliceInitCoin,
      ShelleyTxOut Cast.bobAddr bobInitCoin
    ]
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStGenesisDeleg ::
  forall era hc.
  ( Cr.HeaderCrypto hc,
    ShelleyTest era,
    PParams era ~ ShelleyPParams era
  ) =>
  Proxy hc ->
  ChainState era
initStGenesisDeleg _ = initSt @era @hc initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

newGenDelegate ::
  CryptoClass.Crypto crypto =>
  KeyPair 'GenesisDelegate crypto
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 108 0 0 0 1)

newGenesisVrfKH ::
  (HashAlgorithm c) =>
  Proxy hc ->
  Hash.Hash c GenesisVRF
newGenesisVrfKH _phc = toGenesisVRF . hashVerKeyVRF . snd $ keyPair
  where
    keyPair :: (SignKeyVRF Cr.StandardCrypto, VerKeyVRF Cr.StandardCrypto)
    keyPair = mkVRFKeyPair (RawSeed 9 8 7 6 5)

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: forall c hc. (Cr.Crypto c, Cr.HeaderCrypto hc) => Proxy hc -> ShelleyTxBody (ShelleyEra c)
txbodyEx1 phc =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    (StrictSeq.singleton $ ShelleyTxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertGenesis
            ( GenesisDelegCert
                (hashKey (coreNodeVK @c @hc 0))
                (hashKey (vKey newGenDelegate))
                (newGenesisVrfKH phc)
            )
        ]
    )
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceCoinEx1 = aliceInitCoin <-> Val.inject feeTx1
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

txEx1 ::
  forall c hc.
  ( Cr.Crypto c,
    Cr.HeaderCrypto hc,
    Signable (CryptoClass.DSIGN c) (Hash.Hash (CryptoClass.HASH c) EraIndependentTxBody)
  ) =>
  Proxy hc ->
  ShelleyTx (ShelleyEra c)
txEx1 phc = ShelleyTx (txbodyEx1 phc) txwits SNothing
  where
    txwits :: ShelleyWitnesses (ShelleyEra c)
    txwits =
      mempty
        { addrWits =
            makeWitnessesVKey @c
              (hashAnnotated (txbodyEx1 phc))
              ( [asWitness Cast.alicePay]
                  <> [ asWitness $
                         KeyPair @'Genesis @c
                           (coreNodeVK @c @hc 0)
                           (coreNodeSK @c @hc 0)
                     ]
              )
        }

blockEx1 ::
  forall c hc.
  (ExMock (Crypto (ShelleyEra c)) hc) =>
  Proxy hc ->
  Block (BHeader c hc) (ShelleyEra c)
blockEx1 _ =
  mkBlockFakeVRF @(ShelleyEra c)
    lastByronHeaderHash
    (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10)
    [txEx1 (Proxy :: Proxy hc)]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @c)
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert @c (coreNodeKeysBySchedule @(ShelleyEra c) ppEx 10) 0 (KESPeriod 0))

newGenDeleg ::
  forall crypto hcrypto.
  (Cr.Crypto crypto, Cr.HeaderCrypto hcrypto) =>
  Proxy hcrypto ->
  (FutureGenDeleg crypto, GenDelegPair crypto)
newGenDeleg phc =
  ( FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVK @crypto @hcrypto 0),
    GenDelegPair (hashKey . vKey $ newGenDelegate) (newGenesisVrfKH phc)
  )

data Proxy c = Proxy

expectedStEx1 ::
  forall c hc.
  (ExMock c hc) =>
  Proxy hc ->
  ChainState (ShelleyEra c)
expectedStEx1 phc =
  C.evolveNonceUnfrozen (getBlockNonce @(ShelleyEra c) (blockEx1 phc))
    . C.newLab (blockEx1 phc)
    . C.feesAndDeposits feeTx1 (Coin 0)
    . C.newUTxO (txbodyEx1 phc)
    . C.setFutureGenDeleg (newGenDeleg phc)
    $ (initStGenesisDeleg phc)

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, stage a new future genesis delegate
genesisDelegation1 ::
  forall c hc.
  (ExMock c hc) =>
  Proxy hc ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
genesisDelegation1 phc = CHAINExample (initStGenesisDeleg phc) (blockEx1 phc) (Right $ expectedStEx1 phc)

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall c hc.
  (ExMock (Crypto (ShelleyEra c)) hc) =>
  Proxy hc ->
  Block (BHeader c hc) (ShelleyEra c)
blockEx2 phc =
  mkBlockFakeVRF @(ShelleyEra c)
    (bhHash $ bheader (blockEx1 phc))
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

pulserEx2 :: (ExMock c hc, C.UsesPP (ShelleyEra c)) => Proxy hc -> PulsingRewUpdate c
pulserEx2 phc = makePulser' (expectedStEx1 phc)

expectedStEx2 ::
  forall c hc.
  (ExMock c hc, C.UsesPP (ShelleyEra c)) =>
  Proxy hc ->
  ChainState (ShelleyEra c)
expectedStEx2 phc =
  C.evolveNonceUnfrozen (getBlockNonce @(ShelleyEra c) (blockEx2 phc))
    . C.newLab (blockEx2 phc)
    . C.adoptFutureGenDeleg (newGenDeleg phc)
    . C.pulserUpdate (pulserEx2 phc)
    $ expectedStEx1 phc

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to trigger adopting the genesis delegation.
genesisDelegation2 ::
  (ExMock c hc, C.UsesPP (ShelleyEra c)) =>
  Proxy hc ->
  CHAINExample (BHeader c hc) (ShelleyEra c) hc
genesisDelegation2 phc = CHAINExample (expectedStEx1 phc) (blockEx2 phc) (Right $ expectedStEx2 phc)

--
-- Genesis Delegation Test Group
--

genesisDelegExample :: TestTree
genesisDelegExample =
  testGroup
    "genesis delegation"
    [ testCase "stage genesis key delegation" $ testCHAINExample (genesisDelegation1 (Proxy :: Proxy C_Crypto)),
      testCase "adopt genesis key delegation" $ testCHAINExample (genesisDelegation2 (Proxy :: Proxy C_Crypto))
    ]
