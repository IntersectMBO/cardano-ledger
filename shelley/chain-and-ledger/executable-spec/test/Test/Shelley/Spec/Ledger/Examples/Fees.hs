{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.Fees
  ( sizeTests
  )
where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map (empty, singleton)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import           Cardano.Binary (decodeFullDecoder, fromCBOR)
import           Shelley.Spec.Ledger.BaseTypes (text64)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern RegKey, pattern RegPool, pattern RetirePool)
import           Shelley.Spec.Ledger.Keys (pattern KeyPair, hashKey, undiscriminateKeyHash, vKey)
import           Shelley.Spec.Ledger.LedgerState (genesisId, txsize)
import qualified Shelley.Spec.Ledger.MetaData as MD
import           Shelley.Spec.Ledger.Scripts (pattern RequireMOf, pattern RequireSignature)
import           Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Tx (pattern Tx, hashScript, _body, _metadata, _witnessMSigMap,
                     _witnessVKeySet)
import           Shelley.Spec.Ledger.TxData (pattern DCertDeleg, pattern DCertPool,
                     pattern Delegation, pattern KeyHashObj, PoolMetaData (..), pattern PoolParams,
                     pattern RewardAcnt, pattern TxBody, pattern TxIn, pattern TxOut, Url (..),
                     Wdrl (..), _certs, _inputs, _mdHash, _outputs, _poolCost, _poolMD,
                     _poolMDHash, _poolMDUrl, _poolMargin, _poolOwners, _poolPledge, _poolPubKey,
                     _poolRAcnt, _poolRelays, _poolVrf, _ttl, _txUpdate, _txfee, _wdrls)
import           Shelley.Spec.Ledger.Updates (emptyUpdate)
import           Shelley.Spec.Ledger.UTxO (makeWitnessesVKey)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, ConcreteCrypto, Credential,
                     KeyHash, KeyPair, MultiSig, PoolParams, SignKeyVRF, Tx, TxBody, VerKeyVRF,
                     hashKeyVRF)
import           Test.Shelley.Spec.Ledger.Utils


sizeTest :: BSL.ByteString -> Tx -> Integer -> Assertion
sizeTest b16 tx s = do
  let (b, _) = Base16.decode b16
  decoded <- case decodeFullDecoder "cbor test" fromCBOR b of
    Right t -> pure t
    Left e  -> assertFailure $ "sizeTest" <> show e
  (decoded @?= tx) >> (txsize tx @?= s)

alicePay :: KeyPair
alicePay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair
aliceStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

aliceSHK :: Credential
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

alicePool :: KeyPair
alicePool = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 2)

alicePoolKH :: KeyHash
alicePoolKH = (hashKey . vKey) alicePool

aliceVRF :: (SignKeyVRF, VerKeyVRF)
aliceVRF = mkVRFKeyPair (0, 0, 0, 0, 3)

alicePoolParams :: PoolParams
alicePoolParams =
  PoolParams
    { _poolPubKey = alicePoolKH
    , _poolVrf = hashKeyVRF . snd $ aliceVRF
    , _poolPledge = Coin 1
    , _poolCost = Coin 5
    , _poolMargin = unsafeMkUnitInterval 0.1
    , _poolRAcnt = RewardAcnt aliceSHK
    , _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake
    , _poolRelays = (Seq.singleton . Url . text64) "relay.io"
    , _poolMD = Just $ PoolMetaData
                  { _poolMDUrl  = Url $ text64 "alice.pool"
                  , _poolMDHash = BS.pack "{}"
                  }
    }

aliceAddr :: Addr
aliceAddr = mkAddr (alicePay, aliceStake)

bobPay :: KeyPair
bobPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1, 0, 0, 0, 0)

bobStake :: KeyPair
bobStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1, 0, 0, 0, 1)

bobSHK :: Credential
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

bobAddr :: Addr
bobAddr = mkAddr (bobPay, bobStake)

carlPay :: KeyPair
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (2, 0, 0, 0, 0)


-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness

txbSimpleUTxO :: TxBody
txbSimpleUTxO = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txSimpleUTxO :: Tx
txSimpleUTxO = Tx
  { _body           = txbSimpleUTxO
  , _witnessVKeySet = makeWitnessesVKey txbSimpleUTxO [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txSimpleUTxOBytes16 :: BSL.ByteString
txSimpleUTxOBytes16 = "83a400d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030aa10081821a6753449f824489ec679d1a6753449f80"


-- | Transaction which consumes two UTxO and creates five UTxO
-- | and has two witness

txbMutiUTxO :: TxBody
txbMutiUTxO = TxBody
  { _inputs   = Set.fromList [ TxIn genesisId 0
                             , TxIn genesisId 1
                             ]
  , _outputs  = Seq.fromList [ TxOut aliceAddr (Coin 10)
                             , TxOut aliceAddr (Coin 20)
                             , TxOut aliceAddr (Coin 30)
                             , TxOut bobAddr   (Coin 40)
                             , TxOut bobAddr   (Coin 50)
                             ]
  , _certs    = Seq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 199
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txMutiUTxO :: Tx
txMutiUTxO = Tx
  { _body           = txbMutiUTxO
  , _witnessVKeySet = makeWitnessesVKey txbMutiUTxO
                      [ alicePay
                      , bobPay
                      ]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txMutiUTxOBytes16 :: BSL.ByteString
txMutiUTxOBytes16 = "83a400d90102828244b45c4891008244b45c4891010185840044cfb2c4144476394f7a0a840044cfb2c4144476394f7a14840044cfb2c4144476394f7a181e840044f079394944bcbe39001828840044f079394944bcbe390018320218c7030aa10082821a6753449f8244219dc2b31a6753449f821a3344eb568244219dc2b31a3344eb5680"

-- | Transaction which registers a stake key

txbRegisterStake :: TxBody
txbRegisterStake = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.fromList [ DCertDeleg (RegKey aliceSHK) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txRegisterStake :: Tx
txRegisterStake = Tx
  { _body           = txbRegisterStake
  , _witnessVKeySet = makeWitnessesVKey txbRegisterStake [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txRegisterStakeBytes16 :: BSL.ByteString
txRegisterStakeBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a048182004476394f7aa10081821a6753449f8244ddb63dc11a6753449f80"

-- | Transaction which delegates a stake key

txbDelegateStake :: TxBody
txbDelegateStake = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.fromList [ DCertDeleg (Delegate $ Delegation bobSHK alicePoolKH) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txDelegateStake :: Tx
txDelegateStake = Tx
  { _body           = txbDelegateStake
  , _witnessVKeySet = makeWitnessesVKey txbDelegateStake [alicePay, bobStake]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txDelegateStakeBytes16 :: BSL.ByteString
txDelegateStakeBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a0481830444bcbe3900442b7dd894a10082821a687c686f8244a61602491a687c686f821a6753449f8244a61602491a6753449f80"

-- | Transaction which de-registers a stake key

txbDeregisterStake :: TxBody
txbDeregisterStake = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.fromList [ DCertDeleg (DeRegKey aliceSHK) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txDeregisterStake :: Tx
txDeregisterStake = Tx
  { _body           = txbDeregisterStake
  , _witnessVKeySet = makeWitnessesVKey txbDeregisterStake [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }


txDeregisterStakeBytes16 :: BSL.ByteString
txDeregisterStakeBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a048182024476394f7aa10081821a6753449f8244518af51d1a6753449f80"

-- | Transaction which registers a stake pool

txbRegisterPool :: TxBody
txbRegisterPool = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.fromList [ DCertPool (RegPool alicePoolParams) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txRegisterPool :: Tx
txRegisterPool = Tx
  { _body           = txbRegisterPool
  , _witnessVKeySet = makeWitnessesVKey txbRegisterPool [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txRegisterPoolBytes16 :: BSL.ByteString
txRegisterPoolBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a04818a06442b7dd89444bf6afc170105d81e82010a82004476394f7ad90102814476394f7a816872656c61792e696f81826a616c6963652e706f6f6c427b7da10081821a6753449f8244530b23471a6753449f80"

-- | Transaction which retires a stake pool

txbRetirePool :: TxBody
txbRetirePool = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.fromList [ DCertPool (RetirePool alicePoolKH (EpochNo 5)) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txRetirePool :: Tx
txRetirePool = Tx
  { _body           = txbRetirePool
  , _witnessVKeySet = makeWitnessesVKey txbRetirePool [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txRetirePoolBytes16 :: BSL.ByteString
txRetirePoolBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a04818307442b7dd89405a10081821a6753449f824404d81fe41a6753449f80"

-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness

md :: MD.MetaData
md = MD.MetaData $ Map.singleton 0 (MD.List [ MD.I 5, MD.S "hello"])

txbWithMD :: TxBody
txbWithMD = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Just $ MD.hashMetaData @ConcreteCrypto md
  }

txWithMD :: Tx
txWithMD = Tx
  { _body           = txbWithMD
  , _witnessVKeySet = makeWitnessesVKey txbWithMD [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = Just md
  }

txWithMDBytes16 :: BSL.ByteString
txWithMDBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a07444eece652a10081821a6753449f8244b0436c041a6753449f81a10082056568656c6c6f"

-- | Spending from a multi-sig address

msig :: MultiSig
msig = RequireMOf 2
  [ (RequireSignature . undiscriminateKeyHash . hashKey . vKey) alicePay
  , (RequireSignature . undiscriminateKeyHash . hashKey . vKey) bobPay
  , (RequireSignature . undiscriminateKeyHash . hashKey . vKey) carlPay
  ]

txbWithMultiSig :: TxBody
txbWithMultiSig = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0] -- acting as if this is multi-sig
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txWithMultiSig :: Tx
txWithMultiSig = Tx
  { _body           = txbWithMultiSig
  , _witnessVKeySet = makeWitnessesVKey txbWithMultiSig [alicePay, bobPay]
  , _witnessMSigMap = Map.singleton (hashScript msig) msig
  , _metadata       = Nothing
  }

txWithMultiSigBytes16 :: BSL.ByteString
txWithMultiSigBytes16 = "83a400d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030aa20082821a6753449f824489ec679d1a6753449f821a3344eb56824489ec679d1a3344eb56018183030283820044cfb2c414820044f0793949820044cfff284080"

-- | Transaction with a Reward Withdrawal

txbWithWithdrawal :: TxBody
txbWithWithdrawal = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = Seq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = Seq.empty
  , _wdrls    = Wdrl $ Map.singleton (RewardAcnt aliceSHK) 100
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = emptyUpdate
  , _mdHash   = Nothing
  }

txWithWithdrawal :: Tx
txWithWithdrawal = Tx
  { _body           = txbWithWithdrawal
  , _witnessVKeySet = makeWitnessesVKey txbWithWithdrawal [alicePay, aliceStake]
  , _witnessMSigMap = Map.empty
  , _metadata       = Nothing
  }

txWithWithdrawalBytes16 :: BSL.ByteString
txWithWithdrawalBytes16 = "83a500d90102818244b45c4891000181840044cfb2c4144476394f7a0a02185e030a05a182004476394f7a1864a10082821afdd63a158244bebb7df71afdd63a15821a6753449f8244bebb7df71a6753449f80"

-- NOTE the txsize function takes into account which actual crypto parameter is use.
-- These tests are using ShortHash and MockDSIGN so that:
--       the hash length is ------------>  4
--       the verification key size is -->  8
--       the signature size is ---------> 13

sizeTests :: TestTree
sizeTests = testGroup "Fee Tests"
  [ testCase "simple utxo" $ sizeTest txSimpleUTxOBytes16 txSimpleUTxO 94
  , testCase "multiple utxo" $ sizeTest txMutiUTxOBytes16 txMutiUTxO 199
  , testCase "register stake key" $ sizeTest txRegisterStakeBytes16 txRegisterStake 102
  , testCase "delegate stake key" $ sizeTest txDelegateStakeBytes16 txDelegateStake 129
  , testCase "deregister stake key" $ sizeTest txDeregisterStakeBytes16 txDeregisterStake 102
  , testCase "register stake pool" $ sizeTest txRegisterPoolBytes16 txRegisterPool 173
  , testCase "retire stake pool" $ sizeTest txRetirePoolBytes16 txRetirePool 107
  , testCase "metadata" $ sizeTest txWithMDBytes16 txWithMD 237
  , testCase "multisig" $ sizeTest txWithMultiSigBytes16 txWithMultiSig 145
  , testCase "reward withdrawal" $ sizeTest txWithWithdrawalBytes16 txWithWithdrawal 127
  ]
