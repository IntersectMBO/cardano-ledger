{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.Fees
  ( feeTests
  )
where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict as Map (empty, singleton)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import           Shelley.Spec.Ledger.BaseTypes (text64)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern RegKey, pattern RegPool, pattern RetirePool)
import           Shelley.Spec.Ledger.Keys (pattern KeyPair, hashKey, undiscriminateKeyHash, vKey)
import           Shelley.Spec.Ledger.LedgerState (genesisId, txsize)
import qualified Shelley.Spec.Ledger.MetaData as MD
import           Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Tx (pattern Tx, hashScript, _body, _metadata, _witnessMSigMap,
                     _witnessVKeySet)
import           Shelley.Spec.Ledger.TxData (pattern DCertDeleg, pattern DCertPool,
                     pattern Delegation, pattern KeyHashObj, PoolMetaData (..), pattern PoolParams,
                     pattern RewardAcnt,
                     pattern TxBody, pattern TxIn, pattern TxOut, Url (..), Wdrl (..), _certs,
                     _inputs, _mdHash, _outputs, _poolCost, _poolMD, _poolMDHash, _poolMDUrl,
                     _poolMargin, _poolOwners, _poolPledge, _poolPubKey, _poolRAcnt, _poolRelays,
                     _poolVrf, _ttl, _txUpdate, _txfee, _wdrls)
import           Shelley.Spec.Ledger.Updates (emptyUpdate)
import           Shelley.Spec.Ledger.UTxO (makeWitnessesVKey)
import           Shelley.Spec.Ledger.Scripts (pattern RequireMOf, pattern RequireSignature)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, ConcreteCrypto, Credential,
                     KeyHash, KeyPair, MultiSig, PoolParams, SignKeyVRF, Tx, TxBody, VerKeyVRF,
                     hashKeyVRF)
import           Test.Shelley.Spec.Ledger.Utils



feeTest :: Tx -> Integer -> Assertion
feeTest tx fee = txsize tx @?= fee

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


-- NOTE the txsize function takes into account which actual crypto parameter is use.
-- These tests are using ShortHash and MockDSIGN so that:
--       the hash length is ------------>  4
--       the verification key size is -->  8
--       the signature size is ---------> 13

feeTests :: TestTree
feeTests = testGroup "Fee Tests"
  [ testCase "simple utxo" $ feeTest txSimpleUTxO 94
  , testCase "multiple utxo" $ feeTest txMutiUTxO 199
  , testCase "register stake key" $ feeTest txRegisterStake 102
  , testCase "delegate stake key" $ feeTest txDelegateStake 129
  , testCase "deregister stake key" $ feeTest txDeregisterStake 102
  , testCase "register stake pool" $ feeTest txRegisterPool 173
  , testCase "retire stake pool" $ feeTest txRetirePool 107
  , testCase "metadata" $ feeTest txWithMD 237
  , testCase "multisig" $ feeTest txWithMultiSig 145
  , testCase "reward withdrawal" $ feeTest txWithWithdrawal 127
  ]
