{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.Fees
  ( sizeTests
  )
where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))

import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map (empty, singleton)
import           Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Cardano.Binary (serialize)
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), textToDns, textToUrl)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern RegKey, pattern RegPool, pattern RetirePool)
import           Shelley.Spec.Ledger.Keys (KeyRole(..), asWitness, hashKey, vKey)
import           Shelley.Spec.Ledger.LedgerState (genesisId, txsize)
import qualified Shelley.Spec.Ledger.MetaData as MD
import           Shelley.Spec.Ledger.Scripts (pattern RequireMOf, pattern RequireSignature)
import           Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Tx (pattern Tx, hashScript, _body, _metadata, _witnessMSigMap,
                     _witnessVKeySet)
import           Shelley.Spec.Ledger.TxData (pattern DCertDeleg, pattern DCertPool,
                     pattern Delegation, pattern KeyHashObj, PoolMetaData (..), pattern PoolParams,
                     pattern RewardAcnt, StakePoolRelay (..), pattern TxBody, pattern TxIn,
                     pattern TxOut, Wdrl (..), _certs, _inputs, _mdHash, _outputs, _poolCost,
                     _poolMD, _poolMDHash, _poolMDUrl, _poolMargin, _poolOwners, _poolPledge,
                     _poolPubKey, _poolRAcnt, _poolRelays, _poolVrf, _ttl, _txUpdate, _txfee,
                     _wdrls)
import           Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessesVKey)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, ConcreteCrypto, Credential,
                     KeyHash, MultiSig, PoolParams, SignKeyVRF, Tx, TxBody, VerKeyVRF,
                     hashKeyVRF, KeyPair, pattern KeyPair)
import           Test.Shelley.Spec.Ledger.Utils


sizeTest :: BSL.ByteString -> Tx -> Integer -> Assertion
sizeTest b16 tx s = do
  (Base16.encode (serialize tx) @?= b16) >> (txsize tx @?= s)

alicePay :: KeyPair 'Payment
alicePay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0,0,0,0,0)

aliceStake :: KeyPair 'Staking
aliceStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0,0,0,0,1)

aliceSHK :: Credential 'Staking
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

alicePool :: KeyPair 'StakePool
alicePool = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0,0,0,0,2)

alicePoolKH :: KeyHash 'StakePool
alicePoolKH = (hashKey . vKey) alicePool

aliceVRF :: (SignKeyVRF, VerKeyVRF)
aliceVRF = mkVRFKeyPair (0,0,0,0,3)

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
    , _poolRelays = StrictSeq.singleton $ SingleHostName SNothing $ fromJust $ textToDns "relay.io"
    , _poolMD = SJust $ PoolMetaData
                  { _poolMDUrl  = fromJust $ textToUrl "alice.pool"
                  , _poolMDHash = BS.pack "{}"
                  }
    }

aliceAddr :: Addr
aliceAddr = mkAddr (alicePay, aliceStake)

bobPay :: KeyPair 'Payment
bobPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1,0,0,0,0)

bobStake :: KeyPair 'Staking
bobStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1,0,0,0,1)

bobSHK :: Credential 'Staking
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

bobAddr :: Addr
bobAddr = mkAddr (bobPay, bobStake)

carlPay :: KeyPair 'Payment
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (2,0,0,0,0)


-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness

txbSimpleUTxO :: TxBody
txbSimpleUTxO = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txSimpleUTxO :: Tx
txSimpleUTxO = Tx
  { _body           = txbSimpleUTxO
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbSimpleUTxO) [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txSimpleUTxOBytes16 :: BSL.ByteString
txSimpleUTxOBytes16 = "83a4009f82446050074300ff0181840044aee84c70440acd1b520a02185e030aa10081821b30303030303231348244eb4659ba1b3030303030323134f6"

-- | Transaction which consumes two UTxO and creates five UTxO
-- | and has two witness

txbMutiUTxO :: TxBody
txbMutiUTxO = TxBody
  { _inputs   = Set.fromList [ TxIn genesisId 0
                             , TxIn genesisId 1
                             ]
  , _outputs  = StrictSeq.fromList
                [ TxOut aliceAddr (Coin 10)
                , TxOut aliceAddr (Coin 20)
                , TxOut aliceAddr (Coin 30)
                , TxOut bobAddr   (Coin 40)
                , TxOut bobAddr   (Coin 50)
                ]
  , _certs    = StrictSeq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 199
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txMutiUTxO :: Tx
txMutiUTxO = Tx
  { _body           = txbMutiUTxO
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbMutiUTxO)
                      [ alicePay
                      , bobPay
                      ]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txMutiUTxOBytes16 :: BSL.ByteString
txMutiUTxOBytes16 = "83a4009f8244605007430082446050074301ff0185840044aee84c70440acd1b520a840044aee84c70440acd1b5214840044aee84c70440acd1b52181e840044d42eadb144d42eadb11828840044d42eadb144d42eadb118320218c7030aa10082821b30303030303231348244bbedc57a1b3030303030323134821b31353438353836378244bbedc57a1b3135343835383637f6"

-- | Transaction which registers a stake key

txbRegisterStake :: TxBody
txbRegisterStake = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.fromList [ DCertDeleg (RegKey aliceSHK) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txRegisterStake :: Tx
txRegisterStake = Tx
  { _body           = txbRegisterStake
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbRegisterStake) [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txRegisterStakeBytes16 :: BSL.ByteString
txRegisterStakeBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a048182008200440acd1b52a10081821b303030303032313482443dff0e3e1b3030303030323134f6"

-- | Transaction which delegates a stake key

txbDelegateStake :: TxBody
txbDelegateStake = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.fromList [ DCertDeleg (Delegate $ Delegation bobSHK alicePoolKH) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txDelegateStake :: Tx
txDelegateStake = Tx
  { _body           = txbDelegateStake
  , _witnessVKeySet = makeWitnessesVKey
                        (hashTxBody txbDelegateStake)
                        [asWitness alicePay, asWitness bobStake]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txDelegateStakeBytes16 :: BSL.ByteString
txDelegateStakeBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a04818302820044d42eadb14450d474d0a10082821b303030303032313482446d2304911b3030303030323134821b313534383538363782446d2304911b3135343835383637f6"

-- | Transaction which de-registers a stake key

txbDeregisterStake :: TxBody
txbDeregisterStake = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.fromList [ DCertDeleg (DeRegKey aliceSHK) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txDeregisterStake :: Tx
txDeregisterStake = Tx
  { _body           = txbDeregisterStake
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbDeregisterStake) [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }


txDeregisterStakeBytes16 :: BSL.ByteString
txDeregisterStakeBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a048182018200440acd1b52a10081821b30303030303231348244bd35013b1b3030303030323134f6"

-- | Transaction which registers a stake pool

txbRegisterPool :: TxBody
txbRegisterPool = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.fromList [ DCertPool (RegPool alicePoolParams) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txRegisterPool :: Tx
txRegisterPool = Tx
  { _body           = txbRegisterPool
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbRegisterPool) [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txRegisterPoolBytes16 :: BSL.ByteString
txRegisterPoolBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a04818a034450d474d04495286b7b0105d81e82010a8200440acd1b5281440acd1b52818301f66872656c61792e696f826a616c6963652e706f6f6c427b7da10081821b30303030303231348244b9f105371b3030303030323134f6"

-- | Transaction which retires a stake pool

txbRetirePool :: TxBody
txbRetirePool = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.fromList [ DCertPool (RetirePool alicePoolKH (EpochNo 5)) ]
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txRetirePool :: Tx
txRetirePool = Tx
  { _body           = txbRetirePool
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbRetirePool) [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txRetirePoolBytes16 :: BSL.ByteString
txRetirePoolBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a048183044450d474d005a10081821b303030303032313482443267fcc41b3030303030323134f6"

-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness

md :: MD.MetaData
md = MD.MetaData $ Map.singleton 0 (MD.List [ MD.I 5, MD.S "hello"])

txbWithMD :: TxBody
txbWithMD = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SJust $ MD.hashMetaData @ConcreteCrypto md
  }

txWithMD :: Tx
txWithMD = Tx
  { _body           = txbWithMD
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbWithMD) [alicePay]
  , _witnessMSigMap = Map.empty
  , _metadata       = SJust md
  }

txWithMDBytes16 :: BSL.ByteString
txWithMDBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a07444eece652a10081821b3030303030323134824469e542f81b3030303030323134a10082056568656c6c6f"

-- | Spending from a multi-sig address

msig :: MultiSig
msig = RequireMOf 2
  [ (RequireSignature . asWitness . hashKey . vKey) alicePay
  , (RequireSignature . asWitness . hashKey . vKey) bobPay
  , (RequireSignature . asWitness . hashKey . vKey) carlPay
  ]

txbWithMultiSig :: TxBody
txbWithMultiSig = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0] -- acting as if this is multi-sig
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.empty
  , _wdrls    = Wdrl Map.empty
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txWithMultiSig :: Tx
txWithMultiSig = Tx
  { _body           = txbWithMultiSig
  , _witnessVKeySet = makeWitnessesVKey (hashTxBody txbWithMultiSig) [alicePay, bobPay]
  , _witnessMSigMap = Map.singleton (hashScript msig) msig
  , _metadata       = SNothing
  }

txWithMultiSigBytes16 :: BSL.ByteString
txWithMultiSigBytes16 = "83a4009f82446050074300ff0181840044aee84c70440acd1b520a02185e030aa20082821b30303030303231348244eb4659ba1b3030303030323134821b31353438353836378244eb4659ba1b3135343835383637018183030283820044aee84c70820044d42eadb1820044ae010e05f6"

-- | Transaction with a Reward Withdrawal

txbWithWithdrawal :: TxBody
txbWithWithdrawal = TxBody
  { _inputs   = Set.fromList [TxIn genesisId 0]
  , _outputs  = StrictSeq.fromList [TxOut aliceAddr (Coin 10)]
  , _certs    = StrictSeq.empty
  , _wdrls    = Wdrl $ Map.singleton (RewardAcnt aliceSHK) 100
  , _txfee    = Coin 94
  , _ttl      = SlotNo 10
  , _txUpdate = SNothing
  , _mdHash   = SNothing
  }

txWithWithdrawal :: Tx
txWithWithdrawal = Tx
  { _body           = txbWithWithdrawal
  , _witnessVKeySet = makeWitnessesVKey
                        (hashTxBody txbWithWithdrawal)
                        [asWitness alicePay, asWitness aliceStake]
  , _witnessMSigMap = Map.empty
  , _metadata       = SNothing
  }

txWithWithdrawalBytes16 :: BSL.ByteString
txWithWithdrawalBytes16 = "83a5009f82446050074300ff0181840044aee84c70440acd1b520a02185e030a05a18200440acd1b521864a10082821b3030303038363032824442b5d6c41b3030303038363032821b3030303030323134824442b5d6c41b3030303030323134f6"

-- NOTE the txsize function takes into account which actual crypto parameter is use.
-- These tests are using ShortHash and MockDSIGN so that:
--       the hash length is ------------>  4
--       the verification key size is -->  8
--       the signature size is ---------> 13

sizeTests :: TestTree
sizeTests = testGroup "Fee Tests"
  [ testCase "simple utxo" $ sizeTest txSimpleUTxOBytes16 txSimpleUTxO 145
  , testCase "multiple utxo" $ sizeTest txMutiUTxOBytes16 txMutiUTxO 470
  , testCase "register stake key" $ sizeTest txRegisterStakeBytes16 txRegisterStake 156
  , testCase "delegate stake key" $ sizeTest txDelegateStakeBytes16 txDelegateStake 186
  , testCase "deregister stake key" $ sizeTest txDeregisterStakeBytes16 txDeregisterStake 156
  , testCase "register stake pool" $ sizeTest txRegisterPoolBytes16 txRegisterPool 207
  , testCase "retire stake pool" $ sizeTest txRetirePoolBytes16 txRetirePool 155
  , testCase "metadata" $ sizeTest txWithMDBytes16 txWithMD 160
  , testCase "multisig" $ sizeTest txWithMultiSigBytes16 txWithMultiSig 197
  , testCase "reward withdrawal" $ sizeTest txWithWithdrawalBytes16 txWithWithdrawal 181
  ]
