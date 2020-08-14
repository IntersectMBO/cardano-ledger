{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Fees
  ( sizeTests,
  )
where

import Cardano.Binary (serialize)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map (empty, singleton)
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API
  ( Addr,
    Credential (..),
    DCert (..),
    DelegCert (..),
    Delegation (..),
    MultiSig (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    SignKeyVRF,
    Tx (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    VerKeyVRF,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    textToDns,
    textToUrl,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Crypto ()
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState (txsize)
import qualified Shelley.Spec.Ledger.MetaData as MD
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (..),
    hashScript,
  )
import Shelley.Spec.Ledger.TxData
  ( PoolMetaData (..),
    StakePoolRelay (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (makeWitnessesVKey)
import Shelley.Spec.Ledger.Value(CV)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Generator.Core (genesisId)
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

sizeTest :: CV c v => proxy c -> BSL.ByteString -> Tx c v -> Integer -> Assertion
sizeTest _ b16 tx s = do
  (Base16.encode (serialize tx) @?= b16) >> (txsize tx @?= s)

alicePay :: KeyPair 'Payment C
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair 'Staking C
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

aliceSHK :: Credential 'Staking C
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

alicePool :: KeyPair 'StakePool C
alicePool = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 2)

alicePoolKH :: KeyHash 'StakePool C
alicePoolKH = (hashKey . vKey) alicePool

aliceVRF :: (SignKeyVRF C, VerKeyVRF C)
aliceVRF = mkVRFKeyPair (0, 0, 0, 0, 3)

alicePoolParams :: PoolParams C
alicePoolParams =
  PoolParams
    { _poolPubKey = alicePoolKH,
      _poolVrf = hashVerKeyVRF . snd $ aliceVRF,
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet aliceSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays = StrictSeq.singleton $ SingleHostName SNothing $ fromJust $ textToDns "relay.io",
      _poolMD =
        SJust $
          PoolMetaData
            { _poolMDUrl = fromJust $ textToUrl "alice.pool",
              _poolMDHash = BS.pack "{}"
            }
    }

aliceAddr :: Addr C
aliceAddr = mkAddr (alicePay, aliceStake)

bobPay :: KeyPair 'Payment C
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (1, 0, 0, 0, 0)

bobStake :: KeyPair 'Staking C
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (1, 0, 0, 0, 1)

bobSHK :: Credential 'Staking C
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

bobAddr :: Addr C
bobAddr = mkAddr (bobPay, bobStake)

carlPay :: KeyPair 'Payment C
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (2, 0, 0, 0, 0)

-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness
txbSimpleUTxO :: TxBody C Coin
txbSimpleUTxO =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txSimpleUTxO :: Tx C Coin
txSimpleUTxO =
  Tx
    { _body = txbSimpleUTxO,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbSimpleUTxO) [alicePay]
          },
      _metadata = SNothing
    }

txSimpleUTxOBytes16 :: BSL.ByteString
txSimpleUTxOBytes16 = "83a4008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030aa1008182487c6ffc08d6fa98ad50fb797ba5f94e620a7c6ffc08d6fa98adf6"

-- | Transaction which consumes two UTxO and creates five UTxO
-- | and has two witness
txbMutiUTxO :: TxBody C Coin
txbMutiUTxO =
  TxBody
    { _inputs =
        Set.fromList
          [ TxIn genesisId 0,
            TxIn genesisId 1
          ],
      _outputs =
        StrictSeq.fromList
          [ TxOut aliceAddr (Coin 10),
            TxOut aliceAddr (Coin 20),
            TxOut aliceAddr (Coin 30),
            TxOut bobAddr (Coin 40),
            TxOut bobAddr (Coin 50)
          ],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 199,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txMutiUTxO :: Tx C Coin
txMutiUTxO =
  Tx
    { _body = txbMutiUTxO,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated txbMutiUTxO)
                [ alicePay,
                  bobPay
                ]
          },
      _metadata = SNothing
    }

txMutiUTxOBytes16 :: BSL.ByteString
txMutiUTxOBytes16 = "83a4008282489db8a41713ad20240082489db8a41713ad2024010185825100d58133b22743fae3ac6eb45e783a9cd90a825100d58133b22743fae3ac6eb45e783a9cd914825100d58133b22743fae3ac6eb45e783a9cd9181e825100595ced90e8df7cda4b9d3b869eab9a271828825100595ced90e8df7cda4b9d3b869eab9a2718320218c7030aa100828248933c542202176b765048afa776af8a3729933c542202176b7682487c6ffc08d6fa98ad5048afa776af8a37297c6ffc08d6fa98adf6"

-- | Transaction which registers a stake key
txbRegisterStake :: TxBody C Coin
txbRegisterStake =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.fromList [DCertDeleg (RegKey aliceSHK)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txRegisterStake :: Tx C Coin
txRegisterStake =
  Tx
    { _body = txbRegisterStake,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbRegisterStake) [alicePay]
          },
      _metadata = SNothing
    }

txRegisterStakeBytes16 :: BSL.ByteString
txRegisterStakeBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a04818200820048ac6eb45e783a9cd9a1008182487c6ffc08d6fa98ad50ddfb872ba72a945c7c6ffc08d6fa98adf6"

-- | Transaction which delegates a stake key
txbDelegateStake :: TxBody C Coin
txbDelegateStake =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.fromList [DCertDeleg (Delegate $ Delegation bobSHK alicePoolKH)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txDelegateStake :: Tx C Coin
txDelegateStake =
  Tx
    { _body = txbDelegateStake,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated txbDelegateStake)
                [asWitness alicePay, asWitness bobStake]
          },
      _metadata = SNothing
    }

txDelegateStakeBytes16 :: BSL.ByteString
txDelegateStakeBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a048183028200484b9d3b869eab9a2748089b36543d810124a100828248573bf7473760f6b350159b18405d44466b573bf7473760f6b382487c6ffc08d6fa98ad50159b18405d44466b7c6ffc08d6fa98adf6"

-- | Transaction which de-registers a stake key
txbDeregisterStake :: TxBody C Coin
txbDeregisterStake =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.fromList [DCertDeleg (DeRegKey aliceSHK)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txDeregisterStake :: Tx C Coin
txDeregisterStake =
  Tx
    { _body = txbDeregisterStake,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbDeregisterStake) [alicePay]
          },
      _metadata = SNothing
    }

txDeregisterStakeBytes16 :: BSL.ByteString
txDeregisterStakeBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a04818201820048ac6eb45e783a9cd9a1008182487c6ffc08d6fa98ad50de99c767682e0d477c6ffc08d6fa98adf6"

-- | Transaction which registers a stake pool
txbRegisterPool :: TxBody C Coin
txbRegisterPool =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.fromList [DCertPool (RegPool alicePoolParams)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txRegisterPool :: Tx C Coin
txRegisterPool =
  Tx
    { _body = txbRegisterPool,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbRegisterPool) [alicePay]
          },
      _metadata = SNothing
    }

txRegisterPoolBytes16 :: BSL.ByteString
txRegisterPoolBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a04818a0348089b36543d81012448c6242f6c7faccf7b0105d81e82010a49e0ac6eb45e783a9cd98148ac6eb45e783a9cd9818301f66872656c61792e696f826a616c6963652e706f6f6c427b7da1008182487c6ffc08d6fa98ad50bbd60c42b34ad7fd7c6ffc08d6fa98adf6"

-- | Transaction which retires a stake pool
txbRetirePool :: TxBody C Coin
txbRetirePool =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.fromList [DCertPool (RetirePool alicePoolKH (EpochNo 5))],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txRetirePool :: Tx C Coin
txRetirePool =
  Tx
    { _body = txbRetirePool,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbRetirePool) [alicePay]
          },
      _metadata = SNothing
    }

txRetirePoolBytes16 :: BSL.ByteString
txRetirePoolBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a0481830448089b36543d81012405a1008182487c6ffc08d6fa98ad50ac132140948070ac7c6ffc08d6fa98adf6"

-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness
md :: MD.MetaData
md = MD.MetaData $ Map.singleton 0 (MD.List [MD.I 5, MD.S "hello"])

txbWithMD :: TxBody C Coin
txbWithMD =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SJust $ MD.hashMetaData md
    }

txWithMD :: Tx C Coin
txWithMD =
  Tx
    { _body = txbWithMD,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbWithMD) [alicePay]
          },
      _metadata = SJust md
    }

txWithMDBytes16 :: BSL.ByteString
txWithMDBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a07484eece6527f366cfaa1008182487c6ffc08d6fa98ad5046519a847ff016bc7c6ffc08d6fa98ada10082056568656c6c6f"

-- | Spending from a multi-sig address
msig :: MultiSig C
msig =
  RequireMOf
    2
    [ (RequireSignature . asWitness . hashKey . vKey) alicePay,
      (RequireSignature . asWitness . hashKey . vKey) bobPay,
      (RequireSignature . asWitness . hashKey . vKey) carlPay
    ]

txbWithMultiSig :: TxBody C Coin
txbWithMultiSig =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0], -- acting as if this is multi-sig
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txWithMultiSig :: Tx C Coin
txWithMultiSig =
  Tx
    { _body = txbWithMultiSig,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated txbWithMultiSig) [alicePay, bobPay],
            msigWits = Map.singleton (hashScript msig) msig
          },
      _metadata = SNothing
    }

txWithMultiSigBytes16 :: BSL.ByteString
txWithMultiSigBytes16 = "83a4008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030aa200828248933c542202176b7650fb797ba5f94e620a933c542202176b7682487c6ffc08d6fa98ad50fb797ba5f94e620a7c6ffc08d6fa98ad018183030283820048d58133b22743fae3820048595ced90e8df7cda8200484afb593da12003f4f6"

-- | Transaction with a Reward Withdrawal
txbWithWithdrawal :: TxBody C Coin
txbWithWithdrawal =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl $ Map.singleton (RewardAcnt Testnet aliceSHK) 100,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txWithWithdrawal :: Tx C Coin
txWithWithdrawal =
  Tx
    { _body = txbWithWithdrawal,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated txbWithWithdrawal)
                [asWitness alicePay, asWitness aliceStake]
          },
      _metadata = SNothing
    }

txWithWithdrawalBytes16 :: BSL.ByteString
txWithWithdrawalBytes16 = "83a5008182489db8a41713ad2024000181825100d58133b22743fae3ac6eb45e783a9cd90a02185e030a05a149e0ac6eb45e783a9cd91864a100828248c1eb01540061300550d8806cecf2ddbb5cc1eb01540061300582487c6ffc08d6fa98ad50d8806cecf2ddbb5c7c6ffc08d6fa98adf6"

-- NOTE the txsize function takes into account which actual crypto parameter is use.
-- These tests are using ShortHash and MockDSIGN so that:
--       the hash length is ------------>  4
--       the verification key size is -->  8
--       the signature size is ---------> 13

sizeTests :: TestTree
sizeTests =
  testGroup
    "Fee Tests"
    [ testCase "simple utxo" $ sizeTest p txSimpleUTxOBytes16 txSimpleUTxO 73,
      testCase "multiple utxo" $ sizeTest p txMutiUTxOBytes16 txMutiUTxO 194,
      testCase "register stake key" $ sizeTest p txRegisterStakeBytes16 txRegisterStake 88,
      testCase "delegate stake key" $ sizeTest p txDelegateStakeBytes16 txDelegateStake 124,
      testCase "deregister stake key" $ sizeTest p txDeregisterStakeBytes16 txDeregisterStake 88,
      testCase "register stake pool" $ sizeTest p txRegisterPoolBytes16 txRegisterPool 150,
      testCase "retire stake pool" $ sizeTest p txRetirePoolBytes16 txRetirePool 87,
      testCase "metadata" $ sizeTest p txWithMDBytes16 txWithMD 92,
      testCase "multisig" $ sizeTest p txWithMultiSigBytes16 txWithMultiSig 139,
      testCase "reward withdrawal" $ sizeTest p txWithWithdrawalBytes16 txWithWithdrawal 114
    ]
  where
    p :: Proxy C
    p = Proxy
