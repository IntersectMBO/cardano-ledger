{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Address
  ( addressTests,
  )
where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash (..), HashAlgorithm (..), ShortHash, hashFromBytes)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.VRF.Simple (SimpleVRF)
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Keys
  ( KeyRole (..),
    pattern KeyHash,
  )
import Shelley.Spec.Ledger.Scripts (pattern ScriptHash)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

addressTests :: TestTree
addressTests =
  T.testGroup
    "Address golden tests"
    [ goldenTests_MockCrypto,
      goldenTests_ShelleyCrypto
      -- roundTripTests,
    ]

{------------------------------------------------------------------------------
-- Golden tests
------------------------------------------------------------------------------}

goldenTests_MockCrypto :: TestTree
goldenTests_MockCrypto =
  T.testGroup
    "ConcreteCrypto golden tests"
    [ golden "keyHash" putCredential keyHash "01020304",
      golden "scriptHash" putCredential scriptHash "05060708",
      golden "ptr" putPtr ptr "81000203",
      golden
        "addrBaseKK"
        putAddr
        (Addr Testnet keyHash (StakeRefBase keyHash))
        "000102030401020304",
      golden
        "addrBaseSK"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase keyHash))
        "100506070801020304",
      golden
        "addrBaseKS"
        putAddr
        (Addr Testnet keyHash (StakeRefBase scriptHash))
        "200102030405060708",
      golden
        "addrBaseSS"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase scriptHash))
        "300506070805060708",
      golden
        "addrPtrK"
        putAddr
        (Addr Testnet keyHash (StakeRefPtr ptr))
        "400102030481000203",
      golden
        "addrPtrS"
        putAddr
        (Addr Testnet scriptHash (StakeRefPtr ptr))
        "500506070881000203",
      golden
        "addrEnterpriseK"
        putAddr
        (Addr Testnet keyHash StakeRefNull)
        "6001020304",
      golden
        "addrEnterpriseS"
        putAddr
        (Addr Testnet scriptHash StakeRefNull)
        "7005060708",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet keyHash)
        "e001020304",
      golden
        "rewardAcntS"
        putRewardAcnt
        (RewardAcnt Testnet scriptHash)
        "f005060708"
    ]
  where
    keyHash :: Credential kh (ConcreteCrypto ShortHash)
    keyHash =
      KeyHashObj . KeyHash . UnsafeHash $
        SBS.toShort . fst . B16.decode $ "01020304"
    scriptHash :: Credential kh (ConcreteCrypto ShortHash)
    scriptHash =
      ScriptHashObj . ScriptHash . UnsafeHash $
        SBS.toShort . fst . B16.decode $ "05060708"
    ptr :: Ptr
    ptr = Ptr (SlotNo 128) 2 3

-- Crypto family as used in production Shelley
-- This should match that defined at https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus-shelley/src/Ouroboros/Consensus/Shelley/Protocol/Crypto.hs
data ShelleyCrypto

instance Crypto ShelleyCrypto where
  type DSIGN ShelleyCrypto = Ed25519DSIGN
  type KES ShelleyCrypto = Sum7KES Ed25519DSIGN Blake2b_256
  type VRF ShelleyCrypto = SimpleVRF
  type HASH ShelleyCrypto = Blake2b_256
  type ADDRHASH ShelleyCrypto = Blake2b_224

goldenTests_ShelleyCrypto :: TestTree
goldenTests_ShelleyCrypto =
  T.testGroup
    "ShelleyCrypto golden tests"
    [ golden
        "addrEnterpriseK for network id = 0"
        putAddr
        (Addr Testnet (paymentKey) StakeRefNull)
        "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      golden
        "addrBaseKK for network id = 0"
        putAddr
        (Addr Testnet (paymentKey) (StakeRefBase (stakeKey)))
        "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "addrPtrK for network id = 0"
        putAddr
        (Addr Testnet (paymentKey) (StakeRefPtr ptr))
        "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      golden
        "addrEnterpriseK for network id = 1"
        putAddr
        (Addr Mainnet (paymentKey) StakeRefNull)
        "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      golden
        "addrBaseKK for network id = 1"
        putAddr
        (Addr Mainnet (paymentKey) (StakeRefBase (stakeKey)))
        "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "addrPtrK for network id = 1"
        putAddr
        (Addr Mainnet (paymentKey) (StakeRefPtr ptr))
        "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet stakeKey)
        "e008b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
    ]
  where
    paymentKey :: Credential 'Payment ShelleyCrypto
    paymentKey = keyBlake2b224 $ B16.encode "1a2a3a4a5a6a7a8a"
    stakeKey :: Credential 'Staking ShelleyCrypto
    stakeKey = keyBlake2b224 $ B16.encode "1c2c3c4c5c6c7c8c"
    ptr :: Ptr
    ptr = Ptr (SlotNo 128) 2 3
    -- 32-byte verification key is expected, vk, ie., public key without chain code.
    -- The verification key undergoes Blake2b_224 hashing
    -- and should be 28-byte in the aftermath
    keyBlake2b224 :: BS.ByteString -> Credential kh ShelleyCrypto
    keyBlake2b224 vk =
      KeyHashObj . KeyHash . fromJust . hashFromBytes $ hk
      where
        hash = digest (Proxy :: Proxy Blake2b_224)
        vk' = invariantSize 32 vk
        hk =
          invariantSize
            (fromIntegral $ sizeHash (Proxy :: Proxy Blake2b_224))
            (hash vk')
    invariantSize :: HasCallStack => Int -> BS.ByteString -> BS.ByteString
    invariantSize expectedLength bytes
      | BS.length bytes == expectedLength = bytes
      | otherwise =
        error $
          "length was "
            ++ show (BS.length bytes)
            ++ ", but expected to be "
            ++ show expectedLength

golden :: String -> (a -> B.Put) -> a -> LBS.ByteString -> TestTree
golden name put value expected =
  T.testCase name $
    T.assertEqual name expected (LB16.encode . B.runPut . put $ value)
