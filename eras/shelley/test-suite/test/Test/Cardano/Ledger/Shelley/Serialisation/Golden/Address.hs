{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address
  ( tests,
    Shelley,
  )
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.Hash (HashAlgorithm (..), hashFromBytes, hashFromTextAsHex, sizeHash)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys
  ( KeyRole (..),
    pattern KeyHash,
  )
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (pattern ScriptHash)
import Cardano.Ledger.Slot (SlotNo (..))
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

-- Crypto family as used in production Shelley
-- This should match that defined at https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus-shelley/src/Ouroboros/Consensus/Shelley/Protocol/Crypto.hs

type ShelleyCrypto = StandardCrypto

type Shelley = ShelleyEra StandardCrypto

tests :: TestTree
tests =
  T.testGroup
    "Address golden tests"
    [ goldenTests_MockCrypto,
      goldenTests_ShelleyCrypto
    ]

{------------------------------------------------------------------------------
-- Golden tests
------------------------------------------------------------------------------}

goldenTests_MockCrypto :: TestTree
goldenTests_MockCrypto =
  T.testGroup
    "MockCrypto golden tests"
    [ golden "keyHash" putCredential keyHash "01020304a1a2a3a4",
      golden "scriptHash" putCredential scriptHash "05060708b5b6b7b8",
      golden "ptr" putPtr ptr "81000203",
      golden
        "addrBaseKK"
        putAddr
        (Addr Testnet keyHash (StakeRefBase keyHash))
        "0001020304a1a2a3a401020304a1a2a3a4",
      golden
        "addrBaseSK"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase keyHash))
        "1005060708b5b6b7b801020304a1a2a3a4",
      golden
        "addrBaseKS"
        putAddr
        (Addr Testnet keyHash (StakeRefBase scriptHash))
        "2001020304a1a2a3a405060708b5b6b7b8",
      golden
        "addrBaseSS"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase scriptHash))
        "3005060708b5b6b7b805060708b5b6b7b8",
      golden
        "addrPtrK"
        putAddr
        (Addr Testnet keyHash (StakeRefPtr ptr))
        "4001020304a1a2a3a481000203",
      golden
        "addrPtrS"
        putAddr
        (Addr Testnet scriptHash (StakeRefPtr ptr))
        "5005060708b5b6b7b881000203",
      golden
        "addrEnterpriseK"
        putAddr
        (Addr Testnet keyHash StakeRefNull)
        "6001020304a1a2a3a4",
      golden
        "addrEnterpriseS"
        putAddr
        (Addr Testnet scriptHash StakeRefNull)
        "7005060708b5b6b7b8",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet keyHash)
        "e001020304a1a2a3a4",
      golden
        "rewardAcntS"
        putRewardAcnt
        (RewardAcnt Testnet scriptHash)
        "f005060708b5b6b7b8"
    ]
  where
    keyHash :: Credential kh C_Crypto
    keyHash =
      KeyHashObj . KeyHash
        . fromMaybe (error "Unable to decode")
        . hashFromTextAsHex
        $ "01020304a1a2a3a4"
    scriptHash :: Credential kh C_Crypto
    scriptHash =
      ScriptHashObj . ScriptHash
        . fromMaybe (error "Unable to decode")
        . hashFromTextAsHex
        $ "05060708b5b6b7b8"
    ptr :: Ptr
    ptr = Ptr (SlotNo 128) 2 3

goldenTests_ShelleyCrypto :: TestTree
goldenTests_ShelleyCrypto =
  T.testGroup
    "ShelleyCrypto golden tests"
    [ golden
        "addrEnterpriseK for network id = 0"
        putAddr
        (Addr Testnet paymentKey StakeRefNull)
        "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      golden
        "addrBaseKK for network id = 0"
        putAddr
        (Addr Testnet paymentKey (StakeRefBase stakeKey))
        "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "addrPtrK for network id = 0"
        putAddr
        (Addr Testnet paymentKey (StakeRefPtr ptr))
        "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      golden
        "addrEnterpriseK for network id = 1"
        putAddr
        (Addr Mainnet paymentKey StakeRefNull)
        "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      golden
        "addrBaseKK for network id = 1"
        putAddr
        (Addr Mainnet paymentKey (StakeRefBase stakeKey))
        "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "addrPtrK for network id = 1"
        putAddr
        (Addr Mainnet paymentKey (StakeRefPtr ptr))
        "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet stakeKey)
        "e008b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "bootstrapAddr for network id = 1"
        putAddr
        ( AddrBootstrap . BootstrapAddress $
            Byron.Address
              { Byron.addrRoot = read "4bf3c2ee56bfef278d65f7388c46efa12a1069698e474f77adf0cf6a",
                Byron.addrAttributes =
                  Byron.Attributes
                    { Byron.attrData =
                        Byron.AddrAttributes
                          { Byron.aaVKDerivationPath = Nothing,
                            Byron.aaNetworkMagic = Byron.NetworkMainOrStage
                          },
                      Byron.attrRemain = Byron.UnparsedFields mempty
                    },
                Byron.addrType = Byron.ATVerKey
              }
        )
        "82d818582183581c4bf3c2ee56bfef278d65f7388c46efa12a1069698e474f77adf0cf6aa0001ab4aad9a5",
      T.testCase "fail on extraneous bytes" $
        case deserialiseAddr @ShelleyCrypto addressWithExtraneousBytes of
          Nothing -> pure ()
          Just _a -> error $ "This should have failed"
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

addressWithExtraneousBytes :: BS.ByteString
addressWithExtraneousBytes = bs
  where
    bs = case B16.decode hs of
      Left e -> error $ show e
      Right x -> x
    hs =
      "01AA5C8B35A934ED83436ABB56CDB44878DAC627529D2DA0B59CDA794405931B9359\
      \46E9391CABDFFDED07EB727F94E9E0F23739FF85978905BD460158907C589B9F1A62"

golden :: HasCallStack => String -> (a -> B.Put) -> a -> LBS.ByteString -> TestTree
golden name put value expected =
  T.testCase name $
    T.assertEqual name expected (LB16.encode . B.runPut . put $ value)
