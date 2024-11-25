{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Cast
-- Description : Cast of characters for Shelley ledger examples
--
-- The cast of Characters for Shelley Ledger Examples
-- (excluding the genesis/cord nodes,
-- which are in Test.Cardano.Ledger.Shelley.Examples.Federation).
module Test.Cardano.Ledger.Shelley.Examples.Cast (
  alicePay,
  aliceStake,
  alicePHK,
  aliceSHK,
  aliceAddr,
  alicePtrAddr,
  alicePoolKeys,
  alicePoolParams,
  aliceVRFKeyHash,
  bobPay,
  bobStake,
  bobSHK,
  bobAddr,
  bobPoolKeys,
  bobPoolParams,
  bobVRFKeyHash,
  carlPay,
  carlStake,
  carlSHK,
  carlAddr,
  dariaPay,
  dariaStake,
  dariaSHK,
  dariaAddr,
)
where

import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
  textToUrl,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (
  Credential (..),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  KeyRole (..),
  KeyRoleVRF (StakePoolVRF),
  VRFVerKeyHash,
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Ledger.PoolParams (
  PoolMetadata (..),
  PoolParams (..),
 )
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  VRFKeyPair (..),
 )
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKESKeyPair,
  mkKeyPair,
  mkVRFKeyPair,
  unsafeBoundRational,
 )

-- | Alice's payment key pair
alicePay :: KeyPair 'Payment
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

-- | Alice's stake key pair
aliceStake :: KeyPair 'Staking
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

-- | Alice's stake pool keys (cold keys, VRF keys, hot KES keys)
alicePoolKeys :: AllIssuerKeys StandardCrypto 'StakePool
alicePoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed 1 0 0 0 2))
    ((KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3)) NE.:| [])
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 1 0 0 0 1)

-- | Alice's base address
aliceAddr :: Addr
aliceAddr = mkAddr (alicePay, aliceStake)

-- | Alice's payment credential
alicePHK :: Credential 'Payment
alicePHK = (KeyHashObj . hashKey . vKey) alicePay

-- | Alice's stake credential
aliceSHK :: Credential 'Staking
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

-- | Alice's base address
alicePtrAddr :: Addr
alicePtrAddr = Addr Testnet alicePHK (StakeRefPtr $ Ptr (SlotNo 10) minBound minBound)

-- | Alice's stake pool parameters
alicePoolParams :: PoolParams
alicePoolParams =
  PoolParams
    { ppId = hashKey . vKey $ aikCold alicePoolKeys
    , ppVrf = hashVerKeyVRF @StandardCrypto . vrfVerKey $ aikVrf (alicePoolKeys)
    , ppPledge = Coin 1
    , ppCost = Coin 5
    , ppMargin = unsafeBoundRational 0.1
    , ppRewardAccount = RewardAccount Testnet aliceSHK
    , ppOwners = Set.singleton $ (hashKey . vKey) aliceStake
    , ppRelays = StrictSeq.empty
    , ppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl 64 "alice.pool"
            , pmHash = BS.pack "{}"
            }
    }

-- | Alice's VRF key hash
aliceVRFKeyHash :: VRFVerKeyHash 'StakePoolVRF
aliceVRFKeyHash = hashVerKeyVRF @StandardCrypto (vrfVerKey $ aikVrf (alicePoolKeys))

-- | Bob's payment key pair
bobPay :: KeyPair 'Payment
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 2 2 2 2 2)

-- | Bob's stake key pair
bobStake :: KeyPair 'Staking
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 3 3 3 3 3)

-- | Bob's address
bobAddr :: Addr
bobAddr = mkAddr (bobPay, bobStake)

-- | Bob's stake credential
bobSHK :: Credential 'Staking
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

-- | Bob's stake pool keys (cold keys, VRF keys, hot KES keys)
bobPoolKeys :: AllIssuerKeys StandardCrypto 'StakePool
bobPoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed 2 0 0 0 2))
    ((KESPeriod 0, mkKESKeyPair (RawSeed 2 0 0 0 3)) NE.:| [])
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 2 0 0 0 1)

-- | Bob's stake pool parameters
bobPoolParams :: PoolParams
bobPoolParams =
  PoolParams
    { ppId = hashKey . vKey $ aikCold bobPoolKeys
    , ppVrf = hashVerKeyVRF @StandardCrypto . vrfVerKey $ aikVrf (bobPoolKeys)
    , ppPledge = Coin 2
    , ppCost = Coin 1
    , ppMargin = unsafeBoundRational 0.1
    , ppRewardAccount = RewardAccount Testnet bobSHK
    , ppOwners = Set.singleton $ hashKey (vKey bobStake)
    , ppRelays = StrictSeq.empty
    , ppMetadata = SNothing
    }

-- | Bob's VRF key hash
bobVRFKeyHash :: VRFVerKeyHash 'StakePoolVRF
bobVRFKeyHash = hashVerKeyVRF @StandardCrypto (vrfVerKey $ aikVrf (bobPoolKeys))

-- Carl's payment key pair
carlPay :: KeyPair 'Payment
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 4 4 4 4 4)

-- | Carl's stake key pair
carlStake :: KeyPair 'Staking
carlStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 5 5 5 5 5)

-- | Carl's address
carlAddr :: Addr
carlAddr = mkAddr (carlPay, carlStake)

-- | Carl's stake credential
carlSHK :: Credential 'Staking
carlSHK = (KeyHashObj . hashKey . vKey) carlStake

-- | Daria's payment key pair
dariaPay :: KeyPair 'Payment
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 6 6 6 6 6)

-- | Daria's stake key pair
dariaStake :: KeyPair 'Staking
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 7 7 7 7 7)

-- | Daria's address
dariaAddr :: Addr
dariaAddr = mkAddr (dariaPay, dariaStake)

-- | Daria's stake credential
dariaSHK :: Credential 'Staking
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake
