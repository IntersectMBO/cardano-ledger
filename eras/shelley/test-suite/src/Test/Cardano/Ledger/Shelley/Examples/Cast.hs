{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Cardano.Crypto.KES (UnsoundPureKESAlgorithm)
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
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (
  Hash,
  KeyRole (..),
  VerKeyVRF,
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
alicePay :: Crypto c => KeyPair 'Payment c
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

-- | Alice's stake key pair
aliceStake :: Crypto c => KeyPair 'Staking c
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

-- | Alice's stake pool keys (cold keys, VRF keys, hot KES keys)
alicePoolKeys :: (Crypto c, UnsoundPureKESAlgorithm (KES c)) => AllIssuerKeys c 'StakePool
alicePoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed 1 0 0 0 2))
    ((KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3)) NE.:| [])
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 1 0 0 0 1)

-- | Alice's base address
aliceAddr :: Crypto c => Addr c
aliceAddr = mkAddr (alicePay, aliceStake)

-- | Alice's payment credential
alicePHK :: Crypto c => Credential 'Payment c
alicePHK = (KeyHashObj . hashKey . vKey) alicePay

-- | Alice's stake credential
aliceSHK :: Crypto c => Credential 'Staking c
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

-- | Alice's base address
alicePtrAddr :: Crypto c => Addr c
alicePtrAddr = Addr Testnet alicePHK (StakeRefPtr $ Ptr (SlotNo 10) minBound minBound)

-- | Alice's stake pool parameters
alicePoolParams :: forall c. (Crypto c, UnsoundPureKESAlgorithm (KES c)) => PoolParams c
alicePoolParams =
  PoolParams
    { ppId = hashKey . vKey $ aikCold alicePoolKeys
    , ppVrf = hashVerKeyVRF . vrfVerKey $ aikVrf (alicePoolKeys @c)
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
aliceVRFKeyHash ::
  forall c.
  (Crypto c, UnsoundPureKESAlgorithm (KES c)) =>
  Hash c (VerKeyVRF c)
aliceVRFKeyHash = hashVerKeyVRF (vrfVerKey $ aikVrf (alicePoolKeys @c))

-- | Bob's payment key pair
bobPay :: Crypto c => KeyPair 'Payment c
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 2 2 2 2 2)

-- | Bob's stake key pair
bobStake :: Crypto c => KeyPair 'Staking c
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 3 3 3 3 3)

-- | Bob's address
bobAddr :: Crypto c => Addr c
bobAddr = mkAddr (bobPay, bobStake)

-- | Bob's stake credential
bobSHK :: Crypto c => Credential 'Staking c
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

-- | Bob's stake pool keys (cold keys, VRF keys, hot KES keys)
bobPoolKeys :: (Crypto c, UnsoundPureKESAlgorithm (KES c)) => AllIssuerKeys c 'StakePool
bobPoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed 2 0 0 0 2))
    ((KESPeriod 0, mkKESKeyPair (RawSeed 2 0 0 0 3)) NE.:| [])
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 2 0 0 0 1)

-- | Bob's stake pool parameters
bobPoolParams :: forall c. (Crypto c, UnsoundPureKESAlgorithm (KES c)) => PoolParams c
bobPoolParams =
  PoolParams
    { ppId = hashKey . vKey $ aikCold bobPoolKeys
    , ppVrf = hashVerKeyVRF . vrfVerKey $ aikVrf (bobPoolKeys @c)
    , ppPledge = Coin 2
    , ppCost = Coin 1
    , ppMargin = unsafeBoundRational 0.1
    , ppRewardAccount = RewardAccount Testnet bobSHK
    , ppOwners = Set.singleton $ hashKey (vKey bobStake)
    , ppRelays = StrictSeq.empty
    , ppMetadata = SNothing
    }

-- | Bob's VRF key hash
bobVRFKeyHash ::
  forall c.
  (Crypto c, UnsoundPureKESAlgorithm (KES c)) =>
  Hash c (VerKeyVRF c)
bobVRFKeyHash = hashVerKeyVRF (vrfVerKey $ aikVrf (bobPoolKeys @c))

-- Carl's payment key pair
carlPay :: Crypto c => KeyPair 'Payment c
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 4 4 4 4 4)

-- | Carl's stake key pair
carlStake :: Crypto c => KeyPair 'Staking c
carlStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 5 5 5 5 5)

-- | Carl's address
carlAddr :: Crypto c => Addr c
carlAddr = mkAddr (carlPay, carlStake)

-- | Carl's stake credential
carlSHK :: Crypto c => Credential 'Staking c
carlSHK = (KeyHashObj . hashKey . vKey) carlStake

-- | Daria's payment key pair
dariaPay :: Crypto c => KeyPair 'Payment c
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 6 6 6 6 6)

-- | Daria's stake key pair
dariaStake :: Crypto c => KeyPair 'Staking c
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 7 7 7 7 7)

-- | Daria's address
dariaAddr :: Crypto c => Addr c
dariaAddr = mkAddr (dariaPay, dariaStake)

-- | Daria's stake credential
dariaSHK :: Crypto c => Credential 'Staking c
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake
