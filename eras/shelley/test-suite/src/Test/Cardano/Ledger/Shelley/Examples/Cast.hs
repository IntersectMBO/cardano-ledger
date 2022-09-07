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
module Test.Cardano.Ledger.Shelley.Examples.Cast
  ( alicePay,
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

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    textToUrl,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( Hash,
    KeyPair (..),
    KeyRole (..),
    VerKeyVRF,
    hashKey,
    hashVerKeyVRF,
  )
import Cardano.Ledger.Shelley.TxBody
  ( PoolMetadata (..),
    PoolParams (..),
    RewardAcnt (..),
  )
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( AllIssuerKeys (..),
  )
import Test.Cardano.Ledger.Shelley.Utils
  ( RawSeed (..),
    mkAddr,
    mkKESKeyPair,
    mkKeyPair,
    mkVRFKeyPair,
    unsafeBoundRational,
  )

-- | Alice's payment key pair
alicePay :: CC.Crypto c => KeyPair 'Payment c
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

-- | Alice's stake key pair
aliceStake :: CC.Crypto c => KeyPair 'Staking c
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

-- | Alice's stake pool keys (cold keys, VRF keys, hot KES keys)
alicePoolKeys :: CC.Crypto c => AllIssuerKeys c 'StakePool
alicePoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed 1 0 0 0 2))
    [(KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3))]
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 1 0 0 0 1)

-- | Alice's base address
aliceAddr :: CC.Crypto c => Addr c
aliceAddr = mkAddr (alicePay, aliceStake)

-- | Alice's payment credential
alicePHK :: CC.Crypto c => Credential 'Payment c
alicePHK = (KeyHashObj . hashKey . vKey) alicePay

-- | Alice's stake credential
aliceSHK :: CC.Crypto c => Credential 'Staking c
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

-- | Alice's base address
alicePtrAddr :: CC.Crypto c => Addr c
alicePtrAddr = Addr Testnet alicePHK (StakeRefPtr $ Ptr (SlotNo 10) minBound minBound)

-- | Alice's stake pool parameters
alicePoolParams :: forall c. CC.Crypto c => PoolParams c
alicePoolParams =
  PoolParams
    { _poolId = (hashKey . vKey . cold) alicePoolKeys,
      _poolVrf = hashVerKeyVRF . snd $ vrf (alicePoolKeys @c),
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeBoundRational 0.1,
      _poolRAcnt = RewardAcnt Testnet aliceSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays = StrictSeq.empty,
      _poolMD =
        SJust $
          PoolMetadata
            { _poolMDUrl = fromJust $ textToUrl "alice.pool",
              _poolMDHash = BS.pack "{}"
            }
    }

-- | Alice's VRF key hash
aliceVRFKeyHash ::
  forall c.
  CC.Crypto c =>
  Hash c (VerKeyVRF c)
aliceVRFKeyHash = hashVerKeyVRF (snd $ vrf (alicePoolKeys @c))

-- | Bob's payment key pair
bobPay :: CC.Crypto c => KeyPair 'Payment c
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 2 2 2 2 2)

-- | Bob's stake key pair
bobStake :: CC.Crypto c => KeyPair 'Staking c
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 3 3 3 3 3)

-- | Bob's address
bobAddr :: CC.Crypto c => Addr c
bobAddr = mkAddr (bobPay, bobStake)

-- | Bob's stake credential
bobSHK :: CC.Crypto c => Credential 'Staking c
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

-- | Bob's stake pool keys (cold keys, VRF keys, hot KES keys)
bobPoolKeys :: CC.Crypto c => AllIssuerKeys c 'StakePool
bobPoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed 2 0 0 0 2))
    [(KESPeriod 0, mkKESKeyPair (RawSeed 2 0 0 0 3))]
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 2 0 0 0 1)

-- | Bob's stake pool parameters
bobPoolParams :: forall c. CC.Crypto c => PoolParams c
bobPoolParams =
  PoolParams
    { _poolId = (hashKey . vKey . cold) bobPoolKeys,
      _poolVrf = hashVerKeyVRF . snd $ vrf (bobPoolKeys @c),
      _poolPledge = Coin 2,
      _poolCost = Coin 1,
      _poolMargin = unsafeBoundRational 0.1,
      _poolRAcnt = RewardAcnt Testnet bobSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) bobStake,
      _poolRelays = StrictSeq.empty,
      _poolMD = SNothing
    }

-- | Bob's VRF key hash
bobVRFKeyHash ::
  forall c.
  CC.Crypto c =>
  Hash c (VerKeyVRF c)
bobVRFKeyHash = hashVerKeyVRF (snd $ vrf (bobPoolKeys @c))

-- Carl's payment key pair
carlPay :: CC.Crypto c => KeyPair 'Payment c
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 4 4 4 4 4)

-- | Carl's stake key pair
carlStake :: CC.Crypto c => KeyPair 'Staking c
carlStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 5 5 5 5 5)

-- | Carl's address
carlAddr :: CC.Crypto c => Addr c
carlAddr = mkAddr (carlPay, carlStake)

-- | Carl's stake credential
carlSHK :: CC.Crypto c => Credential 'Staking c
carlSHK = (KeyHashObj . hashKey . vKey) carlStake

-- | Daria's payment key pair
dariaPay :: CC.Crypto c => KeyPair 'Payment c
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 6 6 6 6 6)

-- | Daria's stake key pair
dariaStake :: CC.Crypto c => KeyPair 'Staking c
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 7 7 7 7 7)

-- | Daria's address
dariaAddr :: CC.Crypto c => Addr c
dariaAddr = mkAddr (dariaPay, dariaStake)

-- | Daria's stake credential
dariaSHK :: CC.Crypto c => Credential 'Staking c
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake
