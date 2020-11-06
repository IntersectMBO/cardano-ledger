{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Cast
-- Description : Cast of characters for Shelley ledger examples
--
-- The cast of Characters for Shelley Ledger Examples
-- (excluding the genesis/cord nodes,
-- which are in Test.Shelley.Spec.Ledger.Examples.Federation).
module Test.Shelley.Spec.Ledger.Examples.Cast
  ( alicePay,
    aliceStake,
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

import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    textToUrl,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyPair (..),
    KeyRole (..),
    VerKeyVRF,
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.TxBody
  ( PoolMetaData (..),
    PoolParams (..),
    RewardAcnt (..),
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
  )
import Test.Shelley.Spec.Ledger.Utils
  ( mkAddr,
    mkKESKeyPair,
    mkKeyPair,
    mkVRFKeyPair,
    unsafeMkUnitInterval,
  )

-- | Alice's payment key pair
alicePay :: CC.Crypto crypto => KeyPair 'Payment crypto
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

-- | Alice's stake key pair
aliceStake :: CC.Crypto crypto => KeyPair 'Staking crypto
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

-- | Alice's stake pool keys (cold keys, VRF keys, hot KES keys)
alicePoolKeys :: CC.Crypto crypto => AllIssuerKeys crypto 'StakePool
alicePoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (1, 0, 0, 0, 2))
    [(KESPeriod 0, mkKESKeyPair (1, 0, 0, 0, 3))]
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (1, 0, 0, 0, 1)

-- | Alice's base address
aliceAddr :: Era era => Addr era
aliceAddr = mkAddr (alicePay, aliceStake)

alicePHK :: Era era => Credential 'Payment era
alicePHK = (KeyHashObj . hashKey . vKey) alicePay

-- | Alice's stake credential
aliceSHK :: Era era => Credential 'Staking era
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

-- | Alice's base address
alicePtrAddr :: Era era => Addr era
alicePtrAddr = Addr Testnet alicePHK (StakeRefPtr $ Ptr (SlotNo 10) 0 0)

-- | Alice's stake pool parameters
alicePoolParams :: forall era. Era era => PoolParams era
alicePoolParams =
  PoolParams
    { _poolId = (hashKey . vKey . cold) alicePoolKeys,
      _poolVrf = hashVerKeyVRF . snd $ vrf (alicePoolKeys @(Crypto era)),
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet aliceSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays = StrictSeq.empty,
      _poolMD =
        SJust $
          PoolMetaData
            { _poolMDUrl = fromJust $ textToUrl "alice.pool",
              _poolMDHash = BS.pack "{}"
            }
    }

-- | Alice's VRF key hash
aliceVRFKeyHash ::
  forall crypto.
  CC.Crypto crypto =>
  Hash crypto (VerKeyVRF crypto)
aliceVRFKeyHash = hashVerKeyVRF (snd $ vrf (alicePoolKeys @crypto))

-- | Bob's payment key pair
bobPay :: CC.Crypto crypto => KeyPair 'Payment crypto
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

-- | Bob's stake key pair
bobStake :: CC.Crypto crypto => KeyPair 'Staking crypto
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

-- | Bob's address
bobAddr :: Era era => Addr era
bobAddr = mkAddr (bobPay, bobStake)

-- | Bob's stake credential
bobSHK :: Era era => Credential 'Staking era
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

-- | Bob's stake pool keys (cold keys, VRF keys, hot KES keys)
bobPoolKeys :: CC.Crypto crypto => AllIssuerKeys crypto 'StakePool
bobPoolKeys =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (2, 0, 0, 0, 2))
    [(KESPeriod 0, mkKESKeyPair (2, 0, 0, 0, 3))]
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (2, 0, 0, 0, 1)

-- | Bob's stake pool parameters
bobPoolParams :: forall era. Era era => PoolParams era
bobPoolParams =
  PoolParams
    { _poolId = (hashKey . vKey . cold) bobPoolKeys,
      _poolVrf = hashVerKeyVRF . snd $ vrf (bobPoolKeys @(Crypto era)),
      _poolPledge = Coin 2,
      _poolCost = Coin 1,
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet bobSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) bobStake,
      _poolRelays = StrictSeq.empty,
      _poolMD = SNothing
    }

-- | Bob's VRF key hash
bobVRFKeyHash ::
  forall crypto.
  CC.Crypto crypto =>
  Hash crypto (VerKeyVRF crypto)
bobVRFKeyHash = hashVerKeyVRF (snd $ vrf (bobPoolKeys @crypto))

-- Carl's payment key pair
carlPay :: CC.Crypto crypto => KeyPair 'Payment crypto
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

-- | Carl's stake key pair
carlStake :: CC.Crypto crypto => KeyPair 'Staking crypto
carlStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

-- | Carl's address
carlAddr :: Era era => Addr era
carlAddr = mkAddr (carlPay, carlStake)

-- | Carl's stake credential
carlSHK :: Era era => Credential 'Staking era
carlSHK = (KeyHashObj . hashKey . vKey) carlStake

-- | Daria's payment key pair
dariaPay :: CC.Crypto crypto => KeyPair 'Payment crypto
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

-- | Daria's stake key pair
dariaStake :: CC.Crypto crypto => KeyPair 'Staking crypto
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

-- | Daria's address
dariaAddr :: Era era => Addr era
dariaAddr = mkAddr (dariaPay, dariaStake)

-- | Daria's stake credential
dariaSHK :: Era era => Credential 'Staking era
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake
