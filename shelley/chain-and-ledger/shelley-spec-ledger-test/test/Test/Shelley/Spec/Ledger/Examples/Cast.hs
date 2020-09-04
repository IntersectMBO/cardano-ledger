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
alicePay :: Era era => KeyPair 'Payment era
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

-- | Alice's stake key pair
aliceStake :: Era era => KeyPair 'Staking era
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

-- | Alice's stake pool keys (cold keys, VRF keys, hot KES keys)
alicePoolKeys :: Era era => AllIssuerKeys era 'StakePool
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
    { _poolPubKey = (hashKey . vKey . cold) alicePoolKeys,
      _poolVrf = hashVerKeyVRF . snd $ vrf (alicePoolKeys @era),
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
aliceVRFKeyHash :: forall era. Era era => Hash era (VerKeyVRF era)
aliceVRFKeyHash = hashVerKeyVRF (snd $ vrf (alicePoolKeys @era))

-- | Bob's payment key pair
bobPay :: Era era => KeyPair 'Payment era
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

-- | Bob's stake key pair
bobStake :: Era era => KeyPair 'Staking era
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

-- | Bob's address
bobAddr :: Era era => Addr era
bobAddr = mkAddr (bobPay, bobStake)

-- | Bob's stake credential
bobSHK :: Era era => Credential 'Staking era
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

-- Carl's payment key pair
carlPay :: Era era => KeyPair 'Payment era
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

-- | Carl's stake key pair
carlStake :: Era era => KeyPair 'Staking era
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
dariaPay :: Era era => KeyPair 'Payment era
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

-- | Daria's stake key pair
dariaStake :: Era era => KeyPair 'Staking era
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

-- | Daria's address
dariaAddr :: Era era => Addr era
dariaAddr = mkAddr (dariaPay, dariaStake)

-- | Daria's stake credential
dariaSHK :: Era era => Credential 'Staking era
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake
