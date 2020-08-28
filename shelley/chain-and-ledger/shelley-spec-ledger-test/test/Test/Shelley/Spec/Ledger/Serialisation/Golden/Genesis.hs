{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Serialisation.Golden.Genesis
  ( tests,

    -- * Individual properties
    prop_golden_ShelleyGenesis,
  )
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Slotting.Slot (EpochSize (..))
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hedgehog (Property)
import qualified Shelley.Spec.Ledger.API as L
import Shelley.Spec.Ledger.BaseTypes (textToDns, textToUrl, truncateUnitInterval)
import Cardano.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.Keys (hashKey, hashVerKeyVRF, vKey)
import Shelley.Spec.Ledger.PParams (PParams' (..), emptyPParams)
import Test.Cardano.Prelude
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C, Mock)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Utils
  ( mkKeyPair,
    mkVRFKeyPair,
    unsafeMkUnitInterval,
  )
import Test.Tasty
import Test.Tasty.Hedgehog

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSONPretty example "test/Golden/ShelleyGenesis"
  where
    example :: ShelleyGenesis C
    example = exampleShelleyGenesis

tests :: TestTree
tests =
  testGroup
    "Shelley Genesis golden tests"
    [ testProperty "ShelleyGenesis golden test" prop_golden_ShelleyGenesis
    ]

exampleShelleyGenesis :: forall c. Mock c => ShelleyGenesis c
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
      sgNetworkMagic = 4036000900,
      sgNetworkId = L.Testnet,
      sgActiveSlotsCoeff = 6.259,
      sgSecurityParam = 120842,
      sgEpochLength = EpochSize 1215,
      sgSlotsPerKESPeriod = 8541,
      sgMaxKESEvolutions = 28899,
      sgSlotLength = 8,
      sgUpdateQuorum = 16991,
      sgMaxLovelaceSupply = 71,
      sgProtocolParams =
        emptyPParams
          { _d = truncateUnitInterval . realToFrac $ (1.9e-2 :: Scientific),
            _maxBBSize = 239857,
            _maxBHSize = 217569
          },
      sgGenDelegs = Map.fromList [(genesisVerKeyHash, genDelegPair)],
      sgInitialFunds = Map.fromList [(initialFundedAddress, initialFunds)],
      sgStaking = staking
    }
  where
    -- hash of the genesis verification key
    genesisVerKeyHash :: L.KeyHash 'L.Genesis c
    genesisVerKeyHash = L.KeyHash "23d51e9123d51e91"
    -- hash of the delegators verififation key
    genDelegPair = L.GenDelegPair delegVerKeyHash delegVrfKeyHash
    delegVerKeyHash :: L.KeyHash 'L.GenesisDelegate c
    delegVerKeyHash = L.KeyHash "839b047f839b047f"
    delegVrfKeyHash :: Hash.Hash (HASH c) (L.VerKeyVRF c)
    delegVrfKeyHash = "231391e7231391e70123"
    initialFundedAddress :: L.Addr c
    initialFundedAddress = L.Addr L.Testnet paymentCredential (L.StakeRefBase stakingCredential)
      where
        paymentCredential =
          L.KeyHashObj $
            L.KeyHash
              "1c14ee8e1c14ee8e"
        stakingCredential =
          L.KeyHashObj $
            L.KeyHash
              "e37a65eae37a65ea"
    initialFunds :: L.Coin
    initialFunds = L.Coin 12157196
    relays =
      StrictSeq.fromList
        [ L.SingleHostAddr
            (L.SJust $ L.Port 1234)
            (L.SJust $ read "0.0.0.0")
            (L.SJust $ read "2001:db8:a::123"),
          L.SingleHostName L.SNothing (fromJust $ textToDns "cool.domain.com"),
          L.MultiHostName (fromJust $ textToDns "cool.domain.com")
        ]
    poolParams :: L.PoolParams c
    poolParams =
      L.PoolParams
        { L._poolPubKey = hashKey . snd $ mkKeyPair (1, 0, 0, 0, 1),
          L._poolVrf = hashVerKeyVRF . snd $ mkVRFKeyPair (1, 0, 0, 0, 2),
          L._poolPledge = L.Coin 1,
          L._poolCost = L.Coin 5,
          L._poolMargin = unsafeMkUnitInterval 0.25,
          L._poolRAcnt = L.RewardAcnt L.Testnet Cast.aliceSHK,
          L._poolOwners = Set.singleton $ (hashKey . vKey) Cast.aliceStake,
          L._poolRelays = relays,
          L._poolMD =
            L.SJust $
              L.PoolMetaData
                { L._poolMDUrl = fromJust $ textToUrl "best.pool.com",
                  L._poolMDHash = BS.pack "100ab{}100ab{}"
                }
        }
    staking =
      ShelleyGenesisStaking
        { sgsPools = Map.fromList [(L.KeyHash "3dbe00a13dbe00a1", poolParams)],
          sgsStake = Map.fromList [(L.KeyHash "1c14ee8e1c14ee8e", L.KeyHash "1c14ee8e1c14ee8e")]
        }
