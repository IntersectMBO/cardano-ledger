{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Serialisation.Golden.Genesis
  ( tests,

    -- * Individual properties
    prop_golden_json_ShelleyGenesis,
    prop_golden_cbor_ShelleyGenesis,
  )
where

import Cardano.Binary (Encoding (..), ToCBOR (..), Tokens (..), serializeEncoding)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (textToDns, textToUrl)
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Keys (hashKey, hashVerKeyVRF, vKey)
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
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.PParams (PParams' (..), emptyPParams)
import Test.Cardano.Prelude
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Utils
  ( RawSeed (..),
    mkKeyPair,
    mkVRFKeyPair,
    unsafeMkUnitInterval,
  )
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.Hedgehog

prop_golden_json_ShelleyGenesis :: Property
prop_golden_json_ShelleyGenesis = goldenTestJSONPretty example "test/Golden/ShelleyGenesis"
  where
    example :: ShelleyGenesis C
    example = exampleShelleyGenesis

prop_golden_cbor_ShelleyGenesis :: Assertion
prop_golden_cbor_ShelleyGenesis =
  if serializeEncoding received /= serializeEncoding expected
    then
      assertFailure $
        mconcat
          [ "\nexpected:\n",
            show expected,
            "\nexpected:\n",
            show received,
            "\n"
          ]
    else return ()
  where
    example :: ShelleyGenesis C
    example = exampleShelleyGenesis

    received = Encoding expectedTokens
    expected = toCBOR example

    expectedTokens =
      TkListLen 15
        . TkListLen 3
        . TkInt 2009
        . TkInt 44
        . TkInt 83589000000000000 -- sgSystemStart
        . TkInt 4036000900 -- sgNetworkMagic
        . TkInt 0 -- sgNetworkId
        . TkListLen 2
        . TkInt 6259
        . TkInt 1000 -- sgActiveSlotsCoeff
        . TkInt 120842 -- sgSecurityParam
        . TkInt 1215 -- sgEpochLength
        . TkInt 8541 -- sgSlotsPerKESPeriod
        . TkInt 28899 -- sgMaxKESEvolutions
        . TkInt 8000000 -- sgSlotLength
        . TkInt 16991 -- sgUpdateQuorum
        . TkInt 71 -- sgMaxLovelaceSupply
        . TkListLen 18 -- sgProtocolParams
        . TkInt 0
        . TkInt 0
        . TkInt 239857
        . TkInt 2048
        . TkInt 217569
        . TkInt 0
        . TkInt 0
        . TkInt 0
        . TkInt 100
        . TkTag 30
        . TkListLen 2
        . TkInt 0
        . TkInt 1
        . TkTag 30
        . TkListLen 2
        . TkInt 0
        . TkInt 1
        . TkTag 30
        . TkListLen 2
        . TkInt 0
        . TkInt 1
        . TkTag 30
        . TkListLen 2
        . TkInt 19
        . TkInt 1000
        . TkListLen 1
        . TkInt 0
        . TkInt 0
        . TkInt 0
        . TkInt 0
        . TkInt 0
        . TkMapLen 1 -- sgGenDelegs
        . TkBytes "#\213\RS\145#\213\RS\145"
        . TkListLen 2
        . TkBytes "\131\155\EOT\DEL\131\155\EOT\DEL"
        . TkBytes "#\DC3\145\231#\DC3\145\231\SOH#"
        . TkMapLen 1 -- sgInitialFunds
        . TkBytes "\NUL\FS\DC4\238\142\FS\DC4\238\142\227ze\234\227ze\234"
        . TkInt 12157196
        . TkListLen 2 -- sgStaking
        . TkMapLen 1 -- sgsPools
        . TkBytes "=\190\NUL\161=\190\NUL\161"
        . TkListLen 9 -- PoolParams
        . TkBytes "\160\132\186\143l\131\193\165"
        . TkBytes "\237\201\a\154O7\FS\172\&1\SI"
        . TkInt 1
        . TkInt 5
        . TkTag 30
        . TkListLen 2
        . TkInt 1
        . TkInt 4
        . TkBytes "\224\248h\161\150\n?\160C"
        . TkListLen 1
        . TkBytes "\248h\161\150\n?\160C"
        . TkListLen 3
        . TkListLen 4
        . TkInt 0
        . TkInt 1234
        . TkBytes "\NUL\NUL\NUL\NUL"
        . TkBytes "\184\r\SOH \NUL\NUL\n\NUL\NUL\NUL\NUL\NUL#\SOH\NUL\NUL"
        . TkListLen 3
        . TkInt 1
        . TkNull
        . TkString "cool.domain.com"
        . TkListLen 2
        . TkInt 2
        . TkString "cool.domain.com"
        . TkListLen 2
        . TkString "best.pool.com"
        . TkBytes "100ab{}100ab{}"
        . TkMapLen 1 -- sgsStake
        . TkBytes "\FS\DC4\238\142\FS\DC4\238\142"
        . TkBytes "\FS\DC4\238\142\FS\DC4\238\142"

-- TODO - return a CBOR diff in the case of failure

tests :: TestTree
tests =
  testGroup
    "Shelley Genesis golden tests"
    [ testProperty "ShelleyGenesis JSON golden test" prop_golden_json_ShelleyGenesis,
      testCase "ShelleyGenesis CBOR golden test" prop_golden_cbor_ShelleyGenesis
    ]

exampleShelleyGenesis ::
  forall era.
  Era era =>
  ShelleyGenesis era
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
      sgNetworkMagic = 4036000900,
      sgNetworkId = L.Testnet,
      sgActiveSlotsCoeff = unsafeMkUnitInterval 0.259,
      sgSecurityParam = 120842,
      sgEpochLength = EpochSize 1215,
      sgSlotsPerKESPeriod = 8541,
      sgMaxKESEvolutions = 28899,
      sgSlotLength = 8,
      sgUpdateQuorum = 16991,
      sgMaxLovelaceSupply = 71,
      sgProtocolParams =
        emptyPParams
          { _d = unsafeMkUnitInterval . realToFrac $ (1.9e-2 :: Scientific),
            _maxBBSize = 239857,
            _maxBHSize = 217569
          },
      sgGenDelegs = Map.fromList [(genesisVerKeyHash, genDelegPair)],
      sgInitialFunds = Map.fromList [(initialFundedAddress, initialFunds)],
      sgStaking = staking
    }
  where
    -- hash of the genesis verification key
    genesisVerKeyHash :: L.KeyHash 'L.Genesis (Crypto era)
    genesisVerKeyHash = L.KeyHash "23d51e9123d51e91"
    -- hash of the delegators verififation key
    genDelegPair = L.GenDelegPair delegVerKeyHash delegVrfKeyHash
    delegVerKeyHash :: L.KeyHash 'L.GenesisDelegate (Crypto era)
    delegVerKeyHash = L.KeyHash "839b047f839b047f"
    delegVrfKeyHash :: Hash.Hash (HASH (Crypto era)) (L.VerKeyVRF (Crypto era))
    delegVrfKeyHash = "231391e7231391e70123"
    initialFundedAddress :: L.Addr (Crypto era)
    initialFundedAddress =
      L.Addr
        L.Testnet
        paymentCredential
        (L.StakeRefBase stakingCredential)
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
    poolParams :: L.PoolParams (Crypto era)
    poolParams =
      L.PoolParams
        { L._poolId = hashKey . snd $ mkKeyPair (RawSeed 1 0 0 0 1),
          L._poolVrf = hashVerKeyVRF . snd $ mkVRFKeyPair (RawSeed 1 0 0 0 2),
          L._poolPledge = L.Coin 1,
          L._poolCost = L.Coin 5,
          L._poolMargin = unsafeMkUnitInterval 0.25,
          L._poolRAcnt = L.RewardAcnt L.Testnet Cast.aliceSHK,
          L._poolOwners = Set.singleton $ (hashKey . vKey) Cast.aliceStake,
          L._poolRelays = relays,
          L._poolMD =
            L.SJust $
              L.PoolMetadata
                { L._poolMDUrl = fromJust $ textToUrl "best.pool.com",
                  L._poolMDHash = BS.pack "100ab{}100ab{}"
                }
        }
    staking =
      ShelleyGenesisStaking
        { sgsPools = Map.fromList [(L.KeyHash "3dbe00a13dbe00a1", poolParams)],
          sgsStake = Map.fromList [(L.KeyHash "1c14ee8e1c14ee8e", L.KeyHash "1c14ee8e1c14ee8e")]
        }
