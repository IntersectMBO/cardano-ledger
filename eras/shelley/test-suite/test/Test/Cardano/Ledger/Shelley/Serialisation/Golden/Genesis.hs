{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis (
  tests,

  -- * Individual properties
  golden_json_ShelleyGenesis,
  golden_cbor_ShelleyGenesis,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (textToDns, textToUrl)
import Cardano.Ledger.Binary (Tokens (..))
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core (emptyPParams, ppDL, ppMaxBBSizeL, ppMaxBHSizeL)
import Cardano.Ledger.Crypto (Crypto (HASH), StandardCrypto)
import Cardano.Ledger.Keys (hashKey, hashVerKeyVRF)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as L
import Cardano.Ledger.Shelley.Genesis
import Cardano.Slotting.Slot (EpochSize (..))
import Control.Monad
import Data.Aeson hiding (Encoding)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ListMap as LM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import Paths_cardano_ledger_shelley_test (getDataFileName)
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (CBORBytes), diffExpr)
import Test.Cardano.Ledger.Core.KeyPair (vKey)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import Test.Cardano.Ledger.Shelley.Generator.Core (VRFKeyPair (..))
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkVRFKeyPair,
  unsafeBoundRational,
 )
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

goldenTestJSON :: ToJSON a => a -> FilePath -> Assertion
goldenTestJSON actual expectedFile =
  case eitherDecode' (encode actual) of
    Left err -> error err
    Right (val :: Value) -> do
      expected <- either error id <$> eitherDecodeFileStrict expectedFile
      val @?= expected

golden_json_ShelleyGenesis :: Assertion
golden_json_ShelleyGenesis =
  goldenTestJSON example =<< getDataFileName "test/Golden/ShelleyGenesis"
  where
    example :: ShelleyGenesis StandardCrypto
    example = exampleShelleyGenesis

golden_cbor_ShelleyGenesis :: Assertion
golden_cbor_ShelleyGenesis =
  when (received /= expected) $
    assertFailure
      (diffExpr (CBORBytes expected) (CBORBytes received))
  where
    example :: ShelleyGenesis StandardCrypto
    example = exampleShelleyGenesis

    received = Plain.serialize' expectedTokens
    expected = Plain.serialize' example

    expectedTokens =
      TkListLen 15
        . TkListLen 3
        . TkInt 2009
        . TkInt 44
        . TkInt 83589000000000000 -- sgSystemStart
        . TkInt 4036000900 -- sgNetworkMagic
        . TkInt 0 -- sgNetworkId
        . TkListLen 2
        . TkInt 259
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
        . TkInt 2
        . TkInt 0
        . TkInt 0
        . TkInt 0
        . TkMapLen 1 -- sgGenDelegs
        . TkBytes
          "8\231\197\152j4\243\&4\225\155q,\n\165%\DC4m\171\143\SI\248\137\178\173\SYN\137BA"
        . TkListLen 2
        . TkBytes
          "\230\150\r\214q\238\141s\222\SUB\131\209\&4[f\DC1e\220\221\235\169\150#\190\239/\NAKz"
        . TkBytes
          "\252\227\FSo1\135S\RS\228\163\154\167C\194M\"'_AZ\136\149\233\205\"\195\f\138%\205\239\r"
        . TkMapLen 1 -- sgInitialFunds
        . TkBytes
          "\NUL\225\186\222*\185FZ$\253^\170Ks\136\&09\ETB3\143\133:H\177\SUB_\201\150J\245\202\147\251e\SYN\235\222=\NAK\v>*\203\EM\tS'0\222\&3\131/\251\"\156\178\r"
        . TkInt 12157196
        . TkListLen 2 -- sgStaking
        . TkMapLen 1 -- sgsPools
        . TkBytes
          "\245\131\164^IG\193\STX\t\ESC\150\ETB\SO\245\SO\240\207\142\219bfa\147\162\SYN2G\187"
        . TkListLen 9 -- PoolParams
        . TkBytes
          "N\DC3\f\v\222\183v\142\223.\143\133\NUL\DEL\213 s\227\220\CANq\244\196\DEL\157\252\169."
        . TkBytes
          "h\249\207\211:\200\240D\250\204fM\181\170\ESCs\192\176\245C[\133\231\181 \187/\SUB\146\240)\153"
        . TkInt 1
        . TkInt 5
        . TkTag 30
        . TkListLen 2
        . TkInt 1
        . TkInt 4
        . TkBytes
          "\224N\136\204-'\195d\170\249\ACKH\168}\251\149\248\238\DLE;\166\DEL\161\241/^\134\196*"
        . TkListLen 1
        . TkBytes "N\136\204-'\195d\170\249\ACKH\168}\251\149\248\238\DLE;\166\DEL\161\241/^\134\196*"
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
        . TkBytes
          "\131\161\146\222\192\232\218!\136\229 \208\197\&6\166\154t|\241s\163\223\SYN\166\218\169M\134"
        . TkBytes "d\158\218\130\191dM4\166\146_$\234LL6\210~Q\222\ESCD\239G\227V\v\231"

-- TODO - return a CBOR diff in the case of failure

tests :: TestTree
tests =
  testGroup
    "Shelley Genesis golden tests"
    [ testCase "ShelleyGenesis JSON golden test" golden_json_ShelleyGenesis
    , testCase "ShelleyGenesis CBOR golden test" golden_cbor_ShelleyGenesis
    ]

exampleShelleyGenesis ::
  forall c.
  Crypto c =>
  ShelleyGenesis c
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer)
    , sgNetworkMagic = 4036000900
    , sgNetworkId = L.Testnet
    , sgActiveSlotsCoeff = unsafeBoundRational 0.259
    , sgSecurityParam = 120842
    , sgEpochLength = EpochSize 1215
    , sgSlotsPerKESPeriod = 8541
    , sgMaxKESEvolutions = 28899
    , sgSlotLength = 8
    , sgUpdateQuorum = 16991
    , sgMaxLovelaceSupply = 71
    , sgProtocolParams =
        emptyPParams @(ShelleyEra c)
          & ppDL .~ unsafeBoundRational (realToFrac (1.9e-2 :: Scientific))
          & ppMaxBBSizeL .~ 239857
          & ppMaxBHSizeL .~ 217569
    , sgGenDelegs = Map.fromList [(genesisVerKeyHash, genDelegPair)]
    , sgInitialFunds = LM.ListMap [(initialFundedAddress, initialFunds)]
    , sgStaking = staking
    }
  where
    -- hash of the genesis verification key
    genesisVerKeyHash :: L.KeyHash 'L.Genesis c
    genesisVerKeyHash = L.KeyHash "38e7c5986a34f334e19b712c0aa525146dab8f0ff889b2ad16894241"
    -- hash of the delegators verififation key
    genDelegPair = L.GenDelegPair delegVerKeyHash delegVrfKeyHash
    delegVerKeyHash :: L.KeyHash 'L.GenesisDelegate c
    delegVerKeyHash = L.KeyHash "e6960dd671ee8d73de1a83d1345b661165dcddeba99623beef2f157a"
    delegVrfKeyHash :: Hash.Hash (HASH c) (L.VerKeyVRF c)
    delegVrfKeyHash = "fce31c6f3187531ee4a39aa743c24d22275f415a8895e9cd22c30c8a25cdef0d"
    initialFundedAddress :: L.Addr c
    initialFundedAddress =
      L.Addr
        L.Testnet
        paymentCredential
        (L.StakeRefBase stakingCredential)
      where
        paymentCredential =
          L.KeyHashObj $
            L.KeyHash
              "e1bade2ab9465a24fd5eaa4b7388303917338f853a48b11a5fc9964a"
        stakingCredential =
          L.KeyHashObj $
            L.KeyHash
              "f5ca93fb6516ebde3d150b3e2acb1909532730de33832ffb229cb20d"
    initialFunds :: L.Coin
    initialFunds = L.Coin 12157196
    relays =
      StrictSeq.fromList
        [ L.SingleHostAddr
            (L.SJust $ L.Port 1234)
            (L.SJust $ read "0.0.0.0")
            (L.SJust $ read "2001:db8:a::123")
        , L.SingleHostName L.SNothing (fromJust $ textToDns 64 "cool.domain.com")
        , L.MultiHostName (fromJust $ textToDns 64 "cool.domain.com")
        ]
    poolParams :: L.PoolParams c
    poolParams =
      L.PoolParams
        { L.ppId = hashKey . snd $ mkKeyPair (RawSeed 1 0 0 0 1)
        , L.ppVrf = hashVerKeyVRF . vrfVerKey $ mkVRFKeyPair @c (RawSeed 1 0 0 0 2)
        , L.ppPledge = L.Coin 1
        , L.ppCost = L.Coin 5
        , L.ppMargin = unsafeBoundRational 0.25
        , L.ppRewardAcnt = L.RewardAcnt L.Testnet Cast.aliceSHK
        , L.ppOwners = Set.singleton $ hashKey (vKey Cast.aliceStake)
        , L.ppRelays = relays
        , L.ppMetadata =
            L.SJust $
              L.PoolMetadata
                { L.pmUrl = fromJust $ textToUrl 64 "best.pool.com"
                , L.pmHash = BS.pack "100ab{}100ab{}"
                }
        }
    staking =
      ShelleyGenesisStaking
        { sgsPools =
            LM.ListMap
              [ (L.KeyHash "f583a45e4947c102091b96170ef50ef0cf8edb62666193a2163247bb", poolParams)
              ]
        , sgsStake =
            LM.ListMap
              [
                ( L.KeyHash "83a192dec0e8da2188e520d0c536a69a747cf173a3df16a6daa94d86"
                , L.KeyHash "649eda82bf644d34a6925f24ea4c4c36d27e51de1b44ef47e3560be7"
                )
              ]
        }
