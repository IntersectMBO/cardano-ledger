{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Common.CBOR
  ( tests,
  )
where

import Cardano.Binary
  ( Case (..),
    Raw (..),
    SizeOverride (..),
    ToCBOR,
    decodeFullDecoder,
    serializeEncoding,
    szCases,
  )
import Cardano.Chain.Common
  ( AddrAttributes (..),
    AddrSpendingData (..),
    AddrType (..),
    Attributes (..),
    BlockCount (..),
    ChainDifficulty (..),
    TxFeePolicy (..),
    TxSizeLinear (..),
    decodeAddressBase58,
    decodeCrcProtected,
    encodeAddressBase58,
    encodeCrcProtected,
    isRedeemAddress,
    mkAttributes,
    mkKnownLovelace,
    mkMerkleTree,
    mtRoot,
    rationalToLovelacePortion,
  )
import Cardano.Crypto
  ( Hash,
    abstractHash,
    redeemDeterministicKeyGen,
  )
import Cardano.Prelude hiding (check)
import qualified Data.Map as M
import Hedgehog (Gen, Property, cover, forAll, property, (===))
import qualified Hedgehog as H
import Test.Cardano.Binary.Helpers (SizeTestConfig (..), scfg, sizeTest)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    roundTripsCBORBuildable,
    roundTripsCBORShow,
  )
import Test.Cardano.Chain.Common.Example
  ( exampleAddrSpendingData_VerKey,
    exampleAddress,
    exampleAddress1,
    exampleAddress2,
    exampleAddress3,
    exampleAddress4,
    exampleKeyHash,
  )
import Test.Cardano.Chain.Common.Gen
  ( genAddrAttributes,
    genAddrSpendingData,
    genAddrType,
    genAddress,
    genAttributes,
    genBlockCount,
    genChainDifficulty,
    genKeyHash,
    genLovelace,
    genLovelaceError,
    genLovelacePortion,
    genMerkleRoot,
    genMerkleTree,
    genTxFeePolicy,
    genTxSizeLinear,
  )
import Test.Cardano.Crypto.CBOR (getBytes)
import Test.Cardano.Crypto.Gen (genHashRaw)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- CRC encoding
--------------------------------------------------------------------------------

prop_roundTripCrcProtected :: Property
prop_roundTripCrcProtected = property $ do
  x <- forAll genAddress
  let crcEncodedBS = serializeEncoding . encodeCrcProtected $ x
  decodeFullDecoder "" decodeCrcProtected crcEncodedBS === Right x

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
golden_Address0 :: Property
golden_Address0 =
  goldenTestCBOR exampleAddress "test/golden/cbor/common/Address0"

golden_Address1 :: Property
golden_Address1 =
  goldenTestCBOR exampleAddress1 "test/golden/cbor/common/Address1"

golden_Address2 :: Property
golden_Address2 =
  goldenTestCBOR exampleAddress2 "test/golden/cbor/common/Address2"

golden_Address3 :: Property
golden_Address3 =
  goldenTestCBOR exampleAddress3 "test/golden/cbor/common/Address3"

golden_Address4 :: Property
golden_Address4 =
  goldenTestCBOR exampleAddress4 "test/golden/cbor/common/Address4"

golden_isRedeemAddrees :: Property
golden_isRedeemAddrees =
  H.withTests 1 . property $ do
    H.assert $ not (isRedeemAddress exampleAddress1)
    H.assert $ isRedeemAddress exampleAddress2

ts_roundTripAddressCBOR :: TSProperty
ts_roundTripAddressCBOR = eachOfTS 1000 genAddress roundTripsCBORBuildable

ts_roundTripAddress :: TSProperty
ts_roundTripAddress =
  eachOfTS 1000 genAddress $ \addr -> do
    cover 30 "Redeem Address" $ isRedeemAddress addr
    cover 30 "Pubkey Address" $ not (isRedeemAddress addr)
    decodeAddressBase58 (encodeAddressBase58 addr) === Right addr

--------------------------------------------------------------------------------
-- AddrSpendingData
--------------------------------------------------------------------------------
golden_AddrSpendingData_VerKey :: Property
golden_AddrSpendingData_VerKey =
  goldenTestCBOR
    exampleAddrSpendingData_VerKey
    "test/golden/cbor/common/AddrSpendingData_VerKey"

golden_AddrSpendingData_Redeem :: Property
golden_AddrSpendingData_Redeem =
  goldenTestCBOR asd "test/golden/cbor/common/AddrSpendingData_Redeem"
  where
    asd = RedeemASD redeemVerificationKey
    redeemVerificationKey =
      case fst <$> redeemDeterministicKeyGen (getBytes 0 32) of
        Nothing -> panic "golden_AddrSpendingData_Redeem: impossible"
        Just rk -> rk

ts_roundTripAddrSpendingDataCBOR :: TSProperty
ts_roundTripAddrSpendingDataCBOR =
  eachOfTS 1000 genAddrSpendingData roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- AddrType
--------------------------------------------------------------------------------
golden_AddrType_VK :: Property
golden_AddrType_VK = goldenTestCBOR ATVerKey "test/golden/cbor/common/AddrType_VK"

golden_AddrType_R :: Property
golden_AddrType_R = goldenTestCBOR ATRedeem "test/golden/cbor/common/AddrType_R"

ts_roundTripAddrTypeCBOR :: TSProperty
ts_roundTripAddrTypeCBOR = eachOfTS 1000 genAddrType roundTripsCBORShow

--------------------------------------------------------------------------------
-- BlockCount
--------------------------------------------------------------------------------
golden_BlockCount :: Property
golden_BlockCount = goldenTestCBOR bc "test/golden/cbor/common/BlockCount"
  where
    bc = BlockCount 999

ts_roundTripBlockCountCBOR :: TSProperty
ts_roundTripBlockCountCBOR = eachOfTS 1000 genBlockCount roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- ChainDifficulty
--------------------------------------------------------------------------------
golden_ChainDifficulty :: Property
golden_ChainDifficulty =
  goldenTestCBOR cd "test/golden/cbor/common/ChainDifficulty"
  where
    cd = ChainDifficulty 9999

ts_roundTripChainDifficultyCBOR :: TSProperty
ts_roundTripChainDifficultyCBOR =
  eachOfTS 1000 genChainDifficulty roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- Lovelace
--------------------------------------------------------------------------------
golden_Lovelace :: Property
golden_Lovelace = goldenTestCBOR c "test/golden/cbor/common/Lovelace"
  where
    c = mkKnownLovelace @9732

ts_roundTripLovelaceCBOR :: TSProperty
ts_roundTripLovelaceCBOR = eachOfTS 1000 genLovelace roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- LovelaceError
--------------------------------------------------------------------------------
ts_roundTripLovelaceErrorCBOR :: TSProperty
ts_roundTripLovelaceErrorCBOR =
  eachOfTS 1000 genLovelaceError roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- LovelacePortion
--------------------------------------------------------------------------------
golden_LovelacePortion :: Property
golden_LovelacePortion =
  goldenTestCBOR c "test/golden/cbor/common/LovelacePortion"
  where
    c = rationalToLovelacePortion 9702e-15

ts_roundTripLovelacePortionCBOR :: TSProperty
ts_roundTripLovelacePortionCBOR =
  eachOfTS 1000 genLovelacePortion roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- KeyHash
--------------------------------------------------------------------------------
golden_KeyHash :: Property
golden_KeyHash =
  goldenTestCBOR exampleKeyHash "test/golden/cbor/common/KeyHash"

ts_roundTripKeyHashCBOR :: TSProperty
ts_roundTripKeyHashCBOR =
  eachOfTS 1000 genKeyHash roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------
golden_TxFeePolicy_Linear :: Property
golden_TxFeePolicy_Linear =
  goldenTestCBOR tfp "test/golden/cbor/common/TxFeePolicy_Linear"
  where
    tfp = TxFeePolicyTxSizeLinear (TxSizeLinear c1 c2)
    c1 = mkKnownLovelace @99
    c2 = 777 :: Rational

ts_roundTripTxFeePolicyCBOR :: TSProperty
ts_roundTripTxFeePolicyCBOR =
  eachOfTS 1000 genTxFeePolicy roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- TxSizeLinear
--------------------------------------------------------------------------------
golden_TxSizeLinear :: Property
golden_TxSizeLinear =
  goldenTestCBOR tsl "test/golden/cbor/common/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = mkKnownLovelace @99
    c2 = 777 :: Rational

ts_roundTripTxSizeLinearCBOR :: TSProperty
ts_roundTripTxSizeLinearCBOR =
  eachOfTS 1000 genTxSizeLinear roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------
golden_Attributes :: Property
golden_Attributes = goldenTestCBOR attrib "test/golden/cbor/common/Attributes"
  where
    attrib = mkAttributes ()

ts_roundTripAttributes :: TSProperty
ts_roundTripAttributes =
  eachOfTS 50 (genAttributes (pure ())) roundTripsCBORShow

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------
golden_MerkleTree :: Property
golden_MerkleTree = goldenTestCBOR mTree "test/golden/cbor/common/MerkleTree"
  where
    mTree = mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]

ts_roundTripMerkleTree :: TSProperty
ts_roundTripMerkleTree =
  eachOfTS 10 (genMerkleTree genHashRaw) roundTripsCBORShow

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------
golden_MerkleRoot :: Property
golden_MerkleRoot = goldenTestCBOR mTree "test/golden/cbor/common/MerkleRoot"
  where
    mTree = mtRoot $ mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]

ts_roundTripMerkleRoot :: TSProperty
ts_roundTripMerkleRoot =
  eachOfTS 10 (genMerkleRoot genHashRaw) roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- Size estimates
--------------------------------------------------------------------------------
sizeEstimates :: H.Group
sizeEstimates =
  let check :: forall a. (Show a, ToCBOR a) => Gen a -> Property
      check g = sizeTest $ scfg {gen = g}

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize =
        ( typeRep (Proxy @(Attributes ())),
          SizeConstant 1
        )
      attrAddrSize =
        ( typeRep (Proxy @(Attributes AddrAttributes)),
          SizeConstant (szCases [Case "min" 1, Case "max" 1024])
        )
   in H.Group
        "Encoded size bounds for core types."
        [ ("Lovelace", check genLovelace),
          ("BlockCount", check genBlockCount),
          ( "Attributes ()",
            sizeTest $
              scfg
                { gen = genAttributes (pure ()),
                  addlCtx = M.fromList [attrUnitSize]
                }
          ),
          ( "Attributes AddrAttributes",
            sizeTest $
              scfg
                { gen = genAttributes genAddrAttributes,
                  addlCtx = M.fromList [attrAddrSize]
                }
          ),
          ( "Address",
            sizeTest $
              scfg
                { gen = genAddress,
                  addlCtx = M.fromList [attrAddrSize]
                }
          ),
          ( "AddrSpendingData",
            sizeTest $
              scfg
                { gen = genAddrSpendingData,
                  addlCtx =
                    M.fromList
                      [ ( typeRep (Proxy @AddrSpendingData),
                          SelectCases ["VerKeyASD", "RedeemASD"]
                        )
                      ]
                }
          ),
          ("AddrType", check genAddrType)
        ]

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests =
  concatTSGroups
    [const $$discoverGolden, $$discoverRoundTripArg, const sizeEstimates]
