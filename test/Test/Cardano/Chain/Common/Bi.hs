{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Cardano.Chain.Common.Bi
       ( tests
       ) where

import Cardano.Prelude hiding (check)
import Test.Cardano.Prelude

import qualified Data.Map as M
import Data.Typeable (typeRep)
import Hedgehog
  (Gen, Group, Property)
import qualified Hedgehog as H

import Cardano.Binary.Class (Bi, Case(..), Raw(..), SizeOverride(..), szCases)
import Cardano.Chain.Common
  ( AddrAttributes(..)
  , AddrSpendingData (..)
  , AddrType(..)
  , Attributes(..)
  , BlockCount(..)
  , ChainDifficulty(..)
  , LovelacePortion(..)
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , mkAttributes
  , mkKnownLovelace
  , mkMerkleTree
  , mtRoot
  )
import Cardano.Crypto
  ( Hash
  , abstractHash
  , redeemDeterministicKeyGen
  )

import Test.Cardano.Binary.Helpers (SizeTestConfig (..), sizeTest, scfg)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( goldenTestBi
  , roundTripsBiBuildable
  , roundTripsBiShow
  )
import Test.Cardano.Chain.Common.Example
  ( exampleAddress
  , exampleAddress1
  , exampleAddress2
  , exampleAddress3
  , exampleAddress4
  , exampleAddrSpendingData_PubKey
  , exampleStakeholderId

  )
import Test.Cardano.Chain.Common.Gen
  ( genAddress
  , genAddrAttributes
  , genAddrSpendingData
  , genAddrType
  , genAttributes
  , genBlockCount
  , genChainDifficulty
  , genLovelace
  , genLovelacePortion
  , genMerkleTree
  , genMerkleRoot
  , genStakeholderId
  , genTxFeePolicy
  , genTxSizeLinear
  )
import Test.Cardano.Crypto.Bi (getBytes)
import Test.Cardano.Crypto.Gen (genHashRaw)
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
golden_Address0 :: Property
golden_Address0 = goldenTestBi exampleAddress "test/golden/bi/common/Address0"

golden_Address1 :: Property
golden_Address1 = goldenTestBi exampleAddress1 "test/golden/bi/common/Address1"

golden_Address2 :: Property
golden_Address2 = goldenTestBi exampleAddress2 "test/golden/bi/common/Address2"

golden_Address3 :: Property
golden_Address3 = goldenTestBi exampleAddress3 "test/golden/bi/common/Address3"

golden_Address4 :: Property
golden_Address4 = goldenTestBi exampleAddress4 "test/golden/bi/common/Address4"

ts_roundTripAddressBi :: TSProperty
ts_roundTripAddressBi = eachOfTS 1000 genAddress roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrSpendingData
--------------------------------------------------------------------------------
golden_AddrSpendingData_PubKey :: Property
golden_AddrSpendingData_PubKey = goldenTestBi exampleAddrSpendingData_PubKey
                                              "test/golden/bi/common/AddrSpendingData_PubKey"

golden_AddrSpendingData_Redeem :: Property
golden_AddrSpendingData_Redeem = goldenTestBi asd "test/golden/bi/common/AddrSpendingData_Redeem"
  where
    asd = RedeemASD redeemPublicKey
    redeemPublicKey = case fst <$> redeemDeterministicKeyGen (getBytes 0 32) of
                        Nothing -> panic "golden_AddrSpendingData_Redeem: impossible"
                        Just rk -> rk

ts_roundTripAddrSpendingDataBi :: TSProperty
ts_roundTripAddrSpendingDataBi = eachOfTS 1000 genAddrSpendingData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrType
--------------------------------------------------------------------------------
golden_AddrType_PK :: Property
golden_AddrType_PK = goldenTestBi ATPubKey "test/golden/bi/common/AddrType_PK"

golden_AddrType_R :: Property
golden_AddrType_R = goldenTestBi ATRedeem "test/golden/bi/common/AddrType_R"

ts_roundTripAddrTypeBi :: TSProperty
ts_roundTripAddrTypeBi = eachOfTS 1000 genAddrType roundTripsBiShow

--------------------------------------------------------------------------------
-- BlockCount
--------------------------------------------------------------------------------
golden_BlockCount :: Property
golden_BlockCount = goldenTestBi bc "test/golden/bi/common/BlockCount"
  where bc = BlockCount 999

ts_roundTripBlockCountBi :: TSProperty
ts_roundTripBlockCountBi = eachOfTS 1000 genBlockCount roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ChainDifficulty
--------------------------------------------------------------------------------
golden_ChainDifficulty :: Property
golden_ChainDifficulty = goldenTestBi cd "test/golden/bi/common/ChainDifficulty"
  where cd = ChainDifficulty 9999

ts_roundTripChainDifficultyBi :: TSProperty
ts_roundTripChainDifficultyBi = eachOfTS 1000 genChainDifficulty roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Lovelace
--------------------------------------------------------------------------------
golden_Lovelace :: Property
golden_Lovelace = goldenTestBi c "test/golden/bi/common/Lovelace"
  where c = mkKnownLovelace @9732

ts_roundTripLovelaceBi :: TSProperty
ts_roundTripLovelaceBi = eachOfTS 1000 genLovelace roundTripsBiBuildable

--------------------------------------------------------------------------------
-- LovelacePortion
--------------------------------------------------------------------------------
golden_LovelacePortion :: Property
golden_LovelacePortion = goldenTestBi c "test/golden/bi/common/LovelacePortion"
  where c = LovelacePortion 9702

ts_roundTripLovelacePortionBi :: TSProperty
ts_roundTripLovelacePortionBi = eachOfTS 1000 genLovelacePortion roundTripsBiBuildable


--------------------------------------------------------------------------------
-- StakeholderId
--------------------------------------------------------------------------------
golden_StakeholderId :: Property
golden_StakeholderId =
    goldenTestBi exampleStakeholderId "test/golden/bi/common/StakeholderId"

ts_roundTripStakeholderIdBi :: TSProperty
ts_roundTripStakeholderIdBi = eachOfTS 1000 genStakeholderId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------
golden_TxFeePolicy_Linear :: Property
golden_TxFeePolicy_Linear = goldenTestBi tfp "test/golden/bi/common/TxFeePolicy_Linear"
  where
    tfp = TxFeePolicyTxSizeLinear (TxSizeLinear c1 c2)
    c1 = mkKnownLovelace @99
    c2 = mkKnownLovelace @777

ts_roundTripTxFeePolicyBi :: TSProperty
ts_roundTripTxFeePolicyBi = eachOfTS 1000 genTxFeePolicy roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSizeLinear
--------------------------------------------------------------------------------
golden_TxSizeLinear :: Property
golden_TxSizeLinear = goldenTestBi tsl "test/golden/bi/common/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = mkKnownLovelace @99
    c2 = mkKnownLovelace @777

ts_roundTripTxSizeLinearBi :: TSProperty
ts_roundTripTxSizeLinearBi = eachOfTS 1000 genTxSizeLinear roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------
golden_Attributes :: Property
golden_Attributes = goldenTestBi attrib "test/golden/bi/common/Attributes"
    where attrib = mkAttributes ()

ts_roundTripAttributes :: TSProperty
ts_roundTripAttributes = eachOfTS 50 (genAttributes (pure ())) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------
golden_MerkleTree :: Property
golden_MerkleTree = goldenTestBi mTree "test/golden/bi/common/MerkleTree"
    where mTree = mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]


ts_roundTripMerkleTree :: TSProperty
ts_roundTripMerkleTree = eachOfTS 10 (genMerkleTree genHashRaw) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------
golden_MerkleRoot :: Property
golden_MerkleRoot = goldenTestBi mTree "test/golden/bi/common/MerkleRoot"
    where mTree = mtRoot $ mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]

ts_roundTripMerkleRoot :: TSProperty
ts_roundTripMerkleRoot = eachOfTS 10 (genMerkleRoot genHashRaw) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Size estimates
--------------------------------------------------------------------------------
sizeEstimates :: H.Group
sizeEstimates =
  let check :: forall a. (Show a, Bi a) => Gen a -> Property
      check g = sizeTest $ scfg { gen = g }

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize = (typeRep (Proxy @(Attributes ()))
                     , SizeConstant 1)
      attrAddrSize = (typeRep (Proxy @(Attributes AddrAttributes)),
                      SizeConstant (szCases [ Case "min" 1, Case "max" 1024 ]))

  in H.Group "Encoded size bounds for core types."
        [ ("Lovelace"             , check genLovelace)
        , ("BlockCount"           , check genBlockCount)
        , ("Attributes ()"        , sizeTest $ scfg
              { gen = genAttributes (pure ())
              , addlCtx = M.fromList [ attrUnitSize ]
              })
        , ("Attributes AddrAttributes", sizeTest $ scfg
              { gen = genAttributes genAddrAttributes
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("Address"              , sizeTest $ scfg
              { gen = genAddress
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("AddrSpendingData"     , sizeTest $ scfg
              { gen = genAddrSpendingData
              , addlCtx = M.fromList
                  [ (typeRep (Proxy @AddrSpendingData),
                     SelectCases ["PubKeyASD", "RedeemASD"])
                  ] })
        , ("AddrType"             , check genAddrType)
        ]

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
    , H.checkParallel sizeEstimates
    ]
