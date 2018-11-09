{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Common.Example
  ( exampleAddress
  , exampleAddress1
  , exampleAddress2
  , exampleAddress3
  , exampleAddress4
  , exampleAttributes
  , exampleChainDifficulty
  , exampleStakeholderId
  )
where

import Cardano.Prelude

import Cardano.Chain.Common
  ( AddrAttributes(..)
  , AddrSpendingData(..)
  , AddrStakeDistribution(..)
  , Address
  , Attributes
  , BlockCount(..)
  , ChainDifficulty(..)
  , LovelacePortion(..)
  , Script(..)
  , StakeholderId(..)
  , lovelacePortionDenominator
  , makeAddress
  , mkAttributes
  , mkMultiKeyDistr
  , mkStakeholderId
  )
import Cardano.Crypto.Hashing (abstractHash)
import Cardano.Crypto.HD (HDAddressPayload(..))
import qualified Data.Map as M

import Test.Cardano.Crypto.Bi (getBytes)
import Test.Cardano.Crypto.Example
  (examplePublicKey, examplePublicKeys, exampleRedeemPublicKey)


exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePublicKey

exampleAddress :: Address
exampleAddress = makeAddress exampleAddrSpendingData_PubKey attrs
 where
  attrs = AddrAttributes hap BootstrapEraDistr
  hap   = Just (HDAddressPayload (getBytes 32 32))

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
 where
  easd  = PubKeyASD pk
  [pk]  = examplePublicKeys 24 1
  attrs = AddrAttributes hap BootstrapEraDistr
  hap   = Nothing :: Maybe HDAddressPayload

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
 where
  easd  = RedeemASD exampleRedeemPublicKey
  attrs = AddrAttributes hap asd
  hap   = Just (HDAddressPayload (getBytes 15 32))
  asd   = SingleKeyDistr exampleStakeholderId

exampleAddress3 :: Address
exampleAddress3 = makeAddress easd attrs
 where
  easd  = ScriptASD exampleScript
  attrs = AddrAttributes hap exampleMultiKeyDistr
  hap   = Just (HDAddressPayload (getBytes 17 32))

exampleAddress4 :: Address
exampleAddress4 = makeAddress easd attrs
 where
  easd  = UnknownASD 7 "test value"
  attrs = AddrAttributes Nothing (SingleKeyDistr sId)
  [sId] = exampleStakeholderIds 7 1

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleMultiKeyDistr :: AddrStakeDistribution
exampleMultiKeyDistr = case mkMultiKeyDistr (M.fromList pairs) of
  Left err ->
    panic
      $  "exampleMultiKeyDistr: improperly constructed stake map: "
      <> show err
  Right asd -> asd
 where
  pairs :: [(StakeholderId, LovelacePortion)]
  pairs = zip stakeIds (map LovelacePortion (remainderCP : lovelacePortions))
  stakeIds :: [StakeholderId]
  stakeIds = map (StakeholderId . abstractHash) (examplePublicKeys 7 4)
  lovelacePortions =
    [ (10 :: Word64) ^ (12 :: Word64)
    , (7 :: Word64) ^ (11 :: Word64)
    , (6 :: Word64) ^ (14 :: Word64)
    ]
  remainderCP = lovelacePortionDenominator - sum lovelacePortions

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakeholderIds :: Int -> Int -> [StakeholderId]
exampleStakeholderIds offset l =
  map (StakeholderId . abstractHash) $ examplePublicKeys offset l

exampleStakeholderId :: StakeholderId
exampleStakeholderId = mkStakeholderId examplePublicKey
