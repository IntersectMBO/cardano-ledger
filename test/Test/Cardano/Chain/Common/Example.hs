{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Common.Example
  ( exampleAddress
  , exampleAddress1
  , exampleAddress2
  , exampleAttributes
  , exampleChainDifficulty
  , exampleStakeholderId
  )
where

import Cardano.Prelude

import Cardano.Chain.Common
  ( AddrAttributes(..)
  , AddrSpendingData(..)
  , Address
  , Attributes
  , ChainDifficulty(..)
  , StakeholderId
  , makeAddress
  , mkAttributes
  , mkStakeholderId
  )
import Cardano.Chain.Common (NetworkMagic(..))
import Cardano.Crypto.HD (HDAddressPayload(..))

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
  attrs = AddrAttributes hap nm
  hap   = Just (HDAddressPayload (getBytes 32 32))
  nm    = NetworkMainOrStage

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
 where
  easd  = PubKeyASD pk
  [pk]  = examplePublicKeys 24 1
  attrs = AddrAttributes hap nm
  hap   = Nothing :: Maybe HDAddressPayload
  nm    = NetworkMainOrStage

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
 where
  easd  = RedeemASD exampleRedeemPublicKey
  attrs = AddrAttributes hap nm
  hap   = Just (HDAddressPayload (getBytes 15 32))
  nm    = NetworkMainOrStage

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty 9999

exampleStakeholderId :: StakeholderId
exampleStakeholderId = mkStakeholderId examplePublicKey
