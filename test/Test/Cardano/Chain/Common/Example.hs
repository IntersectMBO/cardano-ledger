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
  , Address
  , Attributes
  , BlockCount(..)
  , ChainDifficulty(..)
  , Script(..)
  , StakeholderId
  , makeAddress
  , mkAttributes
  , mkStakeholderId
  )
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
  attrs = AddrAttributes hap
  hap   = Just (HDAddressPayload (getBytes 32 32))

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
 where
  easd  = PubKeyASD pk
  [pk]  = examplePublicKeys 24 1
  attrs = AddrAttributes hap
  hap   = Nothing :: Maybe HDAddressPayload

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
 where
  easd  = RedeemASD exampleRedeemPublicKey
  attrs = AddrAttributes hap
  hap   = Just (HDAddressPayload (getBytes 15 32))

exampleAddress3 :: Address
exampleAddress3 = makeAddress easd attrs
 where
  easd  = ScriptASD exampleScript
  attrs = AddrAttributes hap
  hap   = Just (HDAddressPayload (getBytes 17 32))

exampleAddress4 :: Address
exampleAddress4 = makeAddress easd attrs
 where
  easd  = UnknownASD 7 "test value"
  attrs = AddrAttributes Nothing

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakeholderId :: StakeholderId
exampleStakeholderId = mkStakeholderId examplePublicKey
