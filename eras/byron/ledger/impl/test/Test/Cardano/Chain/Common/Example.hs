{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Common.Example
  ( exampleAddress,
    exampleAddress1,
    exampleAddress2,
    exampleAddress3,
    exampleAddress4,
    exampleAttributes,
    exampleAddrSpendingData_VerKey,
    exampleChainDifficulty,
    exampleKeyHash,
  )
where

import Cardano.Chain.Common
  ( AddrAttributes (..),
    AddrSpendingData (..),
    Address,
    Attributes,
    ChainDifficulty (..),
    HDAddressPayload (..),
    KeyHash,
    NetworkMagic (..),
    hashKey,
    makeAddress,
    mkAttributes,
  )
import Cardano.Prelude
import Test.Cardano.Crypto.CBOR (getBytes)
import Test.Cardano.Crypto.Example
  ( exampleRedeemVerificationKey,
    exampleVerificationKey,
    exampleVerificationKeys,
  )

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleAddrSpendingData_VerKey :: AddrSpendingData
exampleAddrSpendingData_VerKey = VerKeyASD exampleVerificationKey

exampleAddress :: Address
exampleAddress = makeAddress exampleAddrSpendingData_VerKey attrs
  where
    attrs = AddrAttributes hap nm
    hap = Just (HDAddressPayload (getBytes 32 32))
    nm = NetworkMainOrStage

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
  where
    easd = VerKeyASD vk
    [vk] = exampleVerificationKeys 24 1
    attrs = AddrAttributes hap nm
    hap = Nothing :: Maybe HDAddressPayload
    nm = NetworkMainOrStage

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
  where
    easd = RedeemASD exampleRedeemVerificationKey
    attrs = AddrAttributes hap nm
    hap = Just (HDAddressPayload (getBytes 15 32))
    nm = NetworkMainOrStage

exampleAddress3 :: Address
exampleAddress3 = makeAddress easd attrs
  where
    easd = VerKeyASD vk
    [vk] = exampleVerificationKeys 20 1
    attrs = AddrAttributes hap nm
    hap = Just (HDAddressPayload (getBytes 17 32))
    nm = NetworkTestnet 9973261

exampleAddress4 :: Address
exampleAddress4 = makeAddress easd attrs
  where
    easd = exampleAddrSpendingData_VerKey
    attrs = AddrAttributes Nothing nm
    nm = NetworkTestnet 11111911

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty 9999

exampleKeyHash :: KeyHash
exampleKeyHash = hashKey exampleVerificationKey
