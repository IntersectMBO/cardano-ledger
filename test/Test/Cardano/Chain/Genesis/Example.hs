{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

module Test.Cardano.Chain.Genesis.Example
  ( exampleGenesisAvvmBalances
  , exampleGenesisData0
  , exampleGenesisDelegation
  , exampleGenesisInitializer
  , exampleStaticConfig_GCSpec
  , exampleStaticConfig_GCSrc
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Base16 as B16
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time (UTCTime(..), Day(..), secondsToDiffTime)

import Cardano.Binary (Annotated(..), Raw(..))
import Cardano.Chain.Common
  ( BlockCount(..)
  , LovelacePortion(..)
  , mkKnownLovelace
  , mkKnownLovelacePortion
  , hashKey
  )
import Cardano.Chain.Genesis
  ( FakeAvvmOptions(..)
  , GenesisNonAvvmBalances(..)
  , GenesisAvvmBalances(..)
  , GenesisData(..)
  , GenesisDelegation(..)
  , GenesisInitializer(..)
  , GenesisSpec(..)
  , GenesisKeyHashes(..)
  , StaticConfig(..)
  , TestnetBalanceOptions(..)
  )
import Cardano.Chain.Slotting (EpochIndex(..))
import Cardano.Crypto
  ( AProtocolMagic(..)
  , ProtocolMagicId(..)
  , RequiresNetworkMagic(..)
  , ProxyCert(..)
  , RedeemVerificationKey
  , Signature(..)
  , abstractHash
  , redeemDeterministicKeyGen
  , unsafeProxyVerificationKey
  )
import Cardano.Crypto.Signing (VerificationKey(..))
import qualified Cardano.Crypto.Wallet as CC

import Test.Cardano.Chain.Common.Example
  (exampleAddress, exampleAddress1, exampleKeyHash)
import Test.Cardano.Chain.Update.Example (exampleProtocolParameters)
import Test.Cardano.Crypto.CBOR (getBytes)
import Test.Cardano.Crypto.Example (exampleProtocolMagicId0)


exampleBlockCount :: BlockCount
exampleBlockCount = BlockCount 12344

exampleStaticConfig_GCSrc :: StaticConfig
exampleStaticConfig_GCSrc =
  GCSrc "dRaMwdYsH3QA3dChe" (abstractHash (Raw "Test"))

exampleStaticConfig_GCSpec :: StaticConfig
exampleStaticConfig_GCSpec = GCSpec $ UnsafeGenesisSpec
  exampleGenesisAvvmBalances
  exampleGenesisDelegation
  exampleProtocolParameters
  (BlockCount 37)
  (AProtocolMagic (Annotated (ProtocolMagicId 1783847074) ()) RequiresMagic)
  exampleGenesisInitializer

exampleGenesisAvvmBalances :: GenesisAvvmBalances
exampleGenesisAvvmBalances = GenesisAvvmBalances $ M.fromList
  [ (exampleRedeemVerificationKey' (0, 32) , mkKnownLovelace @36524597913081152)
  , (exampleRedeemVerificationKey' (32, 32), mkKnownLovelace @37343863242999412)
  ]
 where
  exampleRedeemVerificationKey' :: (Int, Int) -> RedeemVerificationKey
  exampleRedeemVerificationKey' (m, n) =
    fromJust (fst <$> redeemDeterministicKeyGen (getBytes m n))

exampleGenesisData0 :: GenesisData
exampleGenesisData0 = GenesisData
  { gdGenesisKeyHashes = exampleGenesisKeyHashes
  , gdHeavyDelegation = exampleGenesisDelegation
  , gdStartTime = exampleUTCTime0
  , gdNonAvvmBalances = exampleGenesisNonAvvmBalances0
  , gdProtocolParameters = exampleProtocolParameters
  , gdK         = exampleBlockCount
  , gdProtocolMagicId = exampleProtocolMagicId0
  , gdAvvmDistr = exampleGenesisAvvmBalances
  }

exampleGenesisDelegation :: GenesisDelegation
exampleGenesisDelegation = UnsafeGenesisDelegation
  (M.fromList
    [ ( hashKey issueVerKey
      , unsafeProxyVerificationKey
        (EpochIndex 68300481033)
        issueVerKey
        (VerificationKey
          (CC.XPub
            { CC.xpubPublicKey = pskDelVerKey
            , CC.xpubChaincode = pskDelChainCode
            }
          )
        )
        (ProxyCert sig)
      )
    ]
  )
 where
  issueVerKey = VerificationKey
    (CC.XPub {CC.xpubPublicKey = pskVerKey, CC.xpubChaincode = pskChainCode})
  sig :: Signature EpochIndex
  sig = Signature $ fromRight (panic "Something went wrong") $ CC.xsignature
    (hexToBS
      "bae5422af5405e3803154a4ad986da5d14cf624d670\
                                 \1c5c78a79ec73777f74e13973af83752114d9f18166\
                                 \085997fc81e432cab7fee99a275d8bf138ad04e103"
    )
  pskVerKey =
    hexToBS
      "e2a1773a2a82d10c30890cbf84eccbdc1aaaee920496424d36e8\
                        \68039d9cb519"
  pskChainCode = CC.ChainCode
    (hexToBS
      "21b25efe033d9b00d4f02ccd9cdabcec332\
                                         \abbc6fdf883ca5bf3a8aff4aac27e"
    )
  pskDelVerKey =
    hexToBS
      "ddca69bfeac14c013304da88ac032ee63281ab036c1b1b918\
                           \8e4b174b303f43e"
  pskDelChainCode = CC.ChainCode
    (hexToBS
      "55163b178e999b9fd50637b2edab8c85\
                                            \8a879ac3c4bd3e610095419a19696573"
    )

exampleGenesisInitializer :: GenesisInitializer
exampleGenesisInitializer = GenesisInitializer
  { giTestBalance = TestnetBalanceOptions
    { tboPoors          = 2448641325904532856
    , tboRichmen        = 14071205313513960321
    , tboTotalBalance   = mkKnownLovelace @10953275486128625
    , tboRichmenShare   = mkKnownLovelacePortion @366832547637728
    , tboUseHDAddresses = True
    }
  , giFakeAvvmBalance = FakeAvvmOptions
    { faoCount      = 17853231730478779264
    , faoOneBalance = mkKnownLovelace @15087947214890024
    }
  , giAvvmBalanceFactor = LovelacePortion {getLovelacePortion = 366832547637728}
  , giUseHeavyDlg = False
  , giSeed        = 0
  }

exampleGenesisNonAvvmBalances0 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances0 = GenesisNonAvvmBalances
  $ M.fromList [(exampleAddress, coin), (exampleAddress1, coin1)]
 where
  coin  = mkKnownLovelace @36524597913081152
  coin1 = mkKnownLovelace @37343863242999412

exampleGenesisKeyHashes :: GenesisKeyHashes
exampleGenesisKeyHashes = GenesisKeyHashes (Set.singleton exampleKeyHash)

exampleUTCTime0 :: UTCTime
exampleUTCTime0 = UTCTime (ModifiedJulianDay 10000) (secondsToDiffTime 82401)

hexToBS :: ByteString -> ByteString
hexToBS ts = case B16.decode ts of
  (fullyDecoded, "") -> fullyDecoded
  (partiallyDecoded, invalid) ->
    panic
      $  "successfully decoded: "
      <> show partiallyDecoded
      <> " decode failed: "
      <> show invalid
