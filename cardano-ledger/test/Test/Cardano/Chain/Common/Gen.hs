{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Common.Gen
  ( genAddrAttributes
  , genAddrAttributesWithNM
  , genAddress
  , genAddressWithNM
  , genAddrType
  , genAddrSpendingData
  , genAttributes
  , genBlockCount
  , genCanonicalTxFeePolicy
  , genChainDifficulty
  , genCompactAddress
  , genCustomLovelace
  , genLovelace
  , genLovelaceError
  , genLovelaceWithRange
  , genLovelacePortion
  , genMerkleRoot
  , genMerkleTree
  , genNetworkMagic
  , genScriptVersion
  , genKeyHash
  , genTxFeePolicy
  , genTxSizeLinear
  )
where

import Cardano.Prelude

import Formatting (build, sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Binary (ToCBOR)
import Cardano.Chain.Common
  ( AddrAttributes(..)
  , AddrSpendingData(..)
  , AddrType(..)
  , Address(..)
  , Attributes
  , BlockCount(..)
  , ChainDifficulty(..)
  , CompactAddress
  , Lovelace
  , LovelaceError(..)
  , LovelacePortion(..)
  , MerkleRoot(..)
  , MerkleTree
  , NetworkMagic(..)
  , KeyHash
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , lovelacePortionDenominator
  , makeAddress
  , maxLovelaceVal
  , mkAttributes
  , mkLovelace
  , mkMerkleTree
  , hashKey
  , mtRoot
  , toCompactAddress
  )

import Test.Cardano.Crypto.Gen
  (genHDAddressPayload, genVerificationKey, genRedeemVerificationKey)


genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = genAddrAttributesWithNM =<< genNetworkMagic

genAddrAttributesWithNM :: NetworkMagic -> Gen AddrAttributes
genAddrAttributesWithNM nm = AddrAttributes <$> hap <*> pure nm
  where hap = Gen.maybe genHDAddressPayload

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddressWithNM :: NetworkMagic -> Gen Address
genAddressWithNM nm = makeAddress <$> genAddrSpendingData
                                  <*> genAddrAttributesWithNM nm

genAddrType :: Gen AddrType
genAddrType = Gen.choice [pure ATVerKey, pure ATRedeem]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData =
  Gen.choice [VerKeyASD <$> genVerificationKey, RedeemASD <$> genRedeemVerificationKey]

genAttributes :: Gen a -> Gen (Attributes a)
genAttributes genA = mkAttributes <$> genA

genBlockCount :: Gen BlockCount
genBlockCount = BlockCount <$> Gen.word64 Range.constantBounded

genCanonicalTxFeePolicy :: Gen TxFeePolicy
genCanonicalTxFeePolicy = TxFeePolicyTxSizeLinear <$> genCanonicalTxSizeLinear

genCanonicalTxSizeLinear :: Gen TxSizeLinear
genCanonicalTxSizeLinear = TxSizeLinear <$> genLovelace' <*> genLovelace'
 where
  genLovelace' :: Gen Lovelace
  genLovelace' =
    mkLovelace
      <$> Gen.word64 (Range.constant 0 maxCanonicalLovelaceVal)
      >>= \case
            Right lovelace -> pure lovelace
            Left  err      -> panic $ sformat
              ("The impossible happened in genLovelace: " . build)
              err
  -- | Maximal possible value of `Lovelace` in Canonical JSON (JSNum !Int54)
  -- This should be (2^53 - 1) ~ 9e15, however in the Canonical ToJSON instance of
  -- `TxFeePolicy` this number is multiplied by 1e9 to keep compatibility with 'Nano'
  --  coefficients
  maxCanonicalLovelaceVal :: Word64
  maxCanonicalLovelaceVal = 9e6

genChainDifficulty :: Gen ChainDifficulty
genChainDifficulty = ChainDifficulty <$> Gen.word64 Range.constantBounded

genCompactAddress :: Gen CompactAddress
genCompactAddress = toCompactAddress <$> genAddress

genCustomLovelace :: Word64 -> Gen Lovelace
genCustomLovelace size = genLovelaceWithRange (Range.linear 0 size)

genLovelace :: Gen Lovelace
genLovelace = genLovelaceWithRange (Range.constant 0 maxLovelaceVal)

genLovelaceError :: Gen LovelaceError
genLovelaceError = Gen.choice
  [ LovelaceOverflow <$> Gen.word64 overflowRange
  , LovelaceTooLarge <$> Gen.integral tooLargeRange
  , LovelaceTooSmall <$> Gen.integral tooSmallRange
  , uncurry LovelaceUnderflow <$> genUnderflowErrorValues
  ]
 where
  overflowRange :: Range Word64
  overflowRange = Range.constant (maxLovelaceVal + 1) (maxBound :: Word64)

  tooLargeRange :: Range Integer
  tooLargeRange = Range.constant (fromIntegral (maxLovelaceVal + 1))
                                 (fromIntegral (maxBound :: Word64))

  tooSmallRange :: Range Integer
  tooSmallRange = Range.constant (fromIntegral (minBound :: Int)) (- 1)

  genUnderflowErrorValues :: Gen (Word64, Word64)
  genUnderflowErrorValues = do
    a <- Gen.word64 (Range.constant 0 (maxBound - 1))
    b <- Gen.word64 (Range.constant a maxBound)
    pure (a, b)

genLovelaceWithRange :: Range Word64 -> Gen Lovelace
genLovelaceWithRange r =
  mkLovelace <$> Gen.word64 r >>= \case
    Right lovelace -> pure lovelace
    Left err ->
      panic $ sformat ("The impossible happened in genLovelace: " . build) err

genLovelacePortion :: Gen LovelacePortion
genLovelacePortion =
  LovelacePortion <$> Gen.word64 (Range.constant 0 lovelacePortionDenominator)

-- slow
genMerkleTree :: ToCBOR a => Gen a -> Gen (MerkleTree a)
genMerkleTree genA = mkMerkleTree <$> Gen.list (Range.linear 0 10) genA

-- slow
genMerkleRoot :: ToCBOR a => Gen a -> Gen (MerkleRoot a)
genMerkleRoot genA = mtRoot <$> genMerkleTree genA

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = Gen.choice
  [ pure NetworkMainOrStage
  , NetworkTestnet <$> Gen.word32 Range.constantBounded
  ]

genScriptVersion :: Gen Word16
genScriptVersion = Gen.word16 Range.constantBounded

genKeyHash :: Gen KeyHash
genKeyHash = hashKey <$> genVerificationKey

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy = TxFeePolicyTxSizeLinear <$> genTxSizeLinear

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genLovelace <*> genLovelace
