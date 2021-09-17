{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Common.Gen
  ( genAddrAttributes,
    genAddrAttributesWithNM,
    genAddress,
    genAddressWithNM,
    genAddrType,
    genAddrSpendingData,
    genAttributes,
    genBlockCount,
    genCanonicalTxFeePolicy,
    genChainDifficulty,
    genCompactAddress,
    genCustomLovelace,
    genLovelace,
    genLovelaceError,
    genLovelaceWithRange,
    genLovelacePortion,
    genMerkleRoot,
    genMerkleTree,
    genNetworkMagic,
    genScriptVersion,
    genKeyHash,
    genTxFeePolicy,
    genTxSizeLinear,
  )
where

import Cardano.Binary (ToCBOR)
import Cardano.Chain.Common
  ( AddrAttributes (..),
    AddrSpendingData (..),
    AddrType (..),
    Address (..),
    Attributes,
    BlockCount (..),
    ChainDifficulty (..),
    CompactAddress,
    HDAddressPayload (..),
    KeyHash,
    Lovelace,
    LovelaceError (..),
    LovelacePortion,
    MerkleRoot (..),
    MerkleTree,
    NetworkMagic (..),
    TxFeePolicy (..),
    TxSizeLinear (..),
    hashKey,
    makeAddress,
    maxLovelaceVal,
    mkAttributes,
    mkLovelace,
    mkMerkleTree,
    mtRoot,
    rationalToLovelacePortion,
    toCompactAddress,
  )
import Cardano.Prelude
import Formatting (build, sformat)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Crypto.Gen (genRedeemVerificationKey, genVerificationKey)
import Test.Cardano.Prelude (gen32Bytes)

genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = genAddrAttributesWithNM =<< genNetworkMagic

genAddrAttributesWithNM :: NetworkMagic -> Gen AddrAttributes
genAddrAttributesWithNM nm = AddrAttributes <$> hap <*> pure nm
  where
    hap = Gen.maybe genHDAddressPayload

genHDAddressPayload :: Gen HDAddressPayload
genHDAddressPayload = HDAddressPayload <$> gen32Bytes

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddressWithNM :: NetworkMagic -> Gen Address
genAddressWithNM nm =
  makeAddress <$> genAddrSpendingData
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
genCanonicalTxSizeLinear = TxSizeLinear <$> genLovelace' <*> genMultiplier
  where
    genLovelace' :: Gen Lovelace
    genLovelace' =
      mkLovelace
        <$> Gen.word64 (Range.constant 0 maxCanonicalLovelaceVal)
        >>= \case
          Right lovelace -> pure lovelace
          Left err ->
            panic $
              sformat
                ("The impossible happened in genLovelace: " . build)
                err

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
genLovelaceError =
  Gen.choice
    [ LovelaceOverflow <$> Gen.word64 overflowRange,
      LovelaceTooLarge <$> Gen.integral tooLargeRange,
      LovelaceTooSmall <$> Gen.integral tooSmallRange,
      uncurry LovelaceUnderflow <$> genUnderflowErrorValues
    ]
  where
    overflowRange :: Range Word64
    overflowRange = Range.constant (maxLovelaceVal + 1) (maxBound :: Word64)

    tooLargeRange :: Range Integer
    tooLargeRange =
      Range.constant
        (fromIntegral (maxLovelaceVal + 1))
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
  rationalToLovelacePortion . realToFrac <$> Gen.double (Range.constant 0 1)

-- slow
genMerkleTree :: ToCBOR a => Gen a -> Gen (MerkleTree a)
genMerkleTree genA = mkMerkleTree <$> Gen.list (Range.linear 0 10) genA

-- slow
genMerkleRoot :: ToCBOR a => Gen a -> Gen (MerkleRoot a)
genMerkleRoot genA = mtRoot <$> genMerkleTree genA

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
  Gen.choice
    [ pure NetworkMainOrStage,
      NetworkTestnet <$> Gen.word32 Range.constantBounded
    ]

genScriptVersion :: Gen Word16
genScriptVersion = Gen.word16 Range.constantBounded

genKeyHash :: Gen KeyHash
genKeyHash = hashKey <$> genVerificationKey

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy = TxFeePolicyTxSizeLinear <$> genTxSizeLinear

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genLovelace <*> genMultiplier

-- | Generate multipliers for the TxSizeLinear.
genMultiplier :: Gen Rational
genMultiplier = fromIntegral <$> Gen.word16 (Range.constant 0 1000)
