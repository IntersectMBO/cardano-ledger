{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Block.Size
  ( tests,
  )
where

import Cardano.Binary hiding (label)
import Cardano.Chain.Block
import Cardano.Prelude
import qualified Data.ByteString as BS
import Data.String (IsString (..))
import Hedgehog (Gen, LabelName, failure, footnote, label, success)
import Test.Cardano.Chain.Block.Gen
import Test.Cardano.Chain.Common.Gen
import qualified Test.Cardano.Chain.Genesis.Gen as Genesis
import qualified Test.Cardano.Chain.Slotting.Gen as Slotting
import qualified Test.Cardano.Chain.Update.Gen as Update
import qualified Test.Cardano.Crypto.Gen as Crypto
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, eachOfTS)

encodedSizeTest ::
  forall a.
  Show a =>
  (a -> Encoding) ->
  (Proxy a -> Size) ->
  Gen a ->
  TSProperty
encodedSizeTest encode encodedSize gen = eachOfTS
  300
  gen
  $ \a -> case szSimplify (encodedSize (Proxy :: Proxy a)) of
    Right rng@Range {lo, hi} ->
      let size :: Natural
          size = fromIntegral $ BS.length (toStrictByteString (encode a))
       in if
              | size < lo -> do
                  footnote $ "actual size not greater or equal the minimal size: " ++ show size ++ " ≱ " ++ show lo
                  failure
              | size > hi -> do
                  footnote $ "actual size not smaller or equal the maximal size: " ++ show size ++ " ≰ " ++ show hi
                  failure
              | otherwise -> do
                  label (classifySize rng size)
                  success
    Left _ -> do
      footnote "a thunk in size expression"
      failure
  where
    -- we can assume that lo ≤ size ≤ hi
    classifySize :: Range Natural -> Natural -> LabelName
    classifySize Range {lo, hi} size =
      fromString $
        "lo: "
          ++ show lo
          ++ " hi: "
          ++ show hi
          ++ " size: "
          ++ show s
          ++ " - "
          ++ show (s + bucket)
      where
        bucket =
          if hi - lo >= 5
            then (hi - lo) `div` 5
            else 1
        s = lo + bucket * ((size - lo) `div` bucket)

encodedSizeTestToCBOR ::
  forall a.
  (ToCBOR a, Show a) =>
  Gen a ->
  TSProperty
encodedSizeTestToCBOR =
  encodedSizeTest toCBOR szGreedy

ts_prop_sizeProtocolMagicId :: TSProperty
ts_prop_sizeProtocolMagicId =
  encodedSizeTestToCBOR Crypto.genProtocolMagicId

ts_prop_sizeEpochAndSlotCount :: TSProperty
ts_prop_sizeEpochAndSlotCount =
  encodedSizeTestToCBOR (Slotting.genEpochSlots >>= Slotting.genEpochAndSlotCount)

ts_prop_sizeChainDifficulty :: TSProperty
ts_prop_sizeChainDifficulty = encodedSizeTestToCBOR genChainDifficulty

ts_prop_sizeHeaderHash :: TSProperty
ts_prop_sizeHeaderHash = encodedSizeTestToCBOR genHeaderHash

ts_prop_sizeSlotNumber :: TSProperty
ts_prop_sizeSlotNumber = encodedSizeTestToCBOR Slotting.genSlotNumber

ts_prop_sizeProtocolVersion :: TSProperty
ts_prop_sizeProtocolVersion = encodedSizeTestToCBOR Update.genProtocolVersion

ts_prop_sizeApplicationName :: TSProperty
ts_prop_sizeApplicationName = encodedSizeTestToCBOR Update.genApplicationName

ts_prop_sizeSoftwareVersion :: TSProperty
ts_prop_sizeSoftwareVersion = encodedSizeTestToCBOR Update.genSoftwareVersion

ts_prop_sizeProof :: TSProperty
ts_prop_sizeProof = encodedSizeTestToCBOR (Crypto.genProtocolMagicId >>= genProof)

ts_prop_sizeVerificationKey :: TSProperty
ts_prop_sizeVerificationKey = encodedSizeTestToCBOR Crypto.genVerificationKey

ts_prop_sizeToSign :: TSProperty
ts_prop_sizeToSign =
  encodedSizeTestToCBOR $
    ((,) <$> Crypto.genProtocolMagicId <*> Slotting.genEpochSlots)
      >>= uncurry genToSign

ts_prop_sizeBlockVersions :: TSProperty
ts_prop_sizeBlockVersions =
  encodedSizeTest
    (uncurry toCBORBlockVersions)
    (uncurryP toCBORBlockVersionsSize)
    ((,) <$> Update.genProtocolVersion <*> Update.genSoftwareVersion)

ts_prop_sizeEpochNumber :: TSProperty
ts_prop_sizeEpochNumber =
  encodedSizeTestToCBOR Slotting.genEpochNumber

-- | test @Signature EpochNumber@ which is a part of 'ACertificate'
ts_prop_sizeEpochNumberSignature :: TSProperty
ts_prop_sizeEpochNumberSignature =
  encodedSizeTestToCBOR $
    Crypto.genProtocolMagicId >>= flip Crypto.genSignature Slotting.genEpochSlots

ts_prop_sizeToSignSignature :: TSProperty
ts_prop_sizeToSignSignature =
  encodedSizeTestToCBOR $
    do
      pm <- Crypto.genProtocolMagicId
      es <- Slotting.genEpochSlots
      Crypto.genSignature pm (genToSign pm es)

ts_prop_sizeBlockSignature :: TSProperty
ts_prop_sizeBlockSignature =
  encodedSizeTestToCBOR $
    ((,) <$> Crypto.genProtocolMagicId <*> Slotting.genEpochSlots)
      >>= uncurry genBlockSignature

--
-- Header
--

ts_prop_sizeHeader :: TSProperty
ts_prop_sizeHeader =
  encodedSizeTest
    (uncurry toCBORHeader)
    (uncurryP toCBORHeaderSize)
    $ do
      protocolMagicId <- Crypto.genProtocolMagicId
      epochSlots <- Slotting.genEpochSlots
      header <- genHeader protocolMagicId epochSlots
      return (epochSlots, header)

--
-- ABoundaryHeader
--

ts_prop_sizeGenesisHash :: TSProperty
ts_prop_sizeGenesisHash = encodedSizeTestToCBOR Genesis.genGenesisHash

ts_prop_sizeABoundaryHeader :: TSProperty
ts_prop_sizeABoundaryHeader =
  encodedSizeTest
    (uncurry toCBORABoundaryHeader)
    (uncurryP toCBORABoundaryHeaderSize)
    ( (,) <$> Crypto.genProtocolMagicId
        <*> genBoundaryHeader
    )

--
-- ABlockOrBoundaryHdr
--

ts_prop_sizeABlockOrBoundaryHdr :: TSProperty
ts_prop_sizeABlockOrBoundaryHdr =
  encodedSizeTest
    toCBORABlockOrBoundaryHdr
    toCBORABlockOrBoundaryHdrSize
    $ ((,) <$> Crypto.genProtocolMagicId <*> Slotting.genEpochSlots)
      >>= uncurry genABlockOrBoundaryHdr

--
-- Utils
--

uncurryP :: (Proxy a -> Proxy b -> c) -> Proxy (a, b) -> c
uncurryP f p = f (fst <$> p) (snd <$> p)

--
-- Tests
--

tests :: TSGroup
tests = $$discoverPropArg
