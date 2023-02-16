{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Block.Size (
  tests,
)
where

import Cardano.Chain.Block
import Cardano.Ledger.Binary hiding (label)
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
          size = fromIntegral $ BS.length (serialize' byronProtVer (encode a))
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

encodedSizeTestEncCBOR ::
  forall a.
  (EncCBOR a, Show a) =>
  Gen a ->
  TSProperty
encodedSizeTestEncCBOR =
  encodedSizeTest encCBOR szGreedy

ts_prop_sizeProtocolMagicId :: TSProperty
ts_prop_sizeProtocolMagicId =
  encodedSizeTestEncCBOR Crypto.genProtocolMagicId

ts_prop_sizeEpochAndSlotCount :: TSProperty
ts_prop_sizeEpochAndSlotCount =
  encodedSizeTestEncCBOR (Slotting.genEpochSlots >>= Slotting.genEpochAndSlotCount)

ts_prop_sizeChainDifficulty :: TSProperty
ts_prop_sizeChainDifficulty = encodedSizeTestEncCBOR genChainDifficulty

ts_prop_sizeHeaderHash :: TSProperty
ts_prop_sizeHeaderHash = encodedSizeTestEncCBOR genHeaderHash

ts_prop_sizeSlotNumber :: TSProperty
ts_prop_sizeSlotNumber = encodedSizeTestEncCBOR Slotting.genSlotNumber

ts_prop_sizeProtocolVersion :: TSProperty
ts_prop_sizeProtocolVersion = encodedSizeTestEncCBOR Update.genProtocolVersion

ts_prop_sizeApplicationName :: TSProperty
ts_prop_sizeApplicationName = encodedSizeTestEncCBOR Update.genApplicationName

ts_prop_sizeSoftwareVersion :: TSProperty
ts_prop_sizeSoftwareVersion = encodedSizeTestEncCBOR Update.genSoftwareVersion

ts_prop_sizeProof :: TSProperty
ts_prop_sizeProof = encodedSizeTestEncCBOR (Crypto.genProtocolMagicId >>= genProof)

ts_prop_sizeVerificationKey :: TSProperty
ts_prop_sizeVerificationKey = encodedSizeTestEncCBOR Crypto.genVerificationKey

ts_prop_sizeToSign :: TSProperty
ts_prop_sizeToSign =
  encodedSizeTestEncCBOR $
    ((,) <$> Crypto.genProtocolMagicId <*> Slotting.genEpochSlots)
      >>= uncurry genToSign

ts_prop_sizeBlockVersions :: TSProperty
ts_prop_sizeBlockVersions =
  encodedSizeTest
    (uncurry encCBORBlockVersions)
    (uncurryP encCBORBlockVersionsSize)
    ((,) <$> Update.genProtocolVersion <*> Update.genSoftwareVersion)

ts_prop_sizeEpochNumber :: TSProperty
ts_prop_sizeEpochNumber =
  encodedSizeTestEncCBOR Slotting.genEpochNumber

-- | test @Signature EpochNumber@ which is a part of 'ACertificate'
ts_prop_sizeEpochNumberSignature :: TSProperty
ts_prop_sizeEpochNumberSignature =
  encodedSizeTestEncCBOR $
    Crypto.genProtocolMagicId >>= flip Crypto.genSignature Slotting.genEpochSlots

ts_prop_sizeToSignSignature :: TSProperty
ts_prop_sizeToSignSignature =
  encodedSizeTestEncCBOR $
    do
      pm <- Crypto.genProtocolMagicId
      es <- Slotting.genEpochSlots
      Crypto.genSignature pm (genToSign pm es)

ts_prop_sizeBlockSignature :: TSProperty
ts_prop_sizeBlockSignature =
  encodedSizeTestEncCBOR $
    ((,) <$> Crypto.genProtocolMagicId <*> Slotting.genEpochSlots)
      >>= uncurry genBlockSignature

--
-- Header
--

ts_prop_sizeHeader :: TSProperty
ts_prop_sizeHeader =
  encodedSizeTest
    (uncurry encCBORHeader)
    (uncurryP encCBORHeaderSize)
    $ do
      protocolMagicId <- Crypto.genProtocolMagicId
      epochSlots <- Slotting.genEpochSlots
      header <- genHeader protocolMagicId epochSlots
      return (epochSlots, header)

--
-- ABoundaryHeader
--

ts_prop_sizeGenesisHash :: TSProperty
ts_prop_sizeGenesisHash = encodedSizeTestEncCBOR Genesis.genGenesisHash

ts_prop_sizeABoundaryHeader :: TSProperty
ts_prop_sizeABoundaryHeader =
  encodedSizeTest
    (uncurry encCBORABoundaryHeader)
    (uncurryP encCBORABoundaryHeaderSize)
    ( (,)
        <$> Crypto.genProtocolMagicId
        <*> genBoundaryHeader
    )

--
-- ABlockOrBoundaryHdr
--

ts_prop_sizeABlockOrBoundaryHdr :: TSProperty
ts_prop_sizeABlockOrBoundaryHdr =
  encodedSizeTest
    encCBORABlockOrBoundaryHdr
    encCBORABlockOrBoundaryHdrSize
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
