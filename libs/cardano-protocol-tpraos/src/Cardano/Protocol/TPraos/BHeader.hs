{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Protocol.TPraos.BHeader (
  HashHeader (..),
  PrevHash (..),
  BHeader (BHeader),
  BHBody (..),
  LastAppliedBlock (..),
  BoundedNatural (bvValue, bvMaxValue),
  assertBoundedNatural,
  lastAppliedHash,
  issuerIDfromBHBody,
  checkLeaderValue,
  checkLeaderNatValue,
  bhHash,
  hashHeaderToNonce,
  prevHashToNonce,
  bHeaderSize,
  bhbody,
  hBbsize,
  seedEta,
  seedL,
  mkSeed,
  bnonce,
  makeHeaderView,
)
where

import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  FixedPoint,
  Nonce (..),
  ProtVer (..),
  Seed (..),
  activeSlotLog,
  activeSlotVal,
  mkNonceFromNumber,
  mkNonceFromOutputVRF,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Binary (
  Annotator (..),
  Case (..),
  FromCBOR (fromCBOR),
  FromCBORGroup (..),
  ToCBOR (..),
  ToCBORGroup (..),
  TokenType (TypeNull),
  annotatorSlice,
  decodeNull,
  decodeRecordNamed,
  encodeListLen,
  encodeNull,
  encodedSigKESSizeExpr,
  encodedVerKeyVRFSizeExpr,
  hashToCBOR,
  listLenInt,
  peekTokenType,
  runByteBuilder,
  serialize',
  serializeEncoding,
  szCases,
  withWordSize,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes (
  EraIndependentBlockBody,
  EraIndependentBlockHeader,
 )
import Cardano.Ledger.Keys (
  CertifiedVRF,
  Hash,
  KeyHash,
  KeyRole (..),
  SignedKES,
  VKey,
  VerKeyVRF,
  decodeSignedKES,
  decodeVerKeyVRF,
  encodeSignedKES,
  encodeVerKeyVRF,
  hashKey,
 )
import Cardano.Ledger.NonIntegral (CompareResult (..), taylorExpCmp)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Protocol.TPraos.OCert (OCert (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Ratio ((%))
import Data.Typeable
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)

-- | The hash of a Block Header
newtype HashHeader c = HashHeader {unHashHeader :: Hash c EraIndependentBlockHeader}
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (NFData, NoThunks)

deriving newtype instance Crypto c => ToCBOR (HashHeader c)

-- | The previous hash of a block
data PrevHash c = GenesisHash | BlockHash !(HashHeader c)
  deriving (Show, Eq, Generic, Ord)

instance Crypto c => NoThunks (PrevHash c)

instance
  Crypto c =>
  ToCBOR (PrevHash c)
  where
  toCBOR GenesisHash = encodeNull
  toCBOR (BlockHash h) = toCBOR h
  encodedSizeExpr size _ =
    szCases
      [ Case "GenesisHash" 1
      , Case "BlockHash" (encodedSizeExpr size p)
      ]
    where
      p = Proxy :: Proxy (HashHeader c)

instance
  Crypto c =>
  FromCBOR (PrevHash c)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure GenesisHash
      _ -> BlockHash <$> fromCBOR

deriving newtype instance Crypto c => FromCBOR (HashHeader c)

data BHBody c = BHBody
  { bheaderBlockNo :: !BlockNo
  -- ^ block number
  , bheaderSlotNo :: !SlotNo
  -- ^ block slot
  , bheaderPrev :: !(PrevHash c)
  -- ^ Hash of the previous block header
  , bheaderVk :: !(VKey 'BlockIssuer c)
  -- ^ verification key of block issuer
  , bheaderVrfVk :: !(VerKeyVRF c)
  -- ^ VRF verification key for block issuer
  , bheaderEta :: !(CertifiedVRF c Nonce)
  -- ^ block nonce
  , bheaderL :: !(CertifiedVRF c Natural)
  -- ^ leader election value
  , bsize :: !Natural
  -- ^ Size of the block body
  , bhash :: !(Hash c EraIndependentBlockBody)
  -- ^ Hash of block body
  , bheaderOCert :: !(OCert c)
  -- ^ operational certificate
  , bprotver :: !ProtVer
  -- ^ protocol version
  }
  deriving (Generic)

deriving instance Crypto c => Show (BHBody c)

deriving instance Crypto c => Eq (BHBody c)

instance
  Crypto c =>
  SignableRepresentation (BHBody c)
  where
  getSignableRepresentation bh = serialize' (pvMajor (bprotver bh)) bh

instance
  Crypto c =>
  NoThunks (BHBody c)

instance
  Crypto c =>
  ToCBOR (BHBody c)
  where
  toCBOR bhBody =
    encodeListLen (9 + listLen oc + listLen pv)
      <> toCBOR (bheaderBlockNo bhBody)
      <> toCBOR (bheaderSlotNo bhBody)
      <> toCBOR (bheaderPrev bhBody)
      <> toCBOR (bheaderVk bhBody)
      <> encodeVerKeyVRF (bheaderVrfVk bhBody)
      <> toCBOR (bheaderEta bhBody)
      <> toCBOR (bheaderL bhBody)
      <> toCBOR (bsize bhBody)
      <> toCBOR (bhash bhBody)
      <> toCBORGroup oc
      <> toCBORGroup pv
    where
      oc = bheaderOCert bhBody
      pv = bprotver bhBody

  encodedSizeExpr size proxy =
    fromInteger (withWordSize $ 9 + listLenBound oc + listLenBound pv)
      + encodedSizeExpr size (bheaderBlockNo <$> proxy)
      + encodedSizeExpr size (bheaderSlotNo <$> proxy)
      + encodedSizeExpr size (bheaderPrev <$> proxy)
      + encodedSizeExpr size (bheaderVk <$> proxy)
      + encodedVerKeyVRFSizeExpr (bheaderVrfVk <$> proxy)
      + encodedSizeExpr size (bheaderEta <$> proxy)
      + encodedSizeExpr size (bheaderL <$> proxy)
      + encodedSizeExpr size (toWord64 . bsize <$> proxy)
      + encodedSizeExpr size (bhash <$> proxy)
      + encodedSizeExpr size (bheaderOCert <$> proxy)
      + encodedSizeExpr size (bprotver <$> proxy)
    where
      oc = bheaderOCert <$> proxy
      pv = bprotver <$> proxy
      toWord64 :: Natural -> Word64
      toWord64 = fromIntegral

instance
  Crypto c =>
  FromCBOR (BHBody c)
  where
  fromCBOR = decodeRecordNamed "BHBody" bhBodySize $ do
    bheaderBlockNo <- fromCBOR
    bheaderSlotNo <- fromCBOR
    bheaderPrev <- fromCBOR
    bheaderVk <- fromCBOR
    bheaderVrfVk <- decodeVerKeyVRF
    bheaderEta <- fromCBOR
    bheaderL <- fromCBOR
    bsize <- fromCBOR
    bhash <- fromCBOR
    bheaderOCert <- fromCBORGroup
    bprotver <- fromCBORGroup
    pure $
      BHBody
        { bheaderBlockNo
        , bheaderSlotNo
        , bheaderPrev
        , bheaderVk
        , bheaderVrfVk
        , bheaderEta
        , bheaderL
        , bsize
        , bhash
        , bheaderOCert
        , bprotver
        }
    where
      bhBodySize body = 9 + listLenInt (bheaderOCert body) + listLenInt (bprotver body)

data BHeader c = BHeader'
  { bHeaderBody' :: !(BHBody c)
  , bHeaderSig' :: !(SignedKES c (BHBody c))
  , bHeaderBytes :: !BSL.ByteString
  }
  deriving (Generic)

deriving via
  AllowThunksIn '["bHeaderBytes"] (BHeader c)
  instance
    Crypto c => NoThunks (BHeader c)

deriving instance Crypto c => Eq (BHeader c)

deriving instance Crypto c => Show (BHeader c)

pattern BHeader ::
  Crypto c =>
  BHBody c ->
  SignedKES c (BHBody c) ->
  BHeader c
pattern BHeader bHeaderBody' bHeaderSig' <-
  BHeader' {bHeaderBody', bHeaderSig'}
  where
    BHeader body sig =
      let mkBytes bhBody kESig =
            serializeEncoding (pvMajor (bprotver bhBody)) $
              encodeListLen 2
                <> toCBOR bhBody
                <> encodeSignedKES kESig
       in BHeader' body sig (mkBytes body sig)

{-# COMPLETE BHeader #-}

instance Crypto c => Plain.EncCBOR (BHeader c) where
  encCBOR (BHeader' _ _ bytes) = Plain.encodePreEncoded (BSL.toStrict bytes)

instance Crypto c => ToCBOR (BHeader c) where
  encodedSizeExpr size proxy =
    1
      + encodedSizeExpr size (bHeaderBody' <$> proxy)
      + encodedSigKESSizeExpr (KES.getSig . bHeaderSig' <$> proxy)

instance
  Crypto c =>
  FromCBOR (Annotator (BHeader c))
  where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Header" (const 2) $ do
      bhb <- fromCBOR
      sig <- decodeSignedKES
      pure $ pure $ BHeader' bhb sig

-- | Hash a given block header
bhHash ::
  Crypto c =>
  BHeader c ->
  HashHeader c
bhHash bh = HashHeader . Hash.castHash . hashToCBOR version $ bh
  where
    version = pvMajor (bprotver (bHeaderBody' bh))

-- | HashHeader to Nonce
-- What is going on here?
-- This is here because the surrounding code is parametrized in the hash algorithm used,
-- but the nonce is hard-coded to Blake2b_256.
-- We require the nonce to have the right length (the size of a Blake2b_256 hash), so
-- if the hash size differs, we pad or remove bytes accordingly.
hashHeaderToNonce :: HashHeader c -> Nonce
hashHeaderToNonce (HashHeader h) = case Hash.hashFromBytes bytes of
  Nothing -> Nonce (Hash.castHash (Hash.hashWith id bytes))
  Just hash -> Nonce $! hash
  where
    bytes = Hash.hashToBytes h

prevHashToNonce ::
  PrevHash c ->
  Nonce
prevHashToNonce = \case
  GenesisHash -> NeutralNonce -- This case can only happen when starting Shelley from genesis,
  -- setting the intial chain state to some epoch e,
  -- and having the first block be in epoch e+1.
  -- In this edge case there is no need to add any extra
  -- entropy via the previous header hash to the next epoch nonce,
  -- so using the neutral nonce is appropriate.
  BlockHash ph -> hashHeaderToNonce ph

-- | Retrieve the issuer id (the hash of the cold key) from the body of the block header.
-- This corresponds to either a genesis/core node or a stake pool.
issuerIDfromBHBody :: Crypto c => BHBody c -> KeyHash 'BlockIssuer c
issuerIDfromBHBody = hashKey . bheaderVk

bHeaderSize :: forall c. BHeader c -> Int
bHeaderSize = fromIntegral . BSL.length . bHeaderBytes

bhbody ::
  Crypto c =>
  BHeader c ->
  BHBody c
bhbody (BHeader b _) = b

hBbsize :: BHBody c -> Natural
hBbsize = bsize

-- | Natural value with some additional bound. It must always be the base that
-- 'bvValue <= bvMaxValue'. The creator is responsible for checking this value.
data BoundedNatural = UnsafeBoundedNatural
  { bvMaxValue :: Natural
  , bvValue :: Natural
  }

-- | Assert that a natural is bounded by a certain value. Throws an error when
-- this is not the case.
assertBoundedNatural ::
  -- | Maximum bound
  Natural ->
  -- | Value
  Natural ->
  BoundedNatural
assertBoundedNatural maxVal val =
  if val <= maxVal
    then UnsafeBoundedNatural maxVal val
    else error $ show val <> " is greater than max value " <> show maxVal

-- | Check that the certified VRF output, when used as a natural, is valid for
-- being slot leader.
checkLeaderValue ::
  forall v.
  (VRF.VRFAlgorithm v) =>
  VRF.OutputVRF v ->
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderValue certVRF σ f =
  checkLeaderNatValue (assertBoundedNatural certNatMax (VRF.getOutputVRFNatural certVRF)) σ f
  where
    certNatMax :: Natural
    certNatMax = (2 :: Natural) ^ (8 * VRF.sizeOutputVRF certVRF)

-- | Check that the certified input natural is valid for being slot leader. This
-- means we check that
--
-- p < 1 - (1 - f)^σ
--
-- where p = certNat / certNatMax.
--
-- The calculation is done using the following optimization:
--
-- let q = 1 - p and c = ln(1 - f)
--
-- then           p < 1 - (1 - f)^σ
-- <=>  1 / (1 - p) < exp(-σ * c)
-- <=>  1 / q       < exp(-σ * c)
--
-- This can be efficiently be computed by `taylorExpCmp` which returns `ABOVE`
-- in case the reference value `1 / (1 - p)` is above the exponential function
-- at `-σ * c`, `BELOW` if it is below or `MaxReached` if it couldn't
-- conclusively compute this within the given iteration bounds.
--
-- Note that  1       1               1                         certNatMax
--           --- =  ----- = ---------------------------- = ----------------------
--            q     1 - p    1 - (certNat / certNatMax)    (certNatMax - certNat)
checkLeaderNatValue ::
  -- | Certified nat value
  BoundedNatural ->
  -- | Stake proportion
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderNatValue bn σ f =
  if activeSlotVal f == maxBound
    then -- If the active slot coefficient is equal to one,
    -- then nearly every stake pool can produce a block every slot.
    -- In this degenerate case, where ln (1-f) is not defined,
    -- we let the VRF leader check always succeed.
    -- This is a testing convenience, the active slot coefficient should not
    -- bet set above one half otherwise.
      True
    else case taylorExpCmp 3 recip_q x of
      ABOVE _ _ -> False
      BELOW _ _ -> True
      MaxReached _ -> False
  where
    c, recip_q, x :: FixedPoint
    c = activeSlotLog f
    recip_q = fromRational (toInteger certNatMax % toInteger (certNatMax - certNat))
    x = -fromRational σ * c
    certNatMax = bvMaxValue bn
    certNat = bvValue bn

seedEta :: Nonce
seedEta = mkNonceFromNumber 0

seedL :: Nonce
seedL = mkNonceFromNumber 1

-- | Construct a seed to use in the VRF computation.
mkSeed ::
  -- | Universal constant
  Nonce ->
  SlotNo ->
  -- | Epoch nonce
  Nonce ->
  Seed
mkSeed ucNonce (SlotNo slot) eNonce =
  Seed
    . ( case ucNonce of
          NeutralNonce -> id
          Nonce h -> Hash.xor (Hash.castHash h)
      )
    . Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ BS.word64BE slot
      <> ( case eNonce of
            NeutralNonce -> mempty
            Nonce h -> BS.byteStringCopy (Hash.hashToBytes h)
         )

data LastAppliedBlock c = LastAppliedBlock
  { labBlockNo :: !BlockNo
  , labSlotNo :: !SlotNo
  , labHash :: !(HashHeader c)
  }
  deriving (Show, Eq, Generic)

instance Crypto c => NoThunks (LastAppliedBlock c)

instance NFData (LastAppliedBlock c)

instance Crypto c => ToCBOR (LastAppliedBlock c) where
  toCBOR (LastAppliedBlock b s h) =
    encodeListLen 3 <> toCBOR b <> toCBOR s <> toCBOR h

instance Crypto c => FromCBOR (LastAppliedBlock c) where
  fromCBOR =
    decodeRecordNamed
      "lastAppliedBlock"
      (const 3)
      ( LastAppliedBlock
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

lastAppliedHash :: WithOrigin (LastAppliedBlock c) -> PrevHash c
lastAppliedHash Origin = GenesisHash
lastAppliedHash (At lab) = BlockHash $ labHash lab

-- | Retrieve the new nonce from the block header body.
bnonce :: BHBody c -> Nonce
bnonce = mkNonceFromOutputVRF . VRF.certifiedOutput . bheaderEta

makeHeaderView :: Crypto c => BHeader c -> BHeaderView c
makeHeaderView bh =
  BHeaderView
    (hashKey . bheaderVk $ bhb)
    (bsize $ bhb)
    (bHeaderSize bh)
    (bhash bhb)
    (bheaderSlotNo bhb)
  where
    bhb = bHeaderBody' bh
