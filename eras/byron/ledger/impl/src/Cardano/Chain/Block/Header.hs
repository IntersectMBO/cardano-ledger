{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Header
  ( -- * Header
    Header,
    AHeader (..),

    -- * Header Constructors
    mkHeader,
    mkHeaderExplicit,

    -- * Header Accessors
    headerProtocolMagicId,
    headerPrevHash,
    headerProof,
    headerSlot,
    headerIssuer,
    headerLength,
    headerDifficulty,
    headerToSign,

    -- * Header Binary Serialization
    toCBORHeader,
    toCBORHeaderSize,
    toCBORHeaderToHash,
    fromCBORAHeader,
    fromCBORHeader,
    fromCBORHeaderToHash,
    wrapHeaderBytes,
    toCBORBlockVersions,
    toCBORBlockVersionsSize,

    -- * Header Formatting
    renderHeader,

    -- * Boundary Header
    ABoundaryHeader (..),
    mkABoundaryHeader,
    toCBORABoundaryHeader,
    toCBORABoundaryHeaderSize,
    fromCBORABoundaryHeader,
    boundaryHeaderHashAnnotated,
    wrapBoundaryBytes,

    -- * HeaderHash
    HeaderHash,
    headerHashF,
    hashHeader,
    headerHashAnnotated,
    genesisHeaderHash,

    -- * BlockSignature
    BlockSignature,
    ABlockSignature (..),

    -- * ToSign
    ToSign (..),
    recoverSignedBytes,
  )
where

import Cardano.Binary
  ( Annotated (..),
    ByteSpan,
    Case (..),
    Decoded (..),
    Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    Raw,
    Size,
    ToCBOR (..),
    annotatedDecoder,
    dropBytes,
    dropInt32,
    encodeListLen,
    enforceSize,
    fromCBORAnnotated,
    serializeEncoding,
    szCases,
    szGreedy,
  )
import Cardano.Chain.Block.Body (Body)
import Cardano.Chain.Block.Boundary
  ( dropBoundaryExtraHeaderDataRetainGenesisTag,
    fromCBORBoundaryConsensusData,
  )
import Cardano.Chain.Block.Proof (Proof (..), mkProof)
import Cardano.Chain.Common (ChainDifficulty (..), dropEmptyAttributes)
import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash (..))
import Cardano.Chain.Slotting
  ( EpochAndSlotCount (..),
    EpochSlots,
    SlotNumber (..),
    WithEpochSlots (WithEpochSlots),
    fromSlotNumber,
    toSlotNumber,
  )
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  ( Hash,
    ProtocolMagicId (..),
    SignTag (..),
    Signature,
    SigningKey,
    VerificationKey,
    hashDecoded,
    hashHexF,
    hashRaw,
    serializeCborHash,
    sign,
    unsafeAbstractHash,
  )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map (singleton)
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, int)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

type Header = AHeader ()

data AHeader a = AHeader
  { aHeaderProtocolMagicId :: !(Annotated ProtocolMagicId a),
    -- | Pointer to the header of the previous block
    aHeaderPrevHash :: !(Annotated HeaderHash a),
    -- | The slot number this block was published for
    aHeaderSlot :: !(Annotated SlotNumber a),
    -- | The chain difficulty up to this block
    aHeaderDifficulty :: !(Annotated ChainDifficulty a),
    -- | The version of the protocol parameters this block is using
    headerProtocolVersion :: !ProtocolVersion,
    -- | The software version this block was published from
    headerSoftwareVersion :: !SoftwareVersion,
    -- | Proof of body
    aHeaderProof :: !(Annotated Proof a),
    -- | The genesis key that is delegating to publish this block
    headerGenesisKey :: !VerificationKey,
    -- | The signature of the block, which contains the delegation certificate
    headerSignature :: !(ABlockSignature a),
    -- | An annotation that captures the full header bytes
    headerAnnotation :: !a,
    -- | An annotation that captures the bytes from the deprecated ExtraHeaderData
    headerExtraAnnotation :: !a
  }
  deriving (Eq, Show, Generic, NFData, Functor, NoThunks)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (AHeader a)

--------------------------------------------------------------------------------
-- Header Constructors
--------------------------------------------------------------------------------

-- | Smart constructor for 'Header'
mkHeader ::
  ProtocolMagicId ->
  Either GenesisHash Header ->
  -- | Number of slots per epoch. This is needed to convert the slot number to
  -- the legacy format used in 'ToSign', where a slot is identified by the
  -- epoch to which it belongs and the offset within that epoch (counted in
  -- number of slots).
  EpochSlots ->
  SlotNumber ->
  -- | The 'SigningKey' used for signing the block
  SigningKey ->
  -- | A certificate of delegation from a genesis key to the 'SigningKey'
  Delegation.Certificate ->
  Body ->
  ProtocolVersion ->
  SoftwareVersion ->
  Header
mkHeader pm prevHeader epochSlots =
  mkHeaderExplicit
    pm
    prevHash
    difficulty
    epochSlots
  where
    prevHash = either genesisHeaderHash (hashHeader epochSlots) prevHeader
    difficulty =
      either
        (const $ ChainDifficulty 0)
        (succ . headerDifficulty)
        prevHeader

-- | Make a 'Header' for a given slot, with a given body, parent hash,
--   and difficulty. This takes care of some signing and consensus data.
mkHeaderExplicit ::
  ProtocolMagicId ->
  -- | Parent
  HeaderHash ->
  ChainDifficulty ->
  -- | See 'mkHeader'.
  EpochSlots ->
  SlotNumber ->
  -- | The 'SigningKey' used for signing the block
  SigningKey ->
  -- | A certificate of delegation from a genesis key to the 'SigningKey'
  Delegation.Certificate ->
  Body ->
  ProtocolVersion ->
  SoftwareVersion ->
  Header
mkHeaderExplicit pm prevHash difficulty epochSlots slotNumber sk dlgCert body pv sv =
  AHeader
    (Annotated pm ())
    (Annotated prevHash ())
    (Annotated slotNumber ())
    (Annotated difficulty ())
    pv
    sv
    (Annotated proof ())
    genesisVK
    sig
    ()
    ()
  where
    proof = mkProof body

    genesisVK = Delegation.issuerVK dlgCert

    sig = ABlockSignature dlgCert $ sign pm (SignBlock genesisVK) sk toSign

    toSign = ToSign prevHash proof epochAndSlotCount difficulty pv sv

    epochAndSlotCount = fromSlotNumber epochSlots slotNumber

--------------------------------------------------------------------------------
-- Header Accessors
--------------------------------------------------------------------------------

headerProtocolMagicId :: AHeader a -> ProtocolMagicId
headerProtocolMagicId = unAnnotated . aHeaderProtocolMagicId

headerPrevHash :: AHeader a -> HeaderHash
headerPrevHash = unAnnotated . aHeaderPrevHash

headerSlot :: AHeader a -> SlotNumber
headerSlot = unAnnotated . aHeaderSlot

headerDifficulty :: AHeader a -> ChainDifficulty
headerDifficulty = unAnnotated . aHeaderDifficulty

headerProof :: AHeader a -> Proof
headerProof = unAnnotated . aHeaderProof

headerIssuer :: AHeader a -> VerificationKey
headerIssuer h = case headerSignature h of
  ABlockSignature cert _ -> Delegation.delegateVK cert

headerToSign :: EpochSlots -> AHeader a -> ToSign
headerToSign epochSlots h =
  ToSign
    (headerPrevHash h)
    (headerProof h)
    (fromSlotNumber epochSlots $ headerSlot h)
    (headerDifficulty h)
    (headerProtocolVersion h)
    (headerSoftwareVersion h)

headerLength :: AHeader ByteString -> Natural
headerLength = fromIntegral . BS.length . headerAnnotation

--------------------------------------------------------------------------------
-- Header Binary Serialization
--------------------------------------------------------------------------------

-- | Encode a header, without taking in to account deprecated epoch boundary
-- blocks.
toCBORHeader :: EpochSlots -> Header -> Encoding
toCBORHeader es h =
  encodeListLen 5
    <> toCBOR (headerProtocolMagicId h)
    <> toCBOR (headerPrevHash h)
    <> toCBOR (headerProof h)
    <> ( encodeListLen 4
           <> toCBOR (fromSlotNumber es $ headerSlot h)
           <> toCBOR (headerGenesisKey h)
           <> toCBOR (headerDifficulty h)
           <> toCBOR (headerSignature h)
       )
    <> toCBORBlockVersions (headerProtocolVersion h) (headerSoftwareVersion h)

toCBORHeaderSize :: Proxy EpochSlots -> Proxy (AHeader a) -> Size
toCBORHeaderSize es hdr =
  1 -- encodeListLen 5
    + szGreedy (headerProtocolMagicId <$> hdr)
    + szGreedy (headerPrevHash <$> hdr)
    + szGreedy (headerProof <$> hdr)
    + ( 1
          + szGreedy (fromSlotNumber <$> es <*> (headerSlot <$> hdr))
          + szGreedy (headerGenesisKey <$> hdr)
          + szGreedy (headerDifficulty <$> hdr)
          -- there is only 'ToCBOR' @ASignature ()@ instance, we
          -- must map 'a' to '()'
          + szGreedy (headerSignature . fmap (const ()) <$> hdr)
      )
    + toCBORBlockVersionsSize
      (headerProtocolVersion <$> hdr)
      (headerSoftwareVersion <$> hdr)

toCBORBlockVersions :: ProtocolVersion -> SoftwareVersion -> Encoding
toCBORBlockVersions pv sv =
  encodeListLen 4
    <> toCBOR pv
    <> toCBOR sv
    -- Encoding of empty Attributes
    <> toCBOR (mempty :: Map Word8 LByteString)
    -- Hash of the encoding of empty ExtraBodyData
    <> toCBOR (hashRaw "\129\160")

toCBORBlockVersionsSize :: Proxy ProtocolVersion -> Proxy SoftwareVersion -> Size
toCBORBlockVersionsSize pv sv =
  1
    + szGreedy pv
    + szGreedy sv
    -- empty attributes dictionary
    + 1
    + szGreedy (Proxy :: Proxy (Hash Raw))

fromCBORHeader :: EpochSlots -> Decoder s Header
fromCBORHeader epochSlots = void <$> fromCBORAHeader epochSlots

fromCBORAHeader :: EpochSlots -> Decoder s (AHeader ByteSpan)
fromCBORAHeader epochSlots = do
  Annotated
    ( pm,
      prevHash,
      proof,
      (slot, genesisKey, difficulty, sig),
      Annotated (protocolVersion, softwareVersion) extraByteSpan
      )
    byteSpan <-
    annotatedDecoder $ do
      enforceSize "Header" 5
      (,,,,)
        <$> fromCBORAnnotated
        <*> fromCBORAnnotated
        <*> fromCBORAnnotated
        <*> do
          enforceSize "ConsensusData" 4
          (,,,)
            -- Next, we decode a 'EpochAndSlotCount' into a 'SlotNumber': the `EpochAndSlotCount`
            -- used in 'AConsensusData' is encoded as a epoch and slot-count
            -- pair.
            <$> fmap (first (toSlotNumber epochSlots)) fromCBORAnnotated
            <*> fromCBOR
            <*> fromCBORAnnotated
            <*> fromCBOR
        <*> annotatedDecoder fromCBORBlockVersions
  pure $
    AHeader
      pm
      prevHash
      slot
      difficulty
      protocolVersion
      softwareVersion
      proof
      genesisKey
      sig
      byteSpan
      extraByteSpan

fromCBORBlockVersions :: Decoder s (ProtocolVersion, SoftwareVersion)
fromCBORBlockVersions = do
  enforceSize "BlockVersions" 4
  (,) <$> fromCBOR <*> fromCBOR <* dropEmptyAttributes <* dropBytes

instance Decoded (AHeader ByteString) where
  type BaseType (AHeader ByteString) = Header
  recoverBytes = headerAnnotation

-- | Encode a 'Header' accounting for deprecated epoch boundary blocks
--
--   This encoding is only used when hashing the header for backwards
--   compatibility, but should not be used when serializing a header within a
--   block
toCBORHeaderToHash :: EpochSlots -> Header -> Encoding
toCBORHeaderToHash epochSlots h =
  encodeListLen 2 <> toCBOR (1 :: Word) <> toCBORHeader epochSlots h

fromCBORHeaderToHash :: EpochSlots -> Decoder s (Maybe Header)
fromCBORHeaderToHash epochSlots = do
  enforceSize "Header" 2
  fromCBOR @Word >>= \case
    0 -> do
      void fromCBORABoundaryHeader
      pure Nothing
    1 -> Just <$!> fromCBORHeader epochSlots
    t -> cborError $ DecoderErrorUnknownTag "Header" (fromIntegral t)

--------------------------------------------------------------------------------
-- Header Formatting
--------------------------------------------------------------------------------

instance B.Buildable (WithEpochSlots Header) where
  build (WithEpochSlots es header) = renderHeader es header

renderHeader :: EpochSlots -> Header -> Builder
renderHeader es header =
  bprint
    ( "Header:\n"
        . "    hash: "
        . hashHexF
        . "\n"
        . "    previous block: "
        . hashHexF
        . "\n"
        . "    slot: "
        . build
        . "\n"
        . "    difficulty: "
        . int
        . "\n"
        . "    protocol: v"
        . build
        . "\n"
        . "    software: "
        . build
        . "\n"
        . "    genesis key: "
        . build
        . "\n"
        . "    signature: "
        . build
    )
    headerHash
    (headerPrevHash header)
    (headerSlot header)
    (unChainDifficulty $ headerDifficulty header)
    (headerProtocolVersion header)
    (headerSoftwareVersion header)
    (headerGenesisKey header)
    (headerSignature header)
  where
    headerHash :: HeaderHash
    headerHash = hashHeader es header

--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

-- | 'Hash' of block header
type HeaderHash = Hash Header

-- | Specialized formatter for 'HeaderHash'
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | Extract the genesis hash and cast it into a header hash.
genesisHeaderHash :: GenesisHash -> HeaderHash
genesisHeaderHash = coerce . unGenesisHash

-- | These bytes must be prepended when hashing raw boundary header data
--
--   In the Byron release, hashes were taken over a data type that was never
--   directly serialized to the blockchain, so these magic bytes cannot be
--   determined from the raw header data.
--
--   These bytes are from `encodeListLen 2 <> toCBOR (1 :: Word8)`
wrapHeaderBytes :: ByteString -> ByteString
wrapHeaderBytes = mappend "\130\SOH"

-- | Hash the serialised representation of a `Header`
--
--   For backwards compatibility we have to take the hash of the header
--   serialised with 'toCBORHeaderToHash'
hashHeader :: EpochSlots -> Header -> HeaderHash
hashHeader es = unsafeAbstractHash . serializeEncoding . toCBORHeaderToHash es

headerHashAnnotated :: AHeader ByteString -> HeaderHash
headerHashAnnotated = hashDecoded . fmap wrapHeaderBytes

--------------------------------------------------------------------------------
-- BoundaryHeader
--------------------------------------------------------------------------------

data ABoundaryHeader a = UnsafeABoundaryHeader
  { boundaryPrevHash :: !(Either GenesisHash HeaderHash),
    boundaryEpoch :: !Word64,
    boundaryDifficulty :: !ChainDifficulty,
    boundaryHeaderAnnotation :: !a
  }
  deriving (Eq, Show, Functor, Generic, NoThunks)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABoundaryHeader a)

-- | Smart constructor for 'ABoundaryHeader'
--
-- Makes sure that the hash is forced.
mkABoundaryHeader ::
  Either GenesisHash HeaderHash ->
  Word64 ->
  ChainDifficulty ->
  a ->
  ABoundaryHeader a
mkABoundaryHeader prevHash epoch dty ann =
  case prevHash of
    Left !genHash -> UnsafeABoundaryHeader (Left genHash) epoch dty ann
    Right !hdrHash -> UnsafeABoundaryHeader (Right hdrHash) epoch dty ann

instance Decoded (ABoundaryHeader ByteString) where
  type BaseType (ABoundaryHeader ByteString) = ABoundaryHeader ()
  recoverBytes = boundaryHeaderAnnotation

-- | Compute the hash of a boundary block header from its annotation.
-- It uses `wrapBoundaryBytes`, for the hash must be computed on the header
-- bytes tagged with the CBOR list length and tag discriminator, which is
-- the encoding chosen by cardano-sl.
boundaryHeaderHashAnnotated :: ABoundaryHeader ByteString -> HeaderHash
boundaryHeaderHashAnnotated = coerce . hashDecoded . fmap wrapBoundaryBytes

-- | Encode from a boundary header with any annotation. This does not
-- necessarily invert `fromCBORBoundaryHeader`, because that decoder drops
-- information that this encoder replaces, such as the body proof (assumes
-- the body is empty) and the extra header data (sets it to empty map).
toCBORABoundaryHeader :: ProtocolMagicId -> ABoundaryHeader a -> Encoding
toCBORABoundaryHeader pm hdr =
  encodeListLen 5
    <> toCBOR pm
    <> ( case boundaryPrevHash hdr of
           Left gh -> toCBOR (genesisHeaderHash gh)
           Right hh -> toCBOR hh
       )
    -- Body proof
    -- The body is always an empty slot leader schedule, so we hash that.
    <> toCBOR (serializeCborHash ([] :: [()]))
    -- Consensus data
    <> ( encodeListLen 2
           -- Epoch
           <> toCBOR (boundaryEpoch hdr)
           -- Chain difficulty
           <> toCBOR (boundaryDifficulty hdr)
       )
    -- Extra data
    <> ( encodeListLen 1
           <> toCBOR genesisTag
       )
  where
    -- Genesis tag to indicate the presence of a genesis hash in a non-zero
    -- epoch. See 'dropBoundaryExtraHeaderDataRetainGenesisTag' for more
    -- details on this.
    genesisTag = case (boundaryPrevHash hdr, boundaryEpoch hdr) of
      (Left _, n) | n > 0 -> Map.singleton 255 "Genesis"
      _ -> mempty :: Map Word8 LByteString

toCBORABoundaryHeaderSize :: Proxy ProtocolMagicId -> Proxy (ABoundaryHeader a) -> Size
toCBORABoundaryHeaderSize pm hdr =
  1
    + szGreedy pm
    + szCases
      [ Case "GenesisHash" $
          szGreedy $
            pFromLeft $
              boundaryPrevHash <$> hdr,
        Case "HeaderHash" $
          szGreedy $
            pFromRight $
              boundaryPrevHash <$> hdr
      ]
    -- Body proof
    + szGreedy (Proxy :: Proxy (Hash LByteString))
    -- Consensus data
    + ( 1
          + szGreedy (boundaryEpoch <$> hdr)
          + szGreedy (boundaryDifficulty <$> hdr)
      )
    -- Extra data
    + ( 1
          + szCases
            [ Case "Genesis" 11,
              Case "" 1
            ]
      )
  where
    pFromLeft :: Proxy (Either a b) -> Proxy a
    pFromLeft _ = Proxy

    pFromRight :: Proxy (Either a b) -> Proxy b
    pFromRight _ = Proxy

fromCBORABoundaryHeader :: Decoder s (ABoundaryHeader ByteSpan)
fromCBORABoundaryHeader = do
  Annotated header bytespan <- annotatedDecoder $ do
    enforceSize "BoundaryHeader" 5
    dropInt32
    -- HeaderHash
    hh <- fromCBOR
    -- BoundaryBodyProof
    dropBytes
    (epoch, difficulty) <- fromCBORBoundaryConsensusData
    isGen <- dropBoundaryExtraHeaderDataRetainGenesisTag
    let hh' = if epoch == 0 || isGen then Left (coerce hh) else Right hh
    pure $ mkABoundaryHeader hh' epoch difficulty ()
  pure (header {boundaryHeaderAnnotation = bytespan})

-- | These bytes must be prepended when hashing raw boundary header data
--
--   In the Byron release, hashes were taken over a data type that was never
--   directly serialized to the blockchain, so these magic bytes cannot be
--   determined from the raw header data.
wrapBoundaryBytes :: ByteString -> ByteString
wrapBoundaryBytes = mappend "\130\NUL"

--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

type BlockSignature = ABlockSignature ()

-- | Signature of the 'Block'
--
--   We use a heavyweight delegation scheme, so the signature has two parts:
--
--   1. A delegation certificate from a genesis key to the block signer
--   2. The actual signature over `ToSign`
data ABlockSignature a = ABlockSignature
  { delegationCertificate :: !(Delegation.ACertificate a),
    signature :: !(Signature ToSign)
  }
  deriving (Show, Eq, Generic, Functor)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable BlockSignature where
  build (ABlockSignature cert _) =
    bprint
      ( "BlockSignature:\n"
          . "  Delegation certificate: "
          . build
      )
      cert

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABlockSignature a)

instance ToCBOR BlockSignature where
  toCBOR (ABlockSignature cert sig) =
    -- Tag 0 was previously used for BlockSignature (no delegation)
    -- Tag 1 was previously used for BlockPSignatureLight
    encodeListLen 2
      <> toCBOR (2 :: Word8)
      <> (encodeListLen 2 <> toCBOR cert <> toCBOR sig)

  encodedSizeExpr size sig =
    3
      + encodedSizeExpr size (delegationCertificate <$> sig)
      + encodedSizeExpr size (signature <$> sig)

instance FromCBOR BlockSignature where
  fromCBOR = void <$> fromCBOR @(ABlockSignature ByteSpan)

instance FromCBOR (ABlockSignature ByteSpan) where
  fromCBOR = do
    enforceSize "BlockSignature" 2
    fromCBOR >>= \case
      2 ->
        ABlockSignature
          <$ enforceSize "BlockSignature" 2
          <*> fromCBOR
          <*> fromCBOR
      t -> cborError $ DecoderErrorUnknownTag "BlockSignature" t

--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

-- | Produces the ByteString that was signed in the block
recoverSignedBytes ::
  EpochSlots -> AHeader ByteString -> Annotated ToSign ByteString
recoverSignedBytes es h = Annotated (headerToSign es h) bytes
  where
    bytes =
      BS.concat
        [ "\133",
          -- This is the value of Codec.CBOR.Write.toLazyByteString (encodeListLen 5)
          -- It is hard coded here because the signed bytes included it as an
          -- implementation artifact
          (annotation . aHeaderPrevHash) h,
          (annotation . aHeaderProof) h,
          (annotation . aHeaderSlot) h,
          (annotation . aHeaderDifficulty) h,
          headerExtraAnnotation h
        ]

-- | Data to be signed in 'Block'
data ToSign = ToSign
  { -- | Hash of previous header in the chain
    tsHeaderHash :: !HeaderHash,
    tsBodyProof :: !Proof,
    tsSlot :: !EpochAndSlotCount,
    tsDifficulty :: !ChainDifficulty,
    tsProtocolVersion :: !ProtocolVersion,
    tsSoftwareVersion :: !SoftwareVersion
  }
  deriving (Eq, Show, Generic)

instance ToCBOR ToSign where
  toCBOR ts =
    encodeListLen 5
      <> toCBOR (tsHeaderHash ts)
      <> toCBOR (tsBodyProof ts)
      <> toCBOR (tsSlot ts)
      <> toCBOR (tsDifficulty ts)
      <> toCBORBlockVersions (tsProtocolVersion ts) (tsSoftwareVersion ts)

  encodedSizeExpr size ts =
    1
      + encodedSizeExpr size (tsHeaderHash <$> ts)
      + encodedSizeExpr size (tsBodyProof <$> ts)
      + encodedSizeExpr size (tsSlot <$> ts)
      + encodedSizeExpr size (tsDifficulty <$> ts)
      + toCBORBlockVersionsSize (tsProtocolVersion <$> ts) (tsSoftwareVersion <$> ts)

instance FromCBOR ToSign where
  fromCBOR = do
    enforceSize "ToSign" 5
    fmap uncurry (ToSign <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR)
      <*> fromCBORBlockVersions
