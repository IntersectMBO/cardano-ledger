{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Chain.Block.Block
  ( -- * Block
    Block,
    ABlock (..),

    -- * Block Constructors
    mkBlock,
    mkBlockExplicit,

    -- * Block Accessors
    blockHash,
    blockHashAnnotated,
    blockAProtocolMagicId,
    blockProtocolMagicId,
    blockPrevHash,
    blockProof,
    blockSlot,
    blockGenesisKey,
    blockIssuer,
    blockDifficulty,
    blockToSign,
    blockSignature,
    blockProtocolVersion,
    blockSoftwareVersion,
    blockTxPayload,
    blockSscPayload,
    blockDlgPayload,
    blockUpdatePayload,
    blockLength,

    -- * Block Binary Serialization
    toCBORBlock,
    fromCBORABlock,

    -- * Block Formatting
    renderBlock,

    -- * ABlockOrBoundary
    ABlockOrBoundary (..),
    toCBORABOBBlock,
    fromCBORABOBBlock,
    fromCBORABlockOrBoundary,
    toCBORABlockOrBoundary,

    -- * ABoundaryBlock
    ABoundaryBlock (..),
    boundaryHashAnnotated,
    fromCBORABoundaryBlock,
    toCBORABoundaryBlock,
    toCBORABOBBoundary,
    boundaryBlockSlot,
    ABoundaryBody (..),

    -- * ABlockOrBoundaryHdr
    ABlockOrBoundaryHdr (..),
    aBlockOrBoundaryHdr,
    fromCBORABlockOrBoundaryHdr,
    toCBORABlockOrBoundaryHdr,
    toCBORABlockOrBoundaryHdrSize,
    abobHdrFromBlock,
    abobHdrSlotNo,
    abobHdrChainDifficulty,
    abobHdrHash,
    abobHdrPrevHash,
  )
where

-- TODO `contramap` should be in `Cardano.Prelude`

import Cardano.Binary
  ( Annotated (..),
    ByteSpan (..),
    Case (..),
    Decoded (..),
    Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    Size,
    ToCBOR (..),
    annotatedDecoder,
    encodeBreak,
    encodeListLen,
    encodeListLenIndef,
    enforceSize,
    szCases,
  )
import Cardano.Chain.Block.Body
  ( ABody,
    Body,
    bodyDlgPayload,
    bodySscPayload,
    bodyTxPayload,
    bodyTxs,
    bodyUpdatePayload,
  )
import Cardano.Chain.Block.Boundary
  ( dropBoundaryBody,
    dropBoundaryExtraBodyData,
  )
import Cardano.Chain.Block.Header
  ( ABlockSignature,
    ABoundaryHeader (..),
    AHeader (..),
    Header,
    HeaderHash,
    ToSign,
    boundaryHeaderHashAnnotated,
    fromCBORABoundaryHeader,
    fromCBORAHeader,
    genesisHeaderHash,
    hashHeader,
    headerDifficulty,
    headerGenesisKey,
    headerHashAnnotated,
    headerIssuer,
    headerPrevHash,
    headerProof,
    headerProtocolMagicId,
    headerProtocolVersion,
    headerSignature,
    headerSlot,
    headerSoftwareVersion,
    headerToSign,
    mkHeaderExplicit,
    toCBORABoundaryHeader,
    toCBORABoundaryHeaderSize,
    toCBORHeader,
    toCBORHeaderSize,
  )
import Cardano.Chain.Block.Proof (Proof (..))
import Cardano.Chain.Common (ChainDifficulty (..), dropEmptyAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash (..))
import Cardano.Chain.Slotting
  ( EpochSlots (..),
    SlotNumber (..),
    WithEpochSlots (WithEpochSlots),
  )
import Cardano.Chain.Ssc (SscPayload)
import Cardano.Chain.UTxO.TxPayload (ATxPayload)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto (ProtocolMagicId, SigningKey, VerificationKey)
import Cardano.Prelude
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad.Fail (fail)
import Control.Tracer (contramap)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder (Builder, fromText)
import Formatting (bprint, build, int, later, shown)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

type Block = ABlock ()

data ABlock a = ABlock
  { blockHeader :: AHeader a,
    blockBody :: ABody a,
    blockAnnotation :: a
  }
  deriving (Eq, Show, Generic, NFData, Functor)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABlock a)

--------------------------------------------------------------------------------
-- Block Constructors
--------------------------------------------------------------------------------

-- | Smart constructor for 'Block'
mkBlock ::
  ProtocolMagicId ->
  ProtocolVersion ->
  SoftwareVersion ->
  Either GenesisHash Header ->
  EpochSlots ->
  SlotNumber ->
  -- | The 'SigningKey' used for signing the block
  SigningKey ->
  -- | A certificate of delegation from a genesis key to the 'SigningKey'
  Delegation.Certificate ->
  Body ->
  Block
mkBlock pm bv sv prevHeader epochSlots =
  mkBlockExplicit
    pm
    bv
    sv
    prevHash
    difficulty
    epochSlots
  where
    prevHash = either genesisHeaderHash (hashHeader epochSlots) prevHeader
    difficulty =
      either (const $ ChainDifficulty 0) (succ . headerDifficulty) prevHeader

-- | Smart constructor for 'Block', without requiring the entire previous
--   'Header'. Instead, you give its hash and the difficulty of this block.
--   These are derived from the previous header in 'mkBlock' so if you have
--   the previous header, consider using that one.
mkBlockExplicit ::
  ProtocolMagicId ->
  ProtocolVersion ->
  SoftwareVersion ->
  HeaderHash ->
  ChainDifficulty ->
  EpochSlots ->
  SlotNumber ->
  -- | The 'SigningKey' used for signing the block
  SigningKey ->
  -- | A certificate of delegation from a genesis key to the 'SigningKey'
  Delegation.Certificate ->
  Body ->
  Block
mkBlockExplicit pm pv sv prevHash difficulty epochSlots slotNumber sk dlgCert body =
  ABlock
    ( mkHeaderExplicit
        pm
        prevHash
        difficulty
        epochSlots
        slotNumber
        sk
        dlgCert
        body
        pv
        sv
    )
    body
    ()

--------------------------------------------------------------------------------
-- Block Accessors
--------------------------------------------------------------------------------

blockHash :: EpochSlots -> Block -> HeaderHash
blockHash epochSlots = hashHeader epochSlots . blockHeader

blockHashAnnotated :: ABlock ByteString -> HeaderHash
blockHashAnnotated = headerHashAnnotated . blockHeader

blockProtocolMagicId :: ABlock a -> ProtocolMagicId
blockProtocolMagicId = headerProtocolMagicId . blockHeader

blockAProtocolMagicId :: ABlock a -> Annotated ProtocolMagicId a
blockAProtocolMagicId = aHeaderProtocolMagicId . blockHeader

blockPrevHash :: ABlock a -> HeaderHash
blockPrevHash = headerPrevHash . blockHeader

blockProof :: ABlock a -> Proof
blockProof = headerProof . blockHeader

blockSlot :: ABlock a -> SlotNumber
blockSlot = headerSlot . blockHeader

blockGenesisKey :: ABlock a -> VerificationKey
blockGenesisKey = headerGenesisKey . blockHeader

blockIssuer :: ABlock a -> VerificationKey
blockIssuer = headerIssuer . blockHeader

blockDifficulty :: ABlock a -> ChainDifficulty
blockDifficulty = headerDifficulty . blockHeader

blockToSign :: EpochSlots -> ABlock a -> ToSign
blockToSign epochSlots = headerToSign epochSlots . blockHeader

blockSignature :: ABlock a -> ABlockSignature a
blockSignature = headerSignature . blockHeader

blockProtocolVersion :: ABlock a -> ProtocolVersion
blockProtocolVersion = headerProtocolVersion . blockHeader

blockSoftwareVersion :: ABlock a -> SoftwareVersion
blockSoftwareVersion = headerSoftwareVersion . blockHeader

blockTxPayload :: ABlock a -> ATxPayload a
blockTxPayload = bodyTxPayload . blockBody

blockSscPayload :: ABlock a -> SscPayload
blockSscPayload = bodySscPayload . blockBody

blockUpdatePayload :: ABlock a -> Update.APayload a
blockUpdatePayload = bodyUpdatePayload . blockBody

blockDlgPayload :: ABlock a -> Delegation.APayload a
blockDlgPayload = bodyDlgPayload . blockBody

blockLength :: ABlock ByteString -> Natural
blockLength = fromIntegral . BS.length . blockAnnotation

--------------------------------------------------------------------------------
-- Block Binary Serialization
--------------------------------------------------------------------------------

-- | Encode a block, given a number of slots-per-epoch.
--
--   Unlike 'toCBORABOBBlock', this function does not take the deprecated epoch
--   boundary blocks into account.
toCBORBlock :: EpochSlots -> Block -> Encoding
toCBORBlock epochSlots block =
  encodeListLen 3
    <> toCBORHeader epochSlots (blockHeader block)
    <> toCBOR (blockBody block)
    <> (encodeListLen 1 <> toCBOR (mempty :: Map Word8 LByteString))

fromCBORABlock :: EpochSlots -> Decoder s (ABlock ByteSpan)
fromCBORABlock epochSlots = do
  Annotated (header, body) byteSpan <- annotatedDecoder $ do
    enforceSize "Block" 3
    (,)
      <$> fromCBORAHeader epochSlots
      <*> fromCBOR
      -- Drop the deprecated ExtraBodyData
      <* (enforceSize "ExtraBodyData" 1 >> dropEmptyAttributes)
  pure $ ABlock header body byteSpan

--------------------------------------------------------------------------------
-- Block Formatting
--------------------------------------------------------------------------------

instance B.Buildable (WithEpochSlots Block) where
  build (WithEpochSlots es block) = renderBlock es block

renderBlock :: EpochSlots -> Block -> Builder
renderBlock es block =
  bprint
    ( "Block:\n"
        . "  "
        . build
        . "  transactions ("
        . int
        . " items): "
        . listJson
        . "\n"
        . "  "
        . build
        . "\n"
        . "  "
        . shown
        . "\n"
        . "  update payload: "
        . build
    )
    (WithEpochSlots es $ blockHeader block)
    (length txs)
    txs
    (blockDlgPayload block)
    (blockSscPayload block)
    (blockUpdatePayload block)
  where
    txs = bodyTxs $ blockBody block

--------------------------------------------------------------------------------
-- ABlockOrBoundary
--------------------------------------------------------------------------------

data ABlockOrBoundary a
  = ABOBBlock (ABlock a)
  | ABOBBoundary (ABoundaryBlock a)
  deriving (Eq, Generic, Show, Functor)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABlockOrBoundary a)

-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
toCBORABOBBlock :: EpochSlots -> ABlock a -> Encoding
toCBORABOBBlock epochSlots block =
  encodeListLen 2
    <> toCBOR (1 :: Word)
    <> toCBORBlock epochSlots (fmap (const ()) block)

-- | toCBORABoundaryBlock but with the list length and tag discriminator bytes.
toCBORABOBBoundary :: ProtocolMagicId -> ABoundaryBlock a -> Encoding
toCBORABOBBoundary pm bvd =
  encodeListLen 2
    <> toCBOR (0 :: Word)
    <> toCBORABoundaryBlock pm bvd

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
fromCBORABOBBlock :: EpochSlots -> Decoder s (Maybe Block)
fromCBORABOBBlock epochSlots =
  fromCBORABlockOrBoundary epochSlots >>= \case
    ABOBBoundary _ -> pure Nothing
    ABOBBlock b -> pure . Just $ void b

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
--
--   Previous versions of Cardano had an explicit boundary block between epochs.
--   A 'Block' was then represented as 'Either BoundaryBlock MainBlock'. We have
--   now deprecated these explicit boundary blocks, but we still need to decode
--   blocks in the old format. In the case that we find a boundary block, we
--   drop it using 'dropBoundaryBlock' and return a 'Nothing'.
fromCBORABlockOrBoundary ::
  EpochSlots -> Decoder s (ABlockOrBoundary ByteSpan)
fromCBORABlockOrBoundary epochSlots = do
  enforceSize "Block" 2
  fromCBOR @Word >>= \case
    0 -> ABOBBoundary <$> fromCBORABoundaryBlock
    1 -> ABOBBlock <$> fromCBORABlock epochSlots
    t -> cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)

toCBORABlockOrBoundary ::
  ProtocolMagicId -> EpochSlots -> ABlockOrBoundary a -> Encoding
toCBORABlockOrBoundary pm epochSlots abob = case abob of
  ABOBBlock blk -> toCBORABOBBlock epochSlots blk
  ABOBBoundary ebb -> toCBORABOBBoundary pm ebb

--------------------------------------------------------------------------------
-- ABoundaryBlock
--------------------------------------------------------------------------------

-- | For boundary body data, we only keep an annotation. It's the body and
-- extra body data.
data ABoundaryBody a = ABoundaryBody
  { boundaryBodyAnnotation :: !a
  }
  deriving (Eq, Generic, Show, Functor)

instance Decoded (ABoundaryBody ByteString) where
  type BaseType (ABoundaryBody ByteString) = ABoundaryBody ()
  recoverBytes = boundaryBodyAnnotation

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABoundaryBody a)

fromCBORABoundaryBody :: Decoder s (ABoundaryBody ByteSpan)
fromCBORABoundaryBody = do
  Annotated _ bs <- annotatedDecoder $ do
    dropBoundaryBody
    dropBoundaryExtraBodyData
  pure $ ABoundaryBody bs

-- | Every boundary body has the same encoding: empty.
toCBORABoundaryBody :: ABoundaryBody a -> Encoding
toCBORABoundaryBody _ =
  (encodeListLenIndef <> encodeBreak)
    <> ( encodeListLen 1
           <> toCBOR (mempty :: Map Word8 LByteString)
       )

-- | For a boundary block, we keep the header, body, and an annotation for
-- the whole thing (commonly the bytes from which it was decoded).
data ABoundaryBlock a = ABoundaryBlock
  { -- | Needed for validation.
    boundaryBlockLength :: !Int64,
    boundaryHeader :: !(ABoundaryHeader a),
    boundaryBody :: !(ABoundaryBody a),
    boundaryAnnotation :: !a
  }
  deriving (Eq, Generic, Show, Functor)

instance Decoded (ABoundaryBlock ByteString) where
  type BaseType (ABoundaryBlock ByteString) = ABoundaryBlock ()
  recoverBytes = boundaryAnnotation

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABoundaryBlock a)

-- | Extract the hash of a boundary block from its annotation.
boundaryHashAnnotated :: ABoundaryBlock ByteString -> HeaderHash
boundaryHashAnnotated = boundaryHeaderHashAnnotated . boundaryHeader

fromCBORABoundaryBlock :: Decoder s (ABoundaryBlock ByteSpan)
fromCBORABoundaryBlock = do
  Annotated (hdr, bod) bytespan@(ByteSpan start end) <- annotatedDecoder $ do
    enforceSize "BoundaryBlock" 3
    -- 1 item (list of 5)
    hdr <- fromCBORABoundaryHeader
    -- 2 items (body and extra body data)
    bod <- fromCBORABoundaryBody
    pure (hdr, bod)
  pure $
    ABoundaryBlock
      { boundaryBlockLength = end - start,
        boundaryHeader = hdr,
        boundaryBody = bod,
        boundaryAnnotation = bytespan
      }

-- | See note on `toCBORABoundaryHeader`. This as well does not necessarily
-- invert the decoder `fromCBORABoundaryBlock`.
toCBORABoundaryBlock :: ProtocolMagicId -> ABoundaryBlock a -> Encoding
toCBORABoundaryBlock pm ebb =
  encodeListLen 3
    -- 1 item (list of 5)
    <> toCBORABoundaryHeader pm (boundaryHeader ebb)
    -- 2 items (body and extra body data)
    <> toCBORABoundaryBody (boundaryBody ebb)

instance B.Buildable (ABoundaryBlock a) where
  build bvd =
    bprint
      ( "Boundary:\n"
          . "  Starting epoch: "
          . int
          . "\n"
          . "  "
          . later buildBoundaryHash
          . "\n"
          . "  Block number: "
          . build
      )
      (boundaryEpoch hdr)
      (boundaryPrevHash hdr)
      (boundaryDifficulty hdr)
    where
      hdr = boundaryHeader bvd
      buildBoundaryHash :: Either GenesisHash HeaderHash -> Builder
      buildBoundaryHash (Left (GenesisHash _)) = fromText "Genesis"
      buildBoundaryHash (Right h) = B.build h

-- | Compute the slot number assigned to a boundary block
boundaryBlockSlot ::
  EpochSlots ->
  -- | Epoch number
  Word64 ->
  SlotNumber
boundaryBlockSlot (EpochSlots es) epoch =
  SlotNumber $ es * epoch

{-------------------------------------------------------------------------------
  Header of a regular block or EBB
-------------------------------------------------------------------------------}

data ABlockOrBoundaryHdr a
  = ABOBBlockHdr !(AHeader a)
  | ABOBBoundaryHdr !(ABoundaryHeader a)
  deriving (Eq, Show, Functor, Generic, NoThunks)

fromCBORABlockOrBoundaryHdr ::
  EpochSlots ->
  Decoder s (ABlockOrBoundaryHdr ByteSpan)
fromCBORABlockOrBoundaryHdr epochSlots = do
  enforceSize "ABlockOrBoundaryHdr" 2
  fromCBOR @Word >>= \case
    0 -> ABOBBoundaryHdr <$> fromCBORABoundaryHeader
    1 -> ABOBBlockHdr <$> fromCBORAHeader epochSlots
    t -> fail $ "Unknown tag in encoded HeaderOrBoundary" <> show t

-- | Encoder for 'ABlockOrBoundaryHdr' which is using the annotation.
-- It is right inverse of 'fromCBORAblockOrBoundaryHdr'.
--
-- TODO: add a round trip test, e.g.
--
-- prop> fromCBORABlockOrBoundaryHdr . toCBORABlockOrBoundaryHdr = id
--
-- which does not type check, but convey the meaning.
toCBORABlockOrBoundaryHdr :: ABlockOrBoundaryHdr ByteString -> Encoding
toCBORABlockOrBoundaryHdr hdr =
  CBOR.encodeListLen 2
    <> case hdr of
      ABOBBoundaryHdr h ->
        CBOR.encodeWord 0
          <> CBOR.encodePreEncoded (boundaryHeaderAnnotation h)
      ABOBBlockHdr h ->
        CBOR.encodeWord 1
          <> CBOR.encodePreEncoded (headerAnnotation h)

-- | The size computation is compatible with 'toCBORABlockOrBoundaryHdr'
toCBORABlockOrBoundaryHdrSize :: Proxy (ABlockOrBoundaryHdr a) -> Size
toCBORABlockOrBoundaryHdrSize hdr =
  2 -- @encodeListLen 2@ followed by @encodeWord 0@ or @encodeWord 1@.
    + szCases
      [ Case "ABOBBoundaryHdr" $ toCBORABoundaryHeaderSize Proxy (ABOBBoundaryHdr `contramap` hdr),
        Case "ABOBBlockHdr" $ toCBORHeaderSize Proxy (ABOBBlockHdr `contramap` hdr)
      ]

-- | The analogue of 'Data.Either.either'
aBlockOrBoundaryHdr ::
  (AHeader a -> b) ->
  (ABoundaryHeader a -> b) ->
  ABlockOrBoundaryHdr a ->
  b
aBlockOrBoundaryHdr f _ (ABOBBlockHdr hdr) = f hdr
aBlockOrBoundaryHdr _ g (ABOBBoundaryHdr hdr) = g hdr

abobHdrFromBlock :: ABlockOrBoundary a -> ABlockOrBoundaryHdr a
abobHdrFromBlock (ABOBBlock blk) = ABOBBlockHdr $ blockHeader blk
abobHdrFromBlock (ABOBBoundary blk) = ABOBBoundaryHdr $ boundaryHeader blk

-- | Slot number of the header
--
-- NOTE: Epoch slot number calculation must match the one in 'applyBoundary'.
abobHdrSlotNo :: EpochSlots -> ABlockOrBoundaryHdr a -> SlotNumber
abobHdrSlotNo epochSlots =
  aBlockOrBoundaryHdr
    headerSlot
    (boundaryBlockSlot epochSlots . boundaryEpoch)

abobHdrChainDifficulty :: ABlockOrBoundaryHdr a -> ChainDifficulty
abobHdrChainDifficulty =
  aBlockOrBoundaryHdr
    headerDifficulty
    boundaryDifficulty

abobHdrHash :: ABlockOrBoundaryHdr ByteString -> HeaderHash
abobHdrHash (ABOBBoundaryHdr hdr) = boundaryHeaderHashAnnotated hdr
abobHdrHash (ABOBBlockHdr hdr) = headerHashAnnotated hdr

abobHdrPrevHash :: ABlockOrBoundaryHdr a -> Maybe HeaderHash
abobHdrPrevHash =
  aBlockOrBoundaryHdr
    (Just . headerPrevHash)
    (either (const Nothing) Just . boundaryPrevHash)
