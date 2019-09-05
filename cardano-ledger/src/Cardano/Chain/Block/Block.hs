{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Cardano.Chain.Block.Block
  (
  -- * Block
    Block (..)

  -- * Block Constructors
  , mkBlock
  , mkBlockExplicit

  -- * Block Accessors
  , blockHash
  , blockHashAnnotated
  , blockAProtocolMagicId
  , blockProtocolMagicId
  , blockPrevHash
  , blockProof
  , blockSlot
  , blockGenesisKey
  , blockIssuer
  , blockDifficulty
  , blockToSign
  , blockSignature
  , blockProtocolVersion
  , blockSoftwareVersion
  , blockTxPayload
  , blockSscPayload
  , blockDlgPayload
  , blockUpdatePayload
  , blockLength

  -- * Block Binary Serialization
  , fromCBORBlock

  -- * Block Formatting
  , renderBlock

  -- * BlockOrBoundary
  , BlockOrBoundary(..)
  , toCBORBOBBlock
  , fromCBORBOBBlock
  , fromCBORBlockOrBoundary
  , toCBORBlockOrBoundary

  -- * ABoundaryBlock
  , ABoundaryBlock(..)
  , boundaryHashAnnotated
  , fromCBORABoundaryBlock
  , toCBORABoundaryBlock
  , toCBORBOBBoundary

  , ABoundaryBody(..)
  )
where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder (Builder, fromText)
import Formatting (bprint, build, int, later, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , AnnotatedDecoder
  , ByteSpan(..)
  , Decoded(..)
  , Decoder
  , DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , FromCBORAnnotated (..)
  , ToCBOR(..)
  , annotatedDecoder
  , encodeBreak
  , encodeListLen
  , encodeListLenIndef
  , encodePreEncoded
  , enforceSize
  , withSlice'
  , liftByteSpanDecoder
  , serializeEncoding'
  )
import Cardano.Chain.Block.Body
  ( Body
  , bodyDlgPayload
  , bodySscPayload
  , bodyTxPayload
  , bodyTxs
  , bodyUpdatePayload
  )
import Cardano.Chain.Block.Boundary
  (dropBoundaryBody, dropBoundaryExtraBodyData)
import Cardano.Chain.Block.Header
  ( ABlockSignature
  , AHeader(..)
  , ABoundaryHeader(..)
  , Header
  , HeaderHash
  , ToSign
  , boundaryHeaderHashAnnotated
  , fromCBORABoundaryHeader
  , fromCBORAHeader
  , hashHeader
  , headerDifficulty
  , headerHashAnnotated
  , headerGenesisKey
  , headerIssuer
  , headerPrevHash
  , headerProof
  , headerProtocolMagicId
  , headerProtocolVersion
  , headerSignature
  , headerSlot
  , headerSoftwareVersion
  , headerToSign
  , mkHeaderExplicit
  , toCBORHeader
  , toCBORABoundaryHeader
  )
import Cardano.Chain.Block.Proof (Proof(..))
import Cardano.Chain.Common (ChainDifficulty(..), dropEmptyAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting
  ( EpochSlots
  , SlotNumber
  , WithEpochSlots(WithEpochSlots)
  )
import Cardano.Chain.Ssc (SscPayload)
import Cardano.Chain.UTxO.TxPayload (ATxPayload)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto (ProtocolMagicId, SigningKey, VerificationKey)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data Block = Block
  { blockHeader     :: AHeader ByteString
  , blockBody       :: Body
  , blockSerialized :: ByteString
  } deriving (Eq, Show, Generic, NFData)


--------------------------------------------------------------------------------
-- Block Constructors
--------------------------------------------------------------------------------

-- | Smart constructor for 'Block'
mkBlock
  :: EpochSlots
  -> Header
  -> Body
  -> Block
mkBlock epochSlots header body =
  -- FIXME: This constructs the members of members of the block with
  -- incorrect bytestring references. We'd need to make the same change we
  -- made to block all the way down to correct this problem.
  let headerBytes = serializeEncoding' $ toCBORHeader epochSlots header
      header' = headerBytes <$ header
      bytes = serializeEncoding' $ encodeListLen 3
              <> encodePreEncoded headerBytes
              <> toCBOR body
              <> (encodeListLen 1 <> toCBOR (mempty :: Map Word8 LByteString))
  in Block header' body bytes

-- | Smart constructor for 'Block', without requiring the entire previous
--   'Header'. Instead, you give its hash and the difficulty of this block.
--   These are derived from the previous header in 'mkBlock' so if you have
--   the previous header, consider using that one.
mkBlockExplicit
  :: ProtocolMagicId
  -> ProtocolVersion
  -> SoftwareVersion
  -> HeaderHash
  -> ChainDifficulty
  -> EpochSlots
  -> SlotNumber
  -> SigningKey
  -- ^ The 'SigningKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SigningKey'
  -> Body
  -> Block
mkBlockExplicit pm pv sv prevHash difficulty epochSlots slotNumber sk dlgCert body
  = let header = mkHeaderExplicit
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
    in mkBlock epochSlots header body


--------------------------------------------------------------------------------
-- Block Accessors
--------------------------------------------------------------------------------

blockHash :: EpochSlots -> Block -> HeaderHash
blockHash epochSlots = hashHeader epochSlots . void . blockHeader

blockHashAnnotated :: Block -> HeaderHash
blockHashAnnotated = headerHashAnnotated . blockHeader

blockProtocolMagicId :: Block -> ProtocolMagicId
blockProtocolMagicId = headerProtocolMagicId . blockHeader

blockAProtocolMagicId :: Block -> Annotated ProtocolMagicId ByteString
blockAProtocolMagicId = aHeaderProtocolMagicId . blockHeader

blockPrevHash :: Block -> HeaderHash
blockPrevHash = headerPrevHash . blockHeader

blockProof :: Block -> Proof
blockProof = headerProof . blockHeader

blockSlot :: Block -> SlotNumber
blockSlot = headerSlot . blockHeader

blockGenesisKey :: Block -> VerificationKey
blockGenesisKey = headerGenesisKey . blockHeader

blockIssuer :: Block -> VerificationKey
blockIssuer = headerIssuer . blockHeader

blockDifficulty :: Block -> ChainDifficulty
blockDifficulty = headerDifficulty . blockHeader

blockToSign :: EpochSlots -> Block -> ToSign
blockToSign epochSlots = headerToSign epochSlots . blockHeader

blockSignature :: Block -> ABlockSignature ByteString
blockSignature = headerSignature . blockHeader

blockProtocolVersion :: Block -> ProtocolVersion
blockProtocolVersion = headerProtocolVersion . blockHeader

blockSoftwareVersion :: Block -> SoftwareVersion
blockSoftwareVersion = headerSoftwareVersion . blockHeader

blockTxPayload :: Block -> ATxPayload ByteString
blockTxPayload = bodyTxPayload . blockBody

blockSscPayload :: Block -> SscPayload
blockSscPayload = bodySscPayload . blockBody

blockUpdatePayload :: Block -> Update.APayload ByteString
blockUpdatePayload = bodyUpdatePayload . blockBody

blockDlgPayload :: Block -> Delegation.APayload ByteString
blockDlgPayload = bodyDlgPayload . blockBody

blockLength :: Block -> Natural
blockLength = fromIntegral . BS.length . blockSerialized


--------------------------------------------------------------------------------
-- Block Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Block where
  toCBOR = encodePreEncoded . blockSerialized

fromCBORBlock :: EpochSlots -> AnnotatedDecoder s Block
fromCBORBlock epochSlots = withSlice' $
  Block <$ lift (enforceSize "Block" 3)
    <*> liftByteSpanDecoder (fromCBORAHeader epochSlots)
    <*> fromCBORAnnotated'
    -- Drop the deprecated ExtraBodyData
    <* (lift $ enforceSize "ExtraBodyData" 1 >> dropEmptyAttributes)

--------------------------------------------------------------------------------
-- Block Formatting
--------------------------------------------------------------------------------

instance B.Buildable (WithEpochSlots Block) where
  build (WithEpochSlots es block) = renderBlock es block

renderBlock :: EpochSlots -> Block -> Builder
renderBlock es block =
  bprint
    ( "Block:\n"
    . "  " . build . "  transactions (" . int . " items): " . listJson . "\n"
    . "  " . build . "\n"
    . "  " . shown . "\n"
    . "  update payload: " . build
    )
    (WithEpochSlots es $ void $ blockHeader block)
    (length txs)
    txs
    (blockDlgPayload block)
    (blockSscPayload block)
    (void $ blockUpdatePayload block)
  where txs = bodyTxs $ blockBody block


--------------------------------------------------------------------------------
-- BlockOrBoundary
--------------------------------------------------------------------------------

data BlockOrBoundary
  = BOBBlock Block
  | BOBBoundary (ABoundaryBlock ByteString)
  deriving (Eq, Show)

-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
toCBORBOBBlock :: Block -> Encoding
toCBORBOBBlock block =
  encodeListLen 2
    <> toCBOR (1 :: Word)
    <> toCBOR block

-- | toCBORABoundaryBlock but with the list length and tag discriminator bytes.
toCBORBOBBoundary :: ProtocolMagicId -> ABoundaryBlock a -> Encoding
toCBORBOBBoundary pm bvd =
  encodeListLen 2
    <> toCBOR (0 :: Word)
    <> toCBORABoundaryBlock pm bvd

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
fromCBORBOBBlock :: EpochSlots -> AnnotatedDecoder s (Maybe Block)
fromCBORBOBBlock epochSlots =
  fromCBORBlockOrBoundary epochSlots >>= \case
    BOBBoundary _ -> pure Nothing
    BOBBlock    b -> pure . Just $ b

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
--
--   Previous versions of Cardano had an explicit boundary block between epochs.
--   A 'Block' was then represented as 'Either BoundaryBlock MainBlock'. We have
--   now deprecated these explicit boundary blocks, but we still need to decode
--   blocks in the old format. In the case that we find a boundary block, we
--   drop it using 'dropBoundaryBlock' and return a 'Nothing'.
fromCBORBlockOrBoundary
  :: EpochSlots -> AnnotatedDecoder s BlockOrBoundary
fromCBORBlockOrBoundary epochSlots = do
  lift $ enforceSize "Block" 2
  (lift $ fromCBOR @Word) >>= \case
    0 -> BOBBoundary <$> liftByteSpanDecoder fromCBORABoundaryBlock
    1 -> BOBBlock <$> fromCBORBlock epochSlots
    t -> lift $ cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)

toCBORBlockOrBoundary
  :: ProtocolMagicId -> BlockOrBoundary -> Encoding
toCBORBlockOrBoundary pm abob = case abob of
  BOBBlock    blk -> toCBORBOBBlock blk
  BOBBoundary ebb -> toCBORBOBBoundary pm ebb

--------------------------------------------------------------------------------
-- ABoundaryBlock
--------------------------------------------------------------------------------

-- | For boundary body data, we only keep an annotation. It's the body and
-- extra body data.
data ABoundaryBody a = ABoundaryBody
  { boundaryBodyAnnotation :: !a
  } deriving (Eq, Show, Functor)

instance Decoded (ABoundaryBody ByteString) where
  type BaseType (ABoundaryBody ByteString) = ABoundaryBody ()
  recoverBytes = boundaryBodyAnnotation

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
  { boundaryBlockLength     :: !Int64
  -- ^ Needed for validation.
  , boundaryHeader          :: !(ABoundaryHeader a)
  , boundaryBody            :: !(ABoundaryBody a)
  , boundaryAnnotation      :: !a
  } deriving (Eq, Show, Functor)

instance Decoded (ABoundaryBlock ByteString) where
  type BaseType (ABoundaryBlock ByteString) = ABoundaryBlock ()
  recoverBytes = boundaryAnnotation

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
  pure $ ABoundaryBlock
    { boundaryBlockLength = end - start
    , boundaryHeader      = hdr
    , boundaryBody        = bod
    , boundaryAnnotation  = bytespan
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
  build bvd = bprint
    ( "Boundary:\n"
    . "  Starting epoch: " . int . "\n"
    . "  " . later buildBoundaryHash . "\n"
    . "  Block number: " . build
    )
    (boundaryEpoch hdr)
    (boundaryPrevHash hdr)
    (boundaryDifficulty hdr)
    where
      hdr = boundaryHeader bvd
      buildBoundaryHash :: Either GenesisHash HeaderHash -> Builder
      buildBoundaryHash (Left (GenesisHash _)) = fromText "Genesis"
      buildBoundaryHash (Right h) = B.build h
