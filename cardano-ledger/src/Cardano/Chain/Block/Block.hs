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
{-# LANGUAGE PatternSynonyms      #-}

module Cardano.Chain.Block.Block
  (
  -- * Block
    Block (blockHeader, blockBody, blockSerialized)

  -- * Block Constructors
  , pattern Block
  , mkBlockExplicit

  -- * Block Accessors
  , blockHash
  , blockHashAnnotated
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

  -- * BoundaryBlock
  , BoundaryBlock(boundaryBlockLength, boundaryHeader, boundaryBody)
  , pattern BoundaryBlock
  , boundaryHashAnnotated
  , toCBORBOBBoundary

  , BoundaryBody
  , pattern BoundaryBody
  )
where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder (Builder, fromText)
import Formatting (bprint, build, int, later, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( AnnotatedDecoder
  , DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , FromCBORAnnotated (..)
  , ToCBOR(..)
  , encodeBreak
  , encodeListLen
  , encodeListLenIndef
  , encodePreEncoded
  , enforceSize
  , withSlice'
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
  ( BlockSignature
  , Header(..)
  , BoundaryHeader(..)
  , Header
  , HeaderHash
  , ToSign
  , boundaryHeaderHashAnnotated
  , fromCBORHeader
  , hashHeader
  , headerDifficulty
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
import Cardano.Chain.UTxO.TxPayload (TxPayload)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto (ProtocolMagicId, SigningKey, VerificationKey)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data Block = Block'
  { blockHeader'    :: !Header
  , blockBody'      :: !Body
  , blockSerialized :: ByteString
  } deriving (Eq, Show, Generic, NFData)


--------------------------------------------------------------------------------
-- Block Constructors
--------------------------------------------------------------------------------

-- | Smart constructor for 'Block'
{-# COMPLETE Block #-}
pattern Block :: Header -> Body -> Block
pattern Block { blockHeader, blockBody } <- Block' blockHeader blockBody _
  where
  Block header body =
    let bytes = serializeEncoding' $ encodeListLen 3
                <> toCBOR header
                <> toCBOR body
                <> (encodeListLen 1 <> toCBOR (mempty :: Map Word8 LByteString))
    in Block' header body bytes

-- | Smart constructor for 'Block', without requiring the entire previous
--   'Header'. Instead, you give its hash and the difficulty of this block.
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
    in Block header body


--------------------------------------------------------------------------------
-- Block Accessors
--------------------------------------------------------------------------------

blockHash :: Block -> HeaderHash
blockHash = hashHeader . blockHeader

blockHashAnnotated :: Block -> HeaderHash
blockHashAnnotated = hashHeader . blockHeader

blockProtocolMagicId :: Block -> ProtocolMagicId
blockProtocolMagicId = headerProtocolMagicId . blockHeader

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

blockSignature :: Block -> BlockSignature
blockSignature = headerSignature . blockHeader

blockProtocolVersion :: Block -> ProtocolVersion
blockProtocolVersion = headerProtocolVersion . blockHeader

blockSoftwareVersion :: Block -> SoftwareVersion
blockSoftwareVersion = headerSoftwareVersion . blockHeader

blockTxPayload :: Block -> TxPayload
blockTxPayload = bodyTxPayload . blockBody

blockSscPayload :: Block -> SscPayload
blockSscPayload = bodySscPayload . blockBody

blockUpdatePayload :: Block -> Update.Payload
blockUpdatePayload = bodyUpdatePayload . blockBody

blockDlgPayload :: Block -> Delegation.Payload
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
  Block' <$ lift (enforceSize "Block" 3)
    <*> fromCBORHeader epochSlots
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
    (WithEpochSlots es $ blockHeader block)
    (length txs)
    txs
    (blockDlgPayload block)
    (blockSscPayload block)
    (blockUpdatePayload block)
  where txs = bodyTxs $ blockBody block


--------------------------------------------------------------------------------
-- BlockOrBoundary
--------------------------------------------------------------------------------

data BlockOrBoundary
  = BOBBlock Block
  | BOBBoundary BoundaryBlock
  deriving (Eq, Show)

-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
toCBORBOBBlock :: Block -> Encoding
toCBORBOBBlock block =
  encodeListLen 2
    <> toCBOR (1 :: Word)
    <> toCBOR block

-- | toCBORABoundaryBlock but with the list length and tag discriminator bytes.
toCBORBOBBoundary :: BoundaryBlock -> Encoding
toCBORBOBBoundary bvd =
  encodeListLen 2
    <> toCBOR (0 :: Word)
    <> toCBOR bvd

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
    0 -> BOBBoundary <$> fromCBORAnnotated'
    1 -> BOBBlock <$> fromCBORBlock epochSlots
    t -> lift $ cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)

toCBORBlockOrBoundary :: BlockOrBoundary -> Encoding
toCBORBlockOrBoundary abob = case abob of
  BOBBlock    blk -> toCBORBOBBlock blk
  BOBBoundary ebb -> toCBORBOBBoundary ebb

--------------------------------------------------------------------------------
-- BoundaryBlock
--------------------------------------------------------------------------------

-- | For boundary body data, we only keep an annotation. It's the body and
-- extra body data.
data BoundaryBody = BoundaryBody'
  { boundaryBodySerialized :: ByteString
  } deriving (Eq, Show)

pattern BoundaryBody :: BoundaryBody
pattern BoundaryBody <- BoundaryBody' _
  where
  BoundaryBody = BoundaryBody' $ serializeEncoding' $
    (encodeListLenIndef <> encodeBreak)
      <> ( encodeListLen 1 <> toCBOR (mempty :: Map Word8 LByteString))

instance FromCBORAnnotated BoundaryBody where
  fromCBORAnnotated' = withSlice' $
    BoundaryBody'
      <$ lift dropBoundaryBody
      <* lift dropBoundaryExtraBodyData

instance ToCBOR BoundaryBody where
  toCBOR = encodePreEncoded . boundaryBodySerialized

-- | For a boundary block, we keep the header, body, and an annotation for
-- the whole thing (commonly the bytes from which it was decoded).
data BoundaryBlock = BoundaryBlock'
  { boundaryBlockLength     :: Int64
  -- ^ Needed for validation.
  , boundaryHeader'         :: !BoundaryHeader
  , boundaryBody'           :: !BoundaryBody
  , boundarySerialized      :: ByteString
  } deriving (Eq, Show)

-- | Extract the hash of a boundary block from its annotation.
boundaryHashAnnotated :: BoundaryBlock -> HeaderHash
boundaryHashAnnotated = boundaryHeaderHashAnnotated . boundaryHeader

instance FromCBORAnnotated BoundaryBlock where
  fromCBORAnnotated' = withSlice' $ do
    lift $ enforceSize "BoundaryBlock" 3
    -- 1 item (list of 5)
    hdr <- fromCBORAnnotated'
    -- 2 items (body and extra body data)
    bod <- fromCBORAnnotated'
    pure $
      \bytes -> BoundaryBlock' (fromIntegral $ BS.length bytes) hdr bod bytes

pattern BoundaryBlock :: BoundaryHeader -> BoundaryBody -> BoundaryBlock
pattern BoundaryBlock{ boundaryHeader, boundaryBody } <-
  BoundaryBlock' _ boundaryHeader boundaryBody _
  where
    BoundaryBlock hdr bod =
      let bytes = serializeEncoding' $ 
            encodeListLen 3
              -- 1 item (list of 5)
              <> toCBOR hdr
              -- 2 items (body and extra body data)
              <> toCBOR bod
      in BoundaryBlock' (fromIntegral $ BS.length bytes) hdr bod bytes

instance ToCBOR BoundaryBlock where
  toCBOR = encodePreEncoded . boundarySerialized

instance B.Buildable BoundaryBlock where
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
