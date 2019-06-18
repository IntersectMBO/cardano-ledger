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
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Block
  (
  -- * Block
    Block
  , ABlock(..)

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
  , toCBORBlock
  , fromCBORABlock

  -- * Block Formatting
  , renderBlock

  -- * ABlockOrBoundary
  , ABlockOrBoundary(..)
  , toCBORABOBBlock
  , fromCBORABOBBlock
  , fromCBORABlockOrBoundary

  -- * BoundaryValidationData
  , BoundaryValidationData(..)
  , boundaryHashAnnotated
  , dropBoundaryBlock
  , toCBORABOBBoundary
  , toCBORBoundaryBlock
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder (Builder, fromText)
import Formatting (bprint, build, int, later, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan(..)
  , Decoded(..)
  , Decoder
  , DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , encodeBreak
  , encodeListLen
  , encodeListLenIndef
  , enforceSize
  )
import Cardano.Chain.Block.Body
  ( ABody
  , Body
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
  , Header
  , HeaderHash
  , ToSign
  , dropBoundaryHeader
  , fromCBORAHeader
  , genesisHeaderHash
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
  , wrapBoundaryBytes
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
import Cardano.Crypto (ProtocolMagicId, SigningKey, VerificationKey, hashDecoded, hash)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

type Block = ABlock ()

data ABlock a = ABlock
  { blockHeader     :: AHeader a
  , blockBody       :: ABody a
  , blockAnnotation :: a
  } deriving (Eq, Show, Generic, NFData, Functor)


--------------------------------------------------------------------------------
-- Block Constructors
--------------------------------------------------------------------------------

-- | Smart constructor for 'Block'
mkBlock
  :: ProtocolMagicId
  -> ProtocolVersion
  -> SoftwareVersion
  -> Either GenesisHash Header
  -> EpochSlots
  -> SlotNumber
  -> SigningKey
  -- ^ The 'SigningKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SigningKey'
  -> Body
  -> Block
mkBlock pm bv sv prevHeader epochSlots = mkBlockExplicit
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
  = ABlock
    (mkHeaderExplicit
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
--
toCBORBlock :: EpochSlots -> Block -> Encoding
toCBORBlock epochSlots block
  =  encodeListLen 3
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
      <*  (enforceSize "ExtraBodyData" 1 >> dropEmptyAttributes)
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
-- ABlockOrBoundary
--------------------------------------------------------------------------------

data ABlockOrBoundary a
  = ABOBBlock (ABlock a)
  | ABOBBoundary (BoundaryValidationData a)
  deriving (Eq, Show, Functor)

-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
toCBORABOBBlock :: EpochSlots -> Block -> Encoding
toCBORABOBBlock epochSlots block =
  encodeListLen 2
    <> toCBOR (1 :: Word)
    <> toCBORBlock epochSlots block

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
fromCBORABOBBlock :: EpochSlots -> Decoder s (Maybe Block)
fromCBORABOBBlock epochSlots =
  fromCBORABlockOrBoundary epochSlots >>= \case
    ABOBBoundary _ -> pure Nothing
    ABOBBlock    b -> pure . Just $ void b

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
--
--   Previous versions of Cardano had an explicit boundary block between epochs.
--   A 'Block' was then represented as 'Either BoundaryBlock MainBlock'. We have
--   now deprecated these explicit boundary blocks, but we still need to decode
--   blocks in the old format. In the case that we find a boundary block, we
--   drop it using 'dropBoundaryBlock' and return a 'Nothing'.
fromCBORABlockOrBoundary
  :: EpochSlots -> Decoder s (ABlockOrBoundary ByteSpan)
fromCBORABlockOrBoundary epochSlots = do
  enforceSize "Block" 2
  fromCBOR @Word >>= \case
    0 -> ABOBBoundary <$> dropBoundaryBlock
    1 -> ABOBBlock <$> fromCBORABlock epochSlots
    t -> cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)


--------------------------------------------------------------------------------
-- BoundaryValidationData
--------------------------------------------------------------------------------

data BoundaryValidationData a = BoundaryValidationData
  { boundaryBlockLength :: !Int64
  -- ^ The length of the boundary block in bytes
  , boundaryPrevHash    :: !(Either GenesisHash HeaderHash)
  -- ^ The hash of the previous block. Should only be GenesisHash for the
  -- initial boundary block.
  , boundaryEpoch       :: !Word64
  , boundaryDifficulty  :: !ChainDifficulty
  -- ^ Block number
  , boundaryHeaderBytes :: !a
  -- ^ Annotation representing the header bytes
  } deriving (Eq, Show, Functor)

instance Decoded (BoundaryValidationData ByteString) where
  type BaseType (BoundaryValidationData ByteString) = BoundaryValidationData ()
  recoverBytes = boundaryHeaderBytes

-- | Extract the hash of a boundary block from its annotation.
boundaryHashAnnotated :: BoundaryValidationData ByteString -> HeaderHash
boundaryHashAnnotated = coerce . hashDecoded . fmap wrapBoundaryBytes

-- | A decoder that drops the boundary block, but preserves the 'ByteSpan' of
--   the header for hashing
dropBoundaryBlock :: Decoder s (BoundaryValidationData ByteSpan)
dropBoundaryBlock = do
  Annotated (Annotated (hh, epoch, difficulty) bs) (ByteSpan start end) <- annotatedDecoder $ do
    enforceSize "BoundaryBlock" 3
    aHeaderStuff <- annotatedDecoder dropBoundaryHeader
    dropBoundaryBody
    dropBoundaryExtraBodyData
    pure aHeaderStuff
  pure $ BoundaryValidationData
    { boundaryBlockLength = end - start
    -- For the zeroth boundary block this field needs to be a 'GenesisHash'
    -- and for all subsequent blocks it's a 'HeaderHash'.
    , boundaryPrevHash    = if epoch == 0 then Left (coerce hh) else Right hh
    , boundaryEpoch       = epoch
    , boundaryDifficulty  = difficulty
    , boundaryHeaderBytes = bs
    }

toCBORABOBBoundary :: ProtocolMagicId -> BoundaryValidationData a -> Encoding
toCBORABOBBoundary pm bvd =
  encodeListLen 2
    <> toCBOR (0 :: Word)
    <> toCBORBoundaryBlock pm bvd

-- See https://github.com/input-output-hk/cardano-sl/blob/develop/docs/on-the-wire/current-spec.cddl
toCBORBoundaryBlock :: ProtocolMagicId -> BoundaryValidationData a -> Encoding
toCBORBoundaryBlock pm bvd = let
    bodyProof = hash (mempty :: LByteString)
  in encodeListLen 3
    -- Header
    <> ( encodeListLen 5
        -- Protocol magic
        <> toCBOR pm
        -- Previous block
        <>  ( case boundaryPrevHash bvd of
                Left gh -> toCBOR (genesisHeaderHash gh)
                Right hh -> toCBOR hh
            )
        -- Body proof
        <> toCBOR bodyProof
        -- Consensus data
        <> ( encodeListLen 2
            -- Epoch
            <> toCBOR (boundaryEpoch bvd)
            -- Chain difficulty
            <> toCBOR (boundaryDifficulty bvd)
           )
        -- Extra data
        <> (encodeListLen 1 <> toCBOR (mempty :: Map Word8 LByteString))
      )
    -- Body
    <> (encodeListLenIndef <> encodeBreak)
    -- Attributes
    <> ( encodeListLen 1
        <> toCBOR (mempty :: Map Word8 LByteString)
       )

instance B.Buildable (BoundaryValidationData a) where
  build bvd = bprint
    ( "Boundary:\n"
    . "  Starting epoch: " . int . "\n"
    . "  " . later buildBoundaryHash . "\n"
    . "  Block number: " . build
    )
    (boundaryEpoch bvd)
    (boundaryPrevHash bvd)
    (boundaryDifficulty bvd)
    where
      buildBoundaryHash :: Either GenesisHash HeaderHash -> Builder
      buildBoundaryHash (Left (GenesisHash _)) = fromText "Genesis"
      buildBoundaryHash (Right h) = B.build h
