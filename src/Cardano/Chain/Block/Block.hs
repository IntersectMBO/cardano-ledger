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
  ( ABlock(..)
  , Block
  , toCBORBlock
  , toCBORBlockWithoutBoundary
  , fromCBORABlock
  , fromCBORABlockOrBoundary
  , fromCBORBlockOrBoundary
  , mkBlock
  , mkBlockExplicit
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

  -- * BoundaryBlock
  , ABlockOrBoundary(..)
  , BoundaryValidationData(..)
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, build, int, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan(..)
  , Decoder
  , DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , encodeListLen
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
  ( AHeader(..)
  , BlockSignature(..)
  , Header
  , HeaderHash
  , ToSign
  , fromCBORAHeader
  , dropBoundaryHeader
  , toCBORHeader'
  , genesisHeaderHash
  , hashHeader
  , headerDifficulty
  , headerIssuer
  , headerGenesisKey
  , headerPrevHash
  , headerProof
  , headerProtocolMagicId
  , headerProtocolVersion
  , headerSignature
  , headerSlot
  , headerSoftwareVersion
  , headerToSign
  , mkHeaderExplicit
  , wrapHeaderBytes
  )
import Cardano.Chain.Block.Proof (Proof(..))
import Cardano.Chain.Common (ChainDifficulty(..), dropEmptyAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting
  ( EpochSlots
  , FlatSlotId
  , WithEpochSlots(WithEpochSlots)
  )
import Cardano.Chain.Ssc (SscPayload)
import Cardano.Chain.UTxO.TxPayload (ATxPayload)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  (ProtocolMagicId, SigningKey, VerificationKey, hashDecoded)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------


type Block = ABlock ()

data ABlock a = ABlock
  { blockHeader     :: AHeader a
  , blockBody       :: ABody a
  , blockAnnotation :: a
  } deriving (Eq, Show, Generic, NFData, Functor)

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


-- | Encode a block, given a number of slots-per-epoch.
--
-- Unlike 'toCBORBlock', this function does not take the deprecated epoch
-- boundary blocks into account.
--
toCBORBlockWithoutBoundary :: EpochSlots -> Block -> Encoding
toCBORBlockWithoutBoundary epochSlots block
  =  encodeListLen 3
  <> toCBORHeader' epochSlots (blockHeader block)
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


-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
toCBORBlock :: EpochSlots -> Block -> Encoding
toCBORBlock epochSlots block =
  encodeListLen 2
    <> toCBOR (1 :: Word)
    <> toCBORBlockWithoutBoundary epochSlots block

data ABlockOrBoundary a
  = ABOBBlock (ABlock a)
  | ABOBBoundary (BoundaryValidationData a)
  deriving (Eq, Show, Functor)

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

fromCBORBlockOrBoundary :: EpochSlots -> Decoder s (Maybe Block)
fromCBORBlockOrBoundary epochSlots =
  fromCBORABlockOrBoundary epochSlots >>= \case
    ABOBBoundary _ -> pure Nothing
    ABOBBlock    b -> pure . Just $ void b

--------------------------------------------------------------------------------
-- BoundaryBlock
--------------------------------------------------------------------------------

data BoundaryValidationData a = BoundaryValidationData
  { boundaryBlockLength :: !Int64
  -- ^ The length of the boundary block in bytes
  , boundaryPrevHash    :: !(Either GenesisHash HeaderHash)
  -- ^ The hash of the previous block. Should only be GenesisHash for the
  -- initial boundary block.
  , boundaryEpoch       :: !Word64
  , boundaryHeaderBytes :: !a
  -- ^ Annotation representing the header bytes
  } deriving (Eq, Show, Functor)

-- | A decoder that drops the boundary block, but preserves the 'ByteSpan' of
--   the header for hashing
dropBoundaryBlock :: Decoder s (BoundaryValidationData ByteSpan)
dropBoundaryBlock = do
  Annotated (Annotated (hh, epoch) bs) (ByteSpan start end) <- annotatedDecoder $ do
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
    , boundaryHeaderBytes = bs
    }

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Smart constructor for 'Block'
mkBlock
  :: ProtocolMagicId
  -> ProtocolVersion
  -> SoftwareVersion
  -> Either GenesisHash Header
  -> EpochSlots
  -> FlatSlotId
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
  -> FlatSlotId
  -> SigningKey
  -- ^ The 'SigningKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SigningKey'
  -> Body
  -> Block
mkBlockExplicit pm pv sv prevHash difficulty epochSlots slotId sk dlgCert body
  = ABlock
    (mkHeaderExplicit
      pm
      prevHash
      difficulty
      epochSlots
      slotId
      sk
      dlgCert
      body
      pv
      sv
    )
    body
    ()


--------------------------------------------------------------------------------
-- Block accessors
--------------------------------------------------------------------------------

blockHash :: EpochSlots -> Block -> HeaderHash
blockHash epochSlots = hashHeader epochSlots . blockHeader

blockHashAnnotated :: ABlock ByteString -> HeaderHash
blockHashAnnotated = hashDecoded . fmap wrapHeaderBytes . blockHeader

blockProtocolMagicId :: ABlock a -> ProtocolMagicId
blockProtocolMagicId = headerProtocolMagicId . blockHeader

blockAProtocolMagicId :: ABlock a -> Annotated ProtocolMagicId a
blockAProtocolMagicId = aHeaderProtocolMagicId . blockHeader

blockPrevHash :: ABlock a -> HeaderHash
blockPrevHash = headerPrevHash . blockHeader

blockProof :: ABlock a -> Proof
blockProof = headerProof . blockHeader

blockSlot :: ABlock a -> FlatSlotId
blockSlot = headerSlot . blockHeader

blockGenesisKey :: ABlock a -> VerificationKey
blockGenesisKey = headerGenesisKey . blockHeader

blockIssuer :: ABlock a -> VerificationKey
blockIssuer = headerIssuer . blockHeader

blockDifficulty :: ABlock a -> ChainDifficulty
blockDifficulty = headerDifficulty . blockHeader

blockToSign :: EpochSlots -> ABlock a -> ToSign
blockToSign epochSlots = headerToSign epochSlots . blockHeader

blockSignature :: ABlock a -> BlockSignature
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
