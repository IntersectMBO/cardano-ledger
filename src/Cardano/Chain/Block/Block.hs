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
  , blockLeaderKey
  , blockIssuer
  , blockDifficulty
  , blockToSign
  , blockSignature
  , blockProtocolVersion
  , blockSoftwareVersion
  , blockHeaderAttributes
  , blockExtraDataProof
  , blockTxPayload
  , blockSscPayload
  , blockDlgPayload
  , blockUpdatePayload
  , blockAttributes
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
  , fromCBORAnnotated
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
import Cardano.Chain.Block.ExtraBodyData (ExtraBodyData(..))
import Cardano.Chain.Block.ExtraHeaderData (ExtraHeaderData(..))
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
  , headerAttributes
  , headerDifficulty
  , headerEBDataProof
  , headerIssuer
  , headerLeaderKey
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
import Cardano.Chain.Common (Attributes, ChainDifficulty (..), mkAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting
  ( EpochSlots
  , FlatSlotId
  , WithEpochSlots(WithEpochSlots)
  )
import Cardano.Chain.Ssc (SscPayload)
import Cardano.Chain.Txp.TxPayload (ATxPayload)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  (Hash, ProtocolMagicId, PublicKey, SecretKey, hash, hashDecoded)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------


type Block = ABlock ()

data ABlock a = ABlock
  { blockHeader     :: AHeader a
  , blockBody       :: ABody a
  , aBlockExtraData :: Annotated ExtraBodyData a
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
    . "  update payload: " . build . "\n"
    . "  " . build
    )
    (WithEpochSlots es $ blockHeader block)
    (length txs)
    txs
    (blockDlgPayload block)
    (blockSscPayload block)
    (blockUpdatePayload block)
    (blockExtraData block)
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
  <> toCBOR (blockExtraData block)

fromCBORABlock :: EpochSlots -> Decoder s (ABlock ByteSpan)
fromCBORABlock epochSlots = do
  Annotated (header, body, ed) byteSpan <-
    annotatedDecoder $ do
      enforceSize "Block" 3
      (,,) <$> fromCBORAHeader epochSlots <*> fromCBOR <*> fromCBORAnnotated
  pure $ ABlock header body ed byteSpan


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
  -> SecretKey
  -- ^ The 'SecretKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SecretKey'
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
  -> SecretKey
  -- ^ The 'SecretKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SecretKey'
  -> Body
  -> Block
mkBlockExplicit pm bv sv prevHash difficulty epochSlots slotId sk dlgCert body
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
      extraH
    )
    body
    (Annotated extraB ())
    ()
 where
  extraB :: ExtraBodyData
  extraB = ExtraBodyData (mkAttributes ())
  extraH :: ExtraHeaderData
  extraH = ExtraHeaderData bv sv (mkAttributes ()) (hash extraB)

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

blockExtraData :: ABlock a -> ExtraBodyData
blockExtraData = unAnnotated . aBlockExtraData

blockPrevHash :: ABlock a -> HeaderHash
blockPrevHash = headerPrevHash . blockHeader

blockProof :: ABlock a -> Proof
blockProof = headerProof . blockHeader

blockSlot :: ABlock a -> FlatSlotId
blockSlot = headerSlot . blockHeader

blockLeaderKey :: ABlock a -> PublicKey
blockLeaderKey = headerLeaderKey . blockHeader

blockIssuer :: ABlock a -> PublicKey
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

blockHeaderAttributes :: ABlock a -> Attributes ()
blockHeaderAttributes = headerAttributes . blockHeader

blockExtraDataProof :: ABlock a -> Hash ExtraBodyData
blockExtraDataProof = headerEBDataProof . blockHeader

blockTxPayload :: ABlock a -> ATxPayload a
blockTxPayload = bodyTxPayload . blockBody

blockSscPayload :: ABlock a -> SscPayload
blockSscPayload = bodySscPayload . blockBody

blockUpdatePayload :: ABlock a -> Update.APayload a
blockUpdatePayload = bodyUpdatePayload . blockBody

blockDlgPayload :: ABlock a -> Delegation.APayload a
blockDlgPayload = bodyDlgPayload . blockBody

blockAttributes :: ABlock a -> Attributes ()
blockAttributes = ebdAttributes . blockExtraData

blockLength :: ABlock ByteString -> Natural
blockLength = fromIntegral . BS.length . blockAnnotation
