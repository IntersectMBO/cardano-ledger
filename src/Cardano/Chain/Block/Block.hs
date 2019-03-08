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
  , encodeBlock
  , decodeABlock
  , decodeABlockOrBoundary
  , decodeBlockOrBoundary
  , mkBlock
  , mkBlockExplicit
  , blockHash
  , blockHashAnnotated
  , blockPrevHash
  , blockProof
  , blockSlot
  , blockLeaderKey
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
  , dropBoundaryBlock
  )
where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Formatting (bprint, build, int, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan(..)
  , Decoder
  , DecoderError(..)
  , Encoding
  , annotatedDecoder
  , decodeAnnotated
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
  , decodeABody
  )
import Cardano.Chain.Block.Boundary
  (dropBoundaryBody, dropBoundaryExtraBodyData)
import Cardano.Chain.Block.ExtraBodyData (ExtraBodyData(..))
import Cardano.Chain.Block.ExtraHeaderData (ExtraHeaderData(..))
import Cardano.Chain.Block.Header
  ( AHeader
  , BlockSignature(..)
  , Header
  , HeaderHash
  , ToSign
  , decodeAHeader
  , dropBoundaryHeader
  , genesisHeaderHash
  , hashHeader
  , headerAttributes
  , headerDifficulty
  , headerEBDataProof
  , headerLeaderKey
  , headerPrevHash
  , headerProof
  , headerProtocolVersion
  , headerSignature
  , headerSlot
  , headerSoftwareVersion
  , headerToSign
  , mkHeaderExplicit
  , wrapHeaderBytes
  )
import Cardano.Chain.Block.Proof (Proof(..))
import Cardano.Chain.Common (Attributes, ChainDifficulty, mkAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting (SlotId(..))
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

instance B.Buildable Block where
  build block = bprint
    ( "Block:\n"
    . "  " . build . "  transactions (" . int . " items): " . listJson . "\n"
    . "  " . build . "\n"
    . "  " . shown . "\n"
    . "  update payload: " . build . "\n"
    . "  " . build
    )
    (blockHeader block)
    (length txs)
    txs
    (blockDlgPayload block)
    (blockSscPayload block)
    (blockUpdatePayload block)
    (blockExtraData block)
    where txs = bodyTxs $ blockBody block

instance Bi Block where
  encode block =
    encodeListLen 3
      <> encode (blockHeader block)
      <> encode (blockBody block)
      <> encode (blockExtraData block)

  decode = void <$> decodeABlock

decodeABlock :: Decoder s (ABlock ByteSpan)
decodeABlock = do
  Annotated (header, body, ed) byteSpan <-
    annotatedDecoder $ do
      enforceSize "Block" 3
      (,,) <$> decodeAHeader <*> decodeABody <*> decodeAnnotated
  pure $ ABlock header body ed byteSpan


-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
encodeBlock :: Block -> Encoding
encodeBlock block = encodeListLen 2 <> encode (1 :: Word) <> encode block


data ABlockOrBoundary a
  = ABOBBlock (ABlock a)
  | ABOBBoundary (BoundaryValidationData a)
  deriving (Functor)

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
--
--   Previous versions of Cardano had an explicit boundary block between epochs.
--   A 'Block' was then represented as 'Either BoundaryBlock MainBlock'. We have
--   now deprecated these explicit boundary blocks, but we still need to decode
--   blocks in the old format. In the case that we find a boundary block, we
--   drop it using 'dropBoundaryBlock' and return a 'Nothing'.
decodeABlockOrBoundary :: Decoder s (ABlockOrBoundary ByteSpan)
decodeABlockOrBoundary = do
  enforceSize "Block" 2
  decode @Word >>= \case
    0 -> ABOBBoundary <$> dropBoundaryBlock
    1 -> ABOBBlock <$> decodeABlock
    t -> cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)

decodeBlockOrBoundary :: Decoder s (Maybe Block)
decodeBlockOrBoundary = decodeABlockOrBoundary >>= \case
  ABOBBoundary _ -> pure Nothing
  ABOBBlock    b -> pure . Just $ void b

--------------------------------------------------------------------------------
-- BoundaryBlock
--------------------------------------------------------------------------------

data BoundaryValidationData a = BoundaryValidationData
  { boundaryBlockLength :: Int64
  -- ^ The length of the boundary block in bytes
  , boundaryPrevHash    :: HeaderHash
  -- ^ The hash of the previous block
  , boundaryHeaderBytes :: a
  -- ^ Annotation representing the header bytes
  } deriving (Functor)

-- | A decoder that drops the boundary block, but preserves the 'ByteSpan' of
--   the header for hashing
dropBoundaryBlock :: Decoder s (BoundaryValidationData ByteSpan)
dropBoundaryBlock = do
  Annotated (Annotated hh bs) (ByteSpan start end) <- annotatedDecoder $ do
    enforceSize "BoundaryBlock" 3
    aHeaderHash <- annotatedDecoder dropBoundaryHeader
    dropBoundaryBody
    dropBoundaryExtraBodyData
    pure aHeaderHash
  pure $ BoundaryValidationData
    { boundaryBlockLength = end - start
    , boundaryPrevHash    = hh
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
  -> SlotId
  -> SecretKey
  -- ^ The 'SecretKey' used for signing the block
  -> Maybe Delegation.Certificate
  -- ^ A certificate of delegation in case the 'SecretKey' does not have the
  --   right to sign this block
  -> Body
  -> Block
mkBlock pm bv sv prevHeader = mkBlockExplicit pm bv sv prevHash difficulty
 where
  prevHash   = either genesisHeaderHash hashHeader prevHeader
  difficulty = either (const 0) (succ . headerDifficulty) prevHeader

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
  -> SlotId
  -> SecretKey
  -- ^ The 'SecretKey' used for signing the block
  -> Maybe Delegation.Certificate
  -- ^ A certificate of delegation in case the 'SecretKey' does not have the
  --   right to sign this block
  -> Body
  -> Block
mkBlockExplicit pm bv sv prevHash difficulty slotId sk mDlgCert body = ABlock
  (mkHeaderExplicit pm prevHash difficulty slotId sk mDlgCert body extraH)
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

blockHash :: Block -> HeaderHash
blockHash = hashHeader . blockHeader

blockHashAnnotated :: ABlock ByteString -> HeaderHash
blockHashAnnotated = hashDecoded . fmap wrapHeaderBytes . blockHeader

blockExtraData :: ABlock a -> ExtraBodyData
blockExtraData = unAnnotated . aBlockExtraData

blockPrevHash :: ABlock a -> HeaderHash
blockPrevHash = headerPrevHash . blockHeader

blockProof :: ABlock a -> Proof
blockProof = headerProof . blockHeader

blockSlot :: ABlock a -> SlotId
blockSlot = headerSlot . blockHeader

blockLeaderKey :: ABlock a -> PublicKey
blockLeaderKey = headerLeaderKey . blockHeader

blockDifficulty :: ABlock a -> ChainDifficulty
blockDifficulty = headerDifficulty . blockHeader

blockToSign :: ABlock a -> ToSign
blockToSign = headerToSign . blockHeader

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

blockLength :: ABlock ByteString -> Int64
blockLength = fromIntegral . BS.length . blockAnnotation
