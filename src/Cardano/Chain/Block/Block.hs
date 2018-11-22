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
  ( ABlock
  , Block
  , encodeBlock
  , decodeABlock
  , decodeABlockOrBoundary
  , decodeBlockOrBoundary
  , mkBlock
  , mkBlockExplicit
  , blockPrevHash
  , blockProof
  , blockSlot
  , blockLeaderKey
  , blockDifficulty
  , blockSignature
  , blockBlockVersion
  , blockSoftwareVersion
  , blockHeaderAttributes
  , blockExtraDataProof
  , blockTxPayload
  , blockSscPayload
  , blockDlgPayload
  , blockUpdatePayload
  , blockAttributes
  , verifyBlock

       -- * BoundaryBlock
  , dropBoundaryBlock
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..), liftEither)
import Formatting (bprint, build, int, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan
  , Decoder
  , DecoderError(..)
  , Dropper
  , Encoding
  , decodeAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Block.Body
  ( ABody
  , Body
  , BodyError
  , bodyDlgPayload
  , bodySscPayload
  , bodyTxPayload
  , bodyTxs
  , bodyUpdatePayload
  , decodeABody
  , verifyBody
  )
import Cardano.Chain.Block.Boundary
  (dropBoundaryBody, dropBoundaryExtraBodyData)
import Cardano.Chain.Block.ExtraBodyData (ExtraBodyData(..))
import Cardano.Chain.Block.ExtraHeaderData (ExtraHeaderData(..))
import Cardano.Chain.Block.Header
  ( AHeader
  , BlockSignature(..)
  , Header
  , HeaderError
  , HeaderHash
  , decodeAHeader
  , dropBoundaryHeader
  , hashHeader
  , headerAttributes
  , headerBlockVersion
  , headerDifficulty
  , headerEBDataProof
  , headerLeaderKey
  , headerPrevHash
  , headerProof
  , headerSignature
  , headerSlot
  , headerSoftwareVersion
  , mkHeaderExplicit
  , verifyHeader
  )
import Cardano.Chain.Block.Proof (Proof(..), ProofError, checkProof)
import Cardano.Chain.Common (Attributes, ChainDifficulty, mkAttributes)
import Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo)
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting (SlotId(..))
import Cardano.Chain.Ssc (SscPayload)
import Cardano.Chain.Txp.TxPayload (ATxPayload)
import Cardano.Chain.Update.BlockVersion (BlockVersion)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  (Hash, ProtocolMagic, PublicKey, SecretKey, hash, hashDecoded)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------


type Block = ABlock ()

data ABlock a = ABlock
  { blockHeader     :: AHeader a
  , blockBody       :: ABody a
  , aBlockExtraData :: Annotated ExtraBodyData a
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
  enforceSize "Block" 3
  ABlock <$> decodeAHeader <*> decodeABody <*> decodeAnnotated


-- | Encode a 'Block' accounting for deprecated epoch boundary blocks
encodeBlock :: Block -> Encoding
encodeBlock block = encodeListLen 2 <> encode (1 :: Word) <> encode block

-- | Decode a 'Block' accounting for deprecated epoch boundary blocks
--
--   Previous versions of Cardano had an explicit boundary block between epochs.
--   A 'Block' was then represented as 'Either BoundaryBlock MainBlock'. We have
--   now deprecated these explicit boundary blocks, but we still need to decode
--   blocks in the old format. In the case that we find a boundary block, we
--   drop it using 'dropBoundaryBlock' and return a 'Nothing'.
decodeABlockOrBoundary :: Decoder s (Maybe (ABlock ByteSpan))
decodeABlockOrBoundary = do
  enforceSize "Block" 2
  decode @Word >>= \case
    0 -> do
      dropBoundaryBlock
      pure Nothing
    1 -> Just <$> decodeABlock
    t -> cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)

decodeBlockOrBoundary :: Decoder s (Maybe Block)
decodeBlockOrBoundary = fmap void <$> decodeABlockOrBoundary

--------------------------------------------------------------------------------
-- BoundaryBlock
--------------------------------------------------------------------------------

dropBoundaryBlock :: Dropper s
dropBoundaryBlock = do
  enforceSize "BoundaryBlock" 3
  dropBoundaryHeader
  dropBoundaryBody
  dropBoundaryExtraBodyData


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Smart constructor for 'Block'
mkBlock
  :: ProtocolMagic
  -> BlockVersion
  -> SoftwareVersion
  -> Either GenesisHash Header
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> Body
  -> Block
mkBlock pm bv sv prevHeader = mkBlockExplicit pm bv sv prevHash difficulty
 where
  prevHash   = either getGenesisHash hashHeader prevHeader
  difficulty = either (const 0) (succ . headerDifficulty) prevHeader

-- | Smart constructor for 'Block', without requiring the entire previous
--   'Header'. Instead, you give its hash and the difficulty of this block.
--   These are derived from the previous header in 'mkBlock' so if you have
--   the previous header, consider using that one.
mkBlockExplicit
  :: ProtocolMagic
  -> BlockVersion
  -> SoftwareVersion
  -> HeaderHash
  -> ChainDifficulty
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> Body
  -> Block
mkBlockExplicit pm bv sv prevHash difficulty slotId sk pske body = ABlock
  (mkHeaderExplicit pm prevHash difficulty slotId sk pske body extraH)
  body
  (Annotated extraB ())
 where
  extraB :: ExtraBodyData
  extraB = ExtraBodyData (mkAttributes ())
  extraH :: ExtraHeaderData
  extraH = ExtraHeaderData bv sv (mkAttributes ()) (hash extraB)

data BlockError
  = BlockBodyError BodyError
  | BlockHeaderError HeaderError
  | BlockInvalidExtraDataProof (Hash ExtraBodyData) (Hash ExtraBodyData)
  | BlockProofError ProofError

instance B.Buildable BlockError where
  build = \case
    BlockBodyError err ->
      bprint ("Body was invalid while checking Block.\n Error: " . build) err
    BlockHeaderError err ->
      bprint ("Header was invalid while checking Block.\n Error: " . build) err
    BlockInvalidExtraDataProof p p' -> bprint
      ( "Incorrect proof of ExtraBodyData.\n"
      . "Proof in Block:\n"
      . build . "\n"
      . "Calculated proof:\n"
      . build . "\n"
      )
      p
      p'
    BlockProofError err ->
      bprint ("Proof was invalid while checking Block.\n Error: " . build) err

verifyBlock
  :: MonadError BlockError m => ProtocolMagic -> ABlock ByteString -> m ()
verifyBlock pm block = do
  liftEither . first BlockHeaderError $ verifyHeader pm (blockHeader block)
  liftEither . first BlockBodyError $ verifyBody pm (blockBody block)
  -- No need to verify the main extra body data. It's an 'Attributes ()'
  -- which is valid whenever it's well-formed.
  --
  -- Check internal consistency: the body proofs are all correct.
  liftEither . first BlockProofError $ checkProof
    (blockBody block)
    (blockProof block)
  -- Check that the headers' extra body data hash is correct.
  -- This isn't subsumed by the body proof check.
  let extraDataHash = hashDecoded (aBlockExtraData block)
  unless (extraDataHash == blockExtraDataProof block) $ throwError
    (BlockInvalidExtraDataProof (blockExtraDataProof block) extraDataHash)


--------------------------------------------------------------------------------
-- Block accessors
--------------------------------------------------------------------------------

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

blockSignature :: ABlock a -> BlockSignature
blockSignature = headerSignature . blockHeader

blockBlockVersion :: ABlock a -> BlockVersion
blockBlockVersion = headerBlockVersion . blockHeader

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
