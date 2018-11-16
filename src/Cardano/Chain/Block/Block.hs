{-# LANGUAGE DeriveAnyClass       #-}
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
  ( Block
  , encodeBlock
  , decodeBlock
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
  ( Bi(..)
  , Decoder
  , DecoderError(..)
  , Dropper
  , Encoding
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Block.Body (Body(..), BodyError, bodyTxs, verifyBody)
import Cardano.Chain.Block.Boundary
  (dropBoundaryBody, dropBoundaryExtraBodyData)
import Cardano.Chain.Block.ExtraBodyData (ExtraBodyData(..))
import Cardano.Chain.Block.ExtraHeaderData (ExtraHeaderData(..))
import Cardano.Chain.Block.Header
  ( BlockSignature(..)
  , Header(..)
  , HeaderError
  , HeaderHash
  , dropBoundaryHeader
  , hashHeader
  , headerAttributes
  , headerBlockVersion
  , headerDifficulty
  , headerEBDataProof
  , headerLeaderKey
  , headerSignature
  , headerSlot
  , headerSoftwareVersion
  , mkHeaderExplicit
  , verifyHeader
  )
import Cardano.Chain.Block.Proof (Proof(..), ProofError, checkProof)
import Cardano.Chain.Common (Attributes, ChainDifficulty, mkAttributes)
import Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo)
import qualified Cardano.Chain.Delegation.Payload as Delegation (Payload)
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting (SlotId(..))
import Cardano.Chain.Ssc (SscPayload)
import Cardano.Chain.Txp.TxPayload (TxPayload)
import Cardano.Chain.Update.BlockVersion (BlockVersion)
import qualified Cardano.Chain.Update.Payload as Update (Payload)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto (Hash, ProtocolMagic, PublicKey, SecretKey, hash)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data Block = Block
  { blockHeader    :: Header
  , blockBody      :: Body
  , blockExtraData :: ExtraBodyData
  } deriving (Eq, Show, Generic, NFData)

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

  decode = do
    enforceSize "Block" 3
    Block <$> decode <*> decode <*> decode

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
decodeBlock :: Decoder s (Maybe Block)
decodeBlock = do
  enforceSize "Block" 2
  decode @Word >>= \case
    0 -> do
      dropBoundaryBlock
      pure Nothing
    1 -> Just <$> decode
    t -> cborError $ DecoderErrorUnknownTag "Block" (fromIntegral t)


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
mkBlockExplicit pm bv sv prevHash difficulty slotId sk pske body = Block
  (mkHeaderExplicit pm prevHash difficulty slotId sk pske body extraH)
  body
  extraB
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

verifyBlock :: MonadError BlockError m => ProtocolMagic -> Block -> m ()
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
  let extraDataHash = hash $ blockExtraData block
  unless (extraDataHash == blockExtraDataProof block) $ throwError
    (BlockInvalidExtraDataProof (blockExtraDataProof block) extraDataHash)


--------------------------------------------------------------------------------
-- Block lenses
--------------------------------------------------------------------------------

blockPrevHash :: Block -> HeaderHash
blockPrevHash = headerPrevHash . blockHeader

blockProof :: Block -> Proof
blockProof = headerProof . blockHeader

blockSlot :: Block -> SlotId
blockSlot = headerSlot . blockHeader

blockLeaderKey :: Block -> PublicKey
blockLeaderKey = headerLeaderKey . blockHeader

blockDifficulty :: Block -> ChainDifficulty
blockDifficulty = headerDifficulty . blockHeader

blockSignature :: Block -> BlockSignature
blockSignature = headerSignature . blockHeader

blockBlockVersion :: Block -> BlockVersion
blockBlockVersion = headerBlockVersion . blockHeader

blockSoftwareVersion :: Block -> SoftwareVersion
blockSoftwareVersion = headerSoftwareVersion . blockHeader

blockHeaderAttributes :: Block -> Attributes ()
blockHeaderAttributes = headerAttributes . blockHeader

blockExtraDataProof :: Block -> Hash ExtraBodyData
blockExtraDataProof = headerEBDataProof . blockHeader

blockTxPayload :: Block -> TxPayload
blockTxPayload = bodyTxPayload . blockBody

blockSscPayload :: Block -> SscPayload
blockSscPayload = bodySscPayload . blockBody

blockUpdatePayload :: Block -> Update.Payload
blockUpdatePayload = bodyUpdatePayload . blockBody

blockDlgPayload :: Block -> Delegation.Payload
blockDlgPayload = bodyDlgPayload . blockBody

blockAttributes :: Block -> Attributes ()
blockAttributes = ebdAttributes . blockExtraData
