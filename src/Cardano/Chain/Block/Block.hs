{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- for the Getter instances

module Cardano.Chain.Block.Block
       ( Block
       , encodeBlock
       , decodeBlock

       -- * GenericBlock
       , GenericBlock
       , mkGenericBlockUnsafe
       , gbHeader
       , gbBody
       , gbExtra
       , gbPrevBlock
       , gbBodyProof
       , gbConsensus

       -- * BoundaryBlock
       , dropBoundaryBlock

       -- * MainBlock
       , MainBlock
       , mkMainBlock
       , mkMainBlockExplicit
       , mainBlockPrevBlock
       , mainBlockProof
       , mainBlockSlot
       , mainBlockLeaderKey
       , mainBlockDifficulty
       , mainBlockSignature
       , mainBlockBlockVersion
       , mainBlockSoftwareVersion
       , mainBlockHeaderAttributes
       , mainBlockEBDataProof
       , mainBlockTxPayload
       , mainBlockSscPayload
       , mainBlockDlgPayload
       , mainBlockUpdatePayload
       , mainBlockAttributes
       , verifyMainBlock
       ) where

import           Cardano.Prelude

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError (..))
import           Formatting (bprint, build, int, shown, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), Decoder, DecoderError (..),
                     Dropper, Encoding, encodeListLen, enforceSize)
import           Cardano.Chain.Block.Boundary (dropBoundaryBody,
                     dropBoundaryExtraBodyData)
import           Cardano.Chain.Block.Header (BlockHeader, BlockSignature (..),
                     GenericBlockHeader, HeaderHash, MainConsensusData (..),
                     blockHeaderHash, dropBoundaryBlockHeader, gbhBodyProof,
                     gbhConsensus, gbhPrevBlock, mainHeaderAttributes,
                     mainHeaderBlockVersion, mainHeaderDifficulty,
                     mainHeaderEBDataProof, mainHeaderLeaderKey,
                     mainHeaderProof, mainHeaderSignature, mainHeaderSlot,
                     mainHeaderSoftwareVersion, mkMainHeaderExplicit,
                     verifyMainBlockHeader)
import           Cardano.Chain.Block.Main (BlockBodyAttributes,
                     BlockHeaderAttributes, MainBody (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), checkMainProof, mbDlgPayload,
                     mbSscPayload, mbTxPayload, mbTxs, mbUpdatePayload,
                     mebAttributes, verifyMainBody)
import           Cardano.Chain.Common (ChainDifficulty, mkAttributes)
import           Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo)
import qualified Cardano.Chain.Delegation.Payload as Delegation (Payload)
import           Cardano.Chain.Genesis.Hash (GenesisHash (..))
import           Cardano.Chain.Slotting (SlotId (..))
import           Cardano.Chain.Ssc (SscPayload)
import           Cardano.Chain.Txp.TxPayload (TxPayload)
import           Cardano.Chain.Update.BlockVersion (BlockVersion)
import qualified Cardano.Chain.Update.Payload as Update (Payload)
import           Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import           Cardano.Crypto (Hash, ProtocolMagic, PublicKey, SecretKey,
                     hash)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

type Block = MainBlock

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
-- GenericBlock
--------------------------------------------------------------------------------

-- | In general Block consists of header and body. It may contain extra data as
--   well
data GenericBlock bodyProof consensus extraH body extraB = GenericBlock
  { _gbHeader :: !(GenericBlockHeader bodyProof consensus extraH)
  , _gbBody   :: !body
  , _gbExtra  :: !extraB
  } deriving (Eq, Show, Generic, NFData)

instance
    (Bi bodyProof , Bi consensus, Bi extraH, Bi body, Bi extraB)
    => Bi (GenericBlock bodyProof consensus extraH body extraB)
  where
  encode gb =
    encodeListLen 3 <> encode (_gbHeader gb) <> encode (_gbBody gb) <> encode
      (_gbExtra gb)

  decode = do
    enforceSize "GenericBlock" 3
    GenericBlock <$> decode <*> decode <*> decode

mkGenericBlockUnsafe
  :: GenericBlockHeader bodyProof consensus extraH
  -> body
  -> extraB
  -> GenericBlock bodyProof consensus extraH body extraB
mkGenericBlockUnsafe = GenericBlock


--------------------------------------------------------------------------------
-- BoundaryBlock
--------------------------------------------------------------------------------

dropBoundaryBlock :: Dropper s
dropBoundaryBlock = do
  enforceSize "BoundaryBlock" 3
  dropBoundaryBlockHeader
  dropBoundaryBody
  dropBoundaryExtraBodyData


--------------------------------------------------------------------------------
-- MainBlock
--------------------------------------------------------------------------------

-- | MainBlock is a block with transactions and MPC messages
type MainBlock = GenericBlock
    MainProof
    MainConsensusData
    MainExtraHeaderData
    MainBody
    MainExtraBodyData

-- | Smart constructor for 'MainBlock'
mkMainBlock
  :: ProtocolMagic
  -> BlockVersion
  -> SoftwareVersion
  -> Either GenesisHash BlockHeader
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> MainBody
  -> MainBlock
mkMainBlock pm bv sv prevHeader = mkMainBlockExplicit
  pm
  bv
  sv
  prevHash
  difficulty
 where
  prevHash = either getGenesisHash blockHeaderHash prevHeader
  difficulty =
    either (const 0) (succ . _mcdDifficulty . view gbhConsensus) prevHeader

-- | Smart constructor for 'MainBlock', without requiring the entire previous
--   'BlockHeader'. Instead, you give its hash and the difficulty of this block.
--   These are derived from the previous header in 'mkMainBlock' so if you have
--   the previous header, consider using that one.
mkMainBlockExplicit
  :: ProtocolMagic
  -> BlockVersion
  -> SoftwareVersion
  -> HeaderHash
  -> ChainDifficulty
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> MainBody
  -> MainBlock
mkMainBlockExplicit pm bv sv prevHash difficulty slotId sk pske body =
  GenericBlock
    (mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extraH)
    body
    extraB
 where
  extraB :: MainExtraBodyData
  extraB = MainExtraBodyData (mkAttributes ())
  extraH :: MainExtraHeaderData
  extraH = MainExtraHeaderData bv sv (mkAttributes ()) (hash extraB)

verifyMainBlock :: MonadError Text m => ProtocolMagic -> MainBlock -> m ()
verifyMainBlock pm gb = do
  verifyMainBlockHeader pm (_gbHeader gb)
  verifyMainBody pm (_gbBody gb)
  -- No need to verify the main extra body data. It's an 'Attributes ()'
  -- which is valid whenever it's well-formed.
  --
  -- Check internal consistency: the body proofs are all correct.
  checkMainProof (_gbBody gb) (_gbHeader gb ^. gbhBodyProof)
  -- Check that the headers' extra body data hash is correct.
  -- This isn't subsumed by the body proof check.
  unless (hash (_gbExtra gb) == (_gbHeader gb ^. mainHeaderEBDataProof))
    $ throwError
        "Hash of extra body data is not equal to its representation in the header."


--------------------------------------------------------------------------------
-- Generic Block Lenses
---------------------------------------------------------------------------

makeLenses ''GenericBlock

-- | Lens from 'GenericBlock' to 'BHeaderHash' of its parent
gbPrevBlock :: Lens' (GenericBlock a b c d e) HeaderHash
gbPrevBlock = gbHeader . gbhPrevBlock

-- | Lens from 'GenericBlock' to 'BodyProof'
gbBodyProof :: Lens' (GenericBlock bodyProof b c d e) bodyProof
gbBodyProof = gbHeader . gbhBodyProof

-- | Lens from 'GenericBlock' to 'ConsensusData'
gbConsensus :: Lens' (GenericBlock a consensus c d e) consensus
gbConsensus = gbHeader . gbhConsensus


--------------------------------------------------------------------------------
-- MainBlock lenses
--------------------------------------------------------------------------------

-- | Lens from 'MainBlock' to 'HeaderHash' of its parent
mainBlockPrevBlock :: Lens' MainBlock HeaderHash
mainBlockPrevBlock = gbPrevBlock

-- | Lens from 'MainBlock' to 'MainProof'
mainBlockProof :: Lens' MainBlock MainProof
mainBlockProof = gbHeader . mainHeaderProof

-- | Lens from 'MainBlock' to 'SlotId'
mainBlockSlot :: Lens' MainBlock SlotId
mainBlockSlot = gbHeader . mainHeaderSlot

-- | Lens from 'MainBlock' to 'PublicKey'
mainBlockLeaderKey :: Lens' MainBlock PublicKey
mainBlockLeaderKey = gbHeader . mainHeaderLeaderKey

-- | Lens from 'MainBlock' to 'ChainDifficulty'
mainBlockDifficulty :: Lens' MainBlock ChainDifficulty
mainBlockDifficulty = gbHeader . mainHeaderDifficulty

-- | Lens from 'MainBlock' to 'Signature'
mainBlockSignature :: Lens' MainBlock BlockSignature
mainBlockSignature = gbHeader . mainHeaderSignature

-- | Lens from 'MainBlock' to 'BlockVersion'
mainBlockBlockVersion :: Lens' MainBlock BlockVersion
mainBlockBlockVersion = gbHeader . mainHeaderBlockVersion

-- | Lens from 'MainBlock' to 'SoftwareVersion'
mainBlockSoftwareVersion :: Lens' MainBlock SoftwareVersion
mainBlockSoftwareVersion = gbHeader . mainHeaderSoftwareVersion

-- | Lens from 'MainBlock' to 'BlockHeaderAttributes'
mainBlockHeaderAttributes :: Lens' MainBlock BlockHeaderAttributes
mainBlockHeaderAttributes = gbHeader . mainHeaderAttributes

-- | Lens from 'MainBlock' to proof (hash) of 'MainExtraBodyData'
mainBlockEBDataProof :: Lens' MainBlock (Hash MainExtraBodyData)
mainBlockEBDataProof = gbHeader . mainHeaderEBDataProof

-- | Lens from 'MainBlock' to 'TxPayload'
mainBlockTxPayload :: Lens' MainBlock TxPayload
mainBlockTxPayload = gbBody . mbTxPayload

-- | Lens from 'MainBlock' to 'SscPayload'
mainBlockSscPayload :: Lens' MainBlock SscPayload
mainBlockSscPayload = gbBody . mbSscPayload

-- | Lens from 'MainBlock' to 'Update.Payload'
mainBlockUpdatePayload :: Lens' MainBlock Update.Payload
mainBlockUpdatePayload = gbBody . mbUpdatePayload

-- | Lens from 'MainBlock' to 'Delegation.Payload'
mainBlockDlgPayload :: Lens' MainBlock Delegation.Payload
mainBlockDlgPayload = gbBody . mbDlgPayload

-- | Lens from 'MainBlock' to 'BlockBodyAttributes'
mainBlockAttributes :: Lens' MainBlock BlockBodyAttributes
mainBlockAttributes = gbExtra . mebAttributes

instance B.Buildable MainBlock where
  build mainBlock = bprint
    ( "MainBlock:\n"
    % "  " % build % "  transactions (" % int % " items): " % listJson % "\n"
    % "  " % build % "\n"
    % "  " % shown % "\n"
    % "  update payload: " % build % "\n"
    % "  " % build
    )
    (mainBlock ^. gbHeader)
    (length txs)
    txs
    (mainBlock ^. mainBlockDlgPayload)
    (mainBlock ^. mainBlockSscPayload)
    (mainBlock ^. mainBlockUpdatePayload)
    (mainBlock ^. gbExtra)
    where txs = mainBlock ^. gbBody . mbTxs
