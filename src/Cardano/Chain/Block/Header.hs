{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Header
       ( BlockHeader
       , encodeBlockHeader
       , decodeBlockHeader
       , verifyBlockHeader
       , blockHeaderHash

       , HeaderHash
       , headerHashF

       , BlockSignature (..)

       , GenericBlockHeader
       , mkGenericBlockHeaderUnsafe
       , gbhProtocolMagic
       , gbhPrevBlock
       , gbhBodyProof
       , gbhConsensus
       , gbhExtra

       , dropBoundaryBlockHeader

       , MainBlockHeader
       , mkMainHeader
       , mkMainHeaderExplicit
       , mainHeaderPrevBlock
       , mainHeaderProof
       , mainHeaderSlot
       , mainHeaderLeaderKey
       , mainHeaderDifficulty
       , mainHeaderSignature
       , mainHeaderBlockVersion
       , mainHeaderSoftwareVersion
       , mainHeaderAttributes
       , mainHeaderEBDataProof
       , verifyMainBlockHeader

       , MainToSign (..)
       , msHeaderHash
       , msBodyProof
       , msSlot
       , msChainDiff
       , msExtraHeader

       , MainConsensusData (..)
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature
       , verifyMainConsensusData
       ) where

import           Cardano.Prelude

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError (..))
import           Formatting (Format, bprint, build, int, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), Decoder, DecoderError (..),
                     Dropper, Encoding, dropBytes, dropInt32, encodeListLen,
                     enforceSize, serializeEncoding)
import           Cardano.Chain.Block.Boundary (dropBoundaryConsensusData,
                     dropBoundaryExtraHeaderData)
import           Cardano.Chain.Block.Main (BlockHeaderAttributes, MainBody,
                     MainExtraBodyData, MainExtraHeaderData, MainProof (..),
                     mehAttributes, mehBlockVersion, mehEBDataProof,
                     mehSoftwareVersion, mkMainProof)
import           Cardano.Chain.Common (ChainDifficulty)
import           Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo,
                     ProxySigHeavy)
import           Cardano.Chain.Delegation.LightDlgIndices (LightDlgIndices (..),
                     ProxySigLight)
import           Cardano.Chain.Genesis.Hash (GenesisHash (..))
import           Cardano.Chain.Slotting (SlotId (..), slotIdF)
import           Cardano.Chain.Update.BlockVersion (BlockVersion)
import           Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import           Cardano.Crypto (Hash, ProtocolMagic (..), PublicKey, SecretKey,
                     SignTag (..), Signature, checkSig, hashHexF,
                     isSelfSignedPsk, proxySign, proxyVerify, psigPsk, sign,
                     toPublic, unsafeAbstractHash)


--------------------------------------------------------------------------------
-- BlockHeader
--------------------------------------------------------------------------------

type BlockHeader = MainBlockHeader

encodeBlockHeader :: BlockHeader -> Encoding
encodeBlockHeader header = encodeListLen 2 <> encode (1 :: Word) <> encode header

decodeBlockHeader :: Decoder s (Maybe BlockHeader)
decodeBlockHeader = do
  enforceSize "BlockHeader" 2
  decode @Word >>= \case
    0 -> do
      dropBoundaryBlockHeader
      pure Nothing
    1 -> Just <$!> decode
    t -> cborError $ DecoderErrorUnknownTag "BlockHeader" (fromIntegral t)

-- | Verify a BlockHeader in isolation. There is nothing to be done for
--   boundary headers.
verifyBlockHeader :: MonadError Text m => ProtocolMagic -> BlockHeader -> m ()
verifyBlockHeader = verifyMainBlockHeader


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

-- | 'Hash' of block header
type HeaderHash = Hash BlockHeader

-- | Specialized formatter for 'HeaderHash'
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | Hash the serialised representation of a `BlockHeader`
--
--   For backwards compatibility we have to take the hash of the header
--   serialised with 'encodeBlockHeader'
blockHeaderHash :: BlockHeader -> HeaderHash
blockHeaderHash = unsafeAbstractHash . serializeEncoding . encodeBlockHeader


--------------------------------------------------------------------------------
-- GenericBlockHeader
--------------------------------------------------------------------------------

-- | Header of block contains some kind of summary. There are various benefits
--   which people get by separating header from other data.
--
--   The constructor has `Unsafe' prefix in its name, because there in general
--   there may be some invariants which must hold for the contents of header.
data GenericBlockHeader bodyProof consensus extra = GenericBlockHeader
  { _gbhProtocolMagic :: !ProtocolMagic
  , _gbhPrevBlock     :: !HeaderHash
  -- ^ Pointer to the header of the previous block
  , _gbhBodyProof     :: !bodyProof
  -- ^ Proof of body
  , _gbhConsensus     :: !consensus
  -- ^ Consensus data to verify consensus algorithm
  , _gbhExtra         :: !extra
  -- ^ Any extra data
  } deriving (Eq, Show, Generic, NFData)

instance (Bi bodyProof, Bi consensus, Bi extra)
    => Bi (GenericBlockHeader bodyProof consensus extra)
  where
  encode bh =
    encodeListLen 5
      <> encode (getProtocolMagic (_gbhProtocolMagic bh))
      <> encode (_gbhPrevBlock bh)
      <> encode (_gbhBodyProof bh)
      <> encode (_gbhConsensus bh)
      <> encode (_gbhExtra bh)

  decode = do
    enforceSize "GenericBlockHeader b" 5
    GenericBlockHeader
      <$> (ProtocolMagic <$> decode)
      <*> decode
      <*> decode
      <*> decode
      <*> decode

-- | Export the @GenericBlockHeader@ constructor as an unsafe function for tests
mkGenericBlockHeaderUnsafe
  :: ProtocolMagic
  -> HeaderHash
  -> bodyProof
  -> consensus
  -> extra
  -> GenericBlockHeader bodyProof consensus extra
mkGenericBlockHeaderUnsafe = GenericBlockHeader


--------------------------------------------------------------------------------
-- BoundaryBlockHeader
--------------------------------------------------------------------------------

dropBoundaryBlockHeader :: Dropper s
dropBoundaryBlockHeader = do
  enforceSize "BoundaryBlockHeader" 5
  dropInt32
  -- HeaderHash
  dropBytes
  -- BoundaryBodyProof
  dropBytes
  dropBoundaryConsensusData
  dropBoundaryExtraHeaderData


--------------------------------------------------------------------------------
-- MainBlockHeader
--------------------------------------------------------------------------------

-- | Header of generic main block
type MainBlockHeader =
    GenericBlockHeader MainProof MainConsensusData MainExtraHeaderData

instance B.Buildable MainBlockHeader where
  build gbh = bprint
    ( "MainBlockHeader:\n"
    % "    hash: " % hashHexF % "\n"
    % "    previous block: " % hashHexF % "\n"
    % "    slot: " % slotIdF % "\n"
    % "    difficulty: " % int % "\n"
    % "    leader: " % build % "\n"
    % "    signature: " % build % "\n"
    % build
    )
    gbhHeaderHash
    (_gbhPrevBlock gbh)
    (_mcdSlot consensus)
    (_mcdDifficulty consensus)
    (_mcdLeaderKey consensus)
    (_mcdSignature consensus)
    (_gbhExtra gbh)
   where
    gbhHeaderHash :: HeaderHash
    gbhHeaderHash = blockHeaderHash gbh
    consensus     = _gbhConsensus gbh

-- | Smart constructor for 'MainBlockHeader'
mkMainHeader
  :: ProtocolMagic
  -> Either GenesisHash BlockHeader
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> MainBody
  -> MainExtraHeaderData
  -> MainBlockHeader
mkMainHeader pm prevHeader = mkMainHeaderExplicit pm prevHash difficulty
 where
  prevHash = either getGenesisHash blockHeaderHash prevHeader
  difficulty =
    either (const 0) (succ . _mcdDifficulty . _gbhConsensus) prevHeader

-- | Make a 'MainBlockHeader' for a given slot, with a given body, parent hash,
--   and difficulty. This takes care of some signing and consensus data.
mkMainHeaderExplicit
  :: ProtocolMagic
  -> HeaderHash
  -- ^ Parent
  -> ChainDifficulty
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> MainBody
  -> MainExtraHeaderData
  -> MainBlockHeader
mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extra =
  GenericBlockHeader pm prevHash proof consensus extra
 where
  proof = mkMainProof body
  makeSignature toSign (psk, _) =
    BlockPSignatureHeavy $ proxySign pm SignMainBlockHeavy sk psk toSign
  signature =
    let toSign = MainToSign prevHash proof slotId difficulty extra
    in
      maybe
        (BlockSignature $ sign pm SignMainBlock sk toSign)
        (makeSignature toSign)
        pske
  leaderPk  = maybe (toPublic sk) snd pske
  consensus = MainConsensusData
    { _mcdSlot       = slotId
    , _mcdLeaderKey  = leaderPk
    , _mcdDifficulty = difficulty
    , _mcdSignature  = signature
    }

-- | Verify a main block header in isolation
verifyMainBlockHeader
  :: MonadError Text m => ProtocolMagic -> MainBlockHeader -> m ()
verifyMainBlockHeader pm gbh = do
  -- Previous header hash is always valid.
  -- Body proof is just a bunch of hashes, which is always valid (although
  -- must be checked against the actual body, in verifyMainBlock.
  -- Consensus data and extra header data require validation.
  verifyMainConsensusData (_gbhConsensus gbh)
  -- verifyMainExtraHeaderData (_gbhExtra gbh)
  -- Internal consistency: is the signature in the consensus data really for
  -- this block?
  unless (verifyBlockSignature $ _mcdSignature consensus)
    $ throwError "can't verify signature"
 where
  verifyBlockSignature (BlockSignature sig) =
    checkSig pm SignMainBlock (_mcdLeaderKey consensus) signature sig
  verifyBlockSignature (BlockPSignatureLight proxySig) = proxyVerify
    pm
    SignMainBlockLight
    proxySig
    (\(LightDlgIndices (epochLow, epochHigh)) ->
      epochLow <= epochId && epochId <= epochHigh
    )
    signature
  verifyBlockSignature (BlockPSignatureHeavy proxySig) =
    proxyVerify pm SignMainBlockHeavy proxySig (const True) signature

  signature = MainToSign
    (_gbhPrevBlock gbh)
    (_gbhBodyProof gbh)
    (_mcdSlot consensus)
    (_mcdDifficulty consensus)
    (_gbhExtra gbh)

  epochId   = siEpoch $ _mcdSlot consensus

  consensus = _gbhConsensus gbh


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

-- | Signature of the block. Can be either regular signature from the issuer or
--   delegated signature having a constraint on epoch indices (it means the
--   signature is valid only if block's slot id has epoch inside the constrained
--   interval).
data BlockSignature
  = BlockSignature (Signature MainToSign)
  | BlockPSignatureLight (ProxySigLight MainToSign)
  | BlockPSignatureHeavy (ProxySigHeavy MainToSign)
  deriving (Show, Eq, Generic)

instance NFData BlockSignature

instance B.Buildable BlockSignature where
  build (BlockSignature s)       = bprint ("BlockSignature: "%build) s
  build (BlockPSignatureLight s) = bprint ("BlockPSignatureLight: "%build) s
  build (BlockPSignatureHeavy s) = bprint ("BlockPSignatureHeavy: "%build) s

instance Bi BlockSignature where
  encode input = case input of
    BlockSignature sig -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
    BlockPSignatureLight pxy ->
      encodeListLen 2 <> encode (1 :: Word8) <> encode pxy
    BlockPSignatureHeavy pxy ->
      encodeListLen 2 <> encode (2 :: Word8) <> encode pxy

  decode = do
    enforceSize "BlockSignature" 2
    decode >>= \case
      0 -> BlockSignature <$> decode
      1 -> BlockPSignatureLight <$> decode
      2 -> BlockPSignatureHeavy <$> decode
      t -> cborError $ DecoderErrorUnknownTag "BlockSignature" t

-- | Data to be signed in 'MainBlock'
data MainToSign = MainToSign
  { _msHeaderHash  :: !HeaderHash
  -- ^ Hash of previous header in the chain
  , _msBodyProof   :: !MainProof
  , _msSlot        :: !SlotId
  , _msChainDiff   :: !ChainDifficulty
  , _msExtraHeader :: !MainExtraHeaderData
  } deriving (Eq, Show, Generic)

instance Bi MainToSign where
  encode mts =
    encodeListLen 5
      <> encode (_msHeaderHash mts)
      <> encode (_msBodyProof mts)
      <> encode (_msSlot mts)
      <> encode (_msChainDiff mts)
      <> encode (_msExtraHeader mts)

  decode = do
    enforceSize "MainToSign" 5
    MainToSign <$> decode <*> decode <*> decode <*> decode <*> decode


--------------------------------------------------------------------------------
-- MainConsensusData
--------------------------------------------------------------------------------

data MainConsensusData = MainConsensusData
  { _mcdSlot       :: !SlotId
  -- ^ Id of the slot for which this block was generated
  , _mcdLeaderKey  :: !PublicKey
  -- ^ Public key of the slot leader. It's essential to have it here, because FTS
  --   gives us only hash of public key (aka 'StakeholderId').
  , _mcdDifficulty :: !ChainDifficulty
  -- ^ Difficulty of chain ending in this block
  , _mcdSignature  :: !BlockSignature
  -- ^ Signature given by slot leader
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData

instance Bi MainConsensusData where
  encode cd =
    encodeListLen 4
      <> encode (_mcdSlot cd)
      <> encode (_mcdLeaderKey cd)
      <> encode (_mcdDifficulty cd)
      <> encode (_mcdSignature cd)

  decode = do
    enforceSize "ConsensusData MainBlockchain)" 4
    MainConsensusData <$> decode <*> decode <*> decode <*> decode

-- | Verify the consensus data in isolation
verifyMainConsensusData :: MonadError Text m => MainConsensusData -> m ()
verifyMainConsensusData mcd = when (selfSignedProxy $ _mcdSignature mcd)
  $ throwError "can't use self-signed psk to issue the block"
 where
  selfSignedProxy (BlockSignature       _  ) = False
  selfSignedProxy (BlockPSignatureLight sig) = isSelfSignedPsk $ psigPsk sig
  selfSignedProxy (BlockPSignatureHeavy sig) = isSelfSignedPsk $ psigPsk sig


--------------------------------------------------------------------------------
-- GenericBlockHeader Lenses
--------------------------------------------------------------------------------

makeLenses ''GenericBlockHeader


--------------------------------------------------------------------------------
-- MainConsensusData lenses
--------------------------------------------------------------------------------

makeLenses 'MainConsensusData


--------------------------------------------------------------------------------
-- MainBlockHeader lenses
--------------------------------------------------------------------------------

-- | Lens from 'MainBlockHeader' to 'HeaderHash' of its parent
mainHeaderPrevBlock :: Lens' MainBlockHeader HeaderHash
mainHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'MainBlockHeader' to 'MainProof'
mainHeaderProof :: Lens' MainBlockHeader MainProof
mainHeaderProof = gbhBodyProof

-- | Lens from 'MainBlockHeader' to 'SlotId'
mainHeaderSlot :: Lens' MainBlockHeader SlotId
mainHeaderSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlockHeader' to 'PublicKey'
mainHeaderLeaderKey :: Lens' MainBlockHeader PublicKey
mainHeaderLeaderKey = gbhConsensus . mcdLeaderKey

-- | Lens from 'MainBlockHeader' to 'ChainDifficulty'
mainHeaderDifficulty :: Lens' MainBlockHeader ChainDifficulty
mainHeaderDifficulty = gbhConsensus . mcdDifficulty

-- | Lens from 'MainBlockHeader' to 'Signature'
mainHeaderSignature :: Lens' MainBlockHeader BlockSignature
mainHeaderSignature = gbhConsensus . mcdSignature

-- | Lens from 'MainBlockHeader' to 'BlockVersion'
mainHeaderBlockVersion :: Lens' MainBlockHeader BlockVersion
mainHeaderBlockVersion = gbhExtra . mehBlockVersion

-- | Lens from 'MainBlockHeader' to 'SoftwareVersion'
mainHeaderSoftwareVersion :: Lens' MainBlockHeader SoftwareVersion
mainHeaderSoftwareVersion = gbhExtra . mehSoftwareVersion

-- | Lens from 'MainBlockHeader' to 'BlockHeaderAttributes'
mainHeaderAttributes :: Lens' MainBlockHeader BlockHeaderAttributes
mainHeaderAttributes = gbhExtra . mehAttributes

-- | Lens from 'MainBlockHeader' to 'MainExtraBodyData'
mainHeaderEBDataProof :: Lens' MainBlockHeader (Hash MainExtraBodyData)
mainHeaderEBDataProof = gbhExtra . mehEBDataProof


----------------------------------------------------------------------------
-- MainToSign lenses
----------------------------------------------------------------------------

makeLenses ''MainToSign
