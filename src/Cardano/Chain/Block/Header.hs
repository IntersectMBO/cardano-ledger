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
       ( BlockHeader (..)
       , blockHeaderProtocolMagic
       , blockHeaderHash
       , blockHeaderDifficulty
       , choosingBlockHeader
       , _BlockHeaderBoundary
       , _BlockHeaderMain
       , verifyBlockHeader

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

       , BoundaryBlockHeader
       , mkBoundaryHeader
       , genHeaderPrevBlock
       , genHeaderProof
       , genHeaderEpoch
       , genHeaderDifficulty
       , genHeaderAttributes

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

import           Codec.CBOR.Decoding (decodeWordCanonical)
import           Codec.CBOR.Encoding (encodeWord)
import           Control.Lens (LensLike', makeLenses, makePrisms)
import           Control.Monad.Except (MonadError (..))
import           Formatting (Format, bprint, build, int, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), DecoderError (..),
                     decodeListLenCanonicalOf, encodeListLen, enforceSize)
import           Cardano.Chain.Block.Boundary (BoundaryBody,
                     BoundaryConsensusData (..), BoundaryExtraHeaderData (..),
                     BoundaryHeaderAttributes, BoundaryProof (..),
                     gcdDifficulty, gcdEpoch, gehAttributes, mkBoundaryProof)
import           Cardano.Chain.Block.Main (BlockHeaderAttributes, MainBody,
                     MainExtraBodyData, MainExtraHeaderData, MainProof (..),
                     mehAttributes, mehBlockVersion, mehEBDataProof,
                     mehSoftwareVersion, mkMainProof)
import           Cardano.Chain.Common (ChainDifficulty)
import           Cardano.Chain.Common.Attributes (mkAttributes)
import           Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo,
                     ProxySigHeavy)
import           Cardano.Chain.Delegation.LightDlgIndices (LightDlgIndices (..),
                     ProxySigLight)
import           Cardano.Chain.Genesis.Hash (GenesisHash (..))
import           Cardano.Chain.Slotting (EpochIndex (..), SlotId (..), slotIdF)
import           Cardano.Chain.Update.BlockVersion (BlockVersion)
import           Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import           Cardano.Crypto (Hash, ProtocolMagic (..), PublicKey, SecretKey,
                     SignTag (..), Signature, checkSig, hash, hashHexF,
                     isSelfSignedPsk, proxySign, proxyVerify, psigPsk, sign,
                     toPublic)


--------------------------------------------------------------------------------
-- BoundaryBlock ∪ MainBlock
--------------------------------------------------------------------------------

-- | Either header of ordinary main block or boundary block
data BlockHeader
    = BlockHeaderBoundary BoundaryBlockHeader
    | BlockHeaderMain MainBlockHeader
    deriving (Eq, Show, Generic, NFData)

instance B.Buildable BlockHeader where
  build = \case
    BlockHeaderBoundary bhg -> B.build bhg
    BlockHeaderMain     bhm -> B.build bhm

choosingBlockHeader
  :: Functor f
  => LensLike' f BoundaryBlockHeader r
  -> LensLike' f MainBlockHeader r
  -> LensLike' f BlockHeader r
choosingBlockHeader onBoundary onMain f = \case
  BlockHeaderBoundary bh -> BlockHeaderBoundary <$> onBoundary f bh
  BlockHeaderMain     bh -> BlockHeaderMain <$> onMain f bh

instance Bi BlockHeader where
  encode x = encodeListLen 2 <> encodeWord tag <> body
   where
    (tag, body) = case x of
      BlockHeaderBoundary bh -> (0, encode bh)
      BlockHeaderMain     bh -> (1, encode bh)

  decode = do
    decodeListLenCanonicalOf 2
    t <- decodeWordCanonical
    case t of
      0 -> BlockHeaderBoundary <$!> decode
      1 -> BlockHeaderMain <$!> decode
      _ -> cborError $ DecoderErrorUnknownTag "BlockHeader" (fromIntegral t)

-- | The 'ProtocolMagic' in a 'BlockHeader'
blockHeaderProtocolMagic :: BlockHeader -> ProtocolMagic
blockHeaderProtocolMagic (BlockHeaderBoundary gbh) = _gbhProtocolMagic gbh
blockHeaderProtocolMagic (BlockHeaderMain     mbh) = _gbhProtocolMagic mbh

blockHeaderDifficulty :: BlockHeader -> ChainDifficulty
blockHeaderDifficulty (BlockHeaderBoundary gbh) =
  _gcdDifficulty $ _gbhConsensus gbh
blockHeaderDifficulty (BlockHeaderMain gbh) =
  _mcdDifficulty $ _gbhConsensus gbh

-- | Verify a BlockHeader in isolation. There is nothing to be done for
--   boundary headers.
verifyBlockHeader :: MonadError Text m => ProtocolMagic -> BlockHeader -> m ()
verifyBlockHeader _  (BlockHeaderBoundary _  ) = pure ()
verifyBlockHeader pm (BlockHeaderMain     bhm) = verifyMainBlockHeader pm bhm


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

-- | 'Hash' of block header
type HeaderHash = Hash BlockHeader

-- | Specialized formatter for 'HeaderHash'
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | This function is required because type inference fails in attempts to hash
--   only @Right@ or @Left@.
--
--   Perhaps, it shouldn't be here, but I decided not to create a module for
--   only this function.
blockHeaderHash :: BlockHeader -> HeaderHash
blockHeaderHash = hash


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

-- | Header of Boundary block
type BoundaryBlockHeader = GenericBlockHeader
    BoundaryProof
    BoundaryConsensusData
    BoundaryExtraHeaderData

instance B.Buildable BoundaryBlockHeader where
  build gbh = bprint
    ( "BoundaryBlockHeader:\n"
    % "    hash: " % hashHexF % "\n"
    % "    previous block: " % hashHexF % "\n"
    % "    epoch: " % build % "\n"
    % "    difficulty: " % int % "\n"
    )
    gbhHeaderHash
    (_gbhPrevBlock gbh)
    (_gcdEpoch consensus)
    (_gcdDifficulty consensus)
   where
    gbhHeaderHash :: HeaderHash
    gbhHeaderHash = blockHeaderHash $ BlockHeaderBoundary gbh
    consensus     = _gbhConsensus gbh

-- | Smart constructor for 'BoundaryBlockHeader'
mkBoundaryHeader
  :: ProtocolMagic
  -> Either GenesisHash BlockHeader
  -> EpochIndex
  -> BoundaryBody
  -> BoundaryBlockHeader
mkBoundaryHeader pm prevHeader epoch body = GenericBlockHeader
  pm
  (either getGenesisHash blockHeaderHash prevHeader)
  (mkBoundaryProof body)
  consensus
  (BoundaryExtraHeaderData $ mkAttributes ())
 where
  difficulty = either (const 0) blockHeaderDifficulty prevHeader
  consensus =
    BoundaryConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty}


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
    gbhHeaderHash = blockHeaderHash $ BlockHeaderMain gbh
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
  prevHash   = either getGenesisHash blockHeaderHash prevHeader
  difficulty = either (const 0) (succ . blockHeaderDifficulty) prevHeader

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
-- BlockHeaderBoundary prisms
--------------------------------------------------------------------------------

makePrisms 'BlockHeaderBoundary


--------------------------------------------------------------------------------
-- GenericBlockHeader Lenses
--------------------------------------------------------------------------------

makeLenses ''GenericBlockHeader


--------------------------------------------------------------------------------
-- MainConsensusData lenses
--------------------------------------------------------------------------------

makeLenses 'MainConsensusData


----------------------------------------------------------------------------
-- BoundaryBlockHeader lenses
----------------------------------------------------------------------------

-- | Lens from 'BoundaryBlockHeader' to 'HeaderHash' of its parent
genHeaderPrevBlock :: Lens' BoundaryBlockHeader HeaderHash
genHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'BoundaryBlockHeader' to 'BoundaryProof'
genHeaderProof :: Lens' BoundaryBlockHeader BoundaryProof
genHeaderProof = gbhBodyProof

-- | Lens from 'BoundaryBlockHeader' to 'EpochIndex'
genHeaderEpoch :: Lens' BoundaryBlockHeader EpochIndex
genHeaderEpoch = gbhConsensus . gcdEpoch

-- | Lens from 'BoundaryBlockHeader' to 'ChainDifficulty'
genHeaderDifficulty :: Lens' BoundaryBlockHeader ChainDifficulty
genHeaderDifficulty = gbhConsensus . gcdDifficulty

-- | Lens from 'BoundaryBlockHeader' to 'BoundaryHeaderAttributes'
genHeaderAttributes :: Lens' BoundaryBlockHeader BoundaryHeaderAttributes
genHeaderAttributes = gbhExtra . gehAttributes


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
