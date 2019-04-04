{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Header
  ( Header
  , AHeader
  , headerPrevHash
  , headerProof
  , mkHeader
  , mkHeaderExplicit
  , wrapHeaderBytes

  -- * Accessors
  , headerSlot
  , headerLeaderKey
  , headerLength
  , headerDifficulty
  , headerSignature
  , headerProtocolVersion
  , headerSoftwareVersion
  , headerAttributes
  , headerEBDataProof
  , headerToSign
  , recoverSignedBytes

  -- * Boundary Header
  , dropBoundaryHeader
  , wrapBoundaryBytes
  , toCBORHeader
  , toCBORHeader'
  , fromCBORHeader
  , fromCBORHeader'
  , fromCBORAHeader
  , HeaderError(..)
  , verifyHeader
  , HeaderHash
  , headerHashF
  , hashHeader
  , BlockSignature(..)
  , ToSign(..)
  , ConsensusData
  , consensusData

  -- * 'ConsensusData' encoding and decoding
  , toCBORConsensusData
  , fromCBORConsensusData

  -- * Utility functions
  , genesisHeaderHash
  , renderHeader
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, build, int)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , Decoded(..)
  , Decoder
  , DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , ToCBOR(..)
  , annotatedDecoder
  , fromCBORAnnotated
  , dropBytes
  , dropInt32
  , encodeListLen
  , enforceSize
  , serializeEncoding
  )
import Cardano.Chain.Block.Body (Body)
import Cardano.Chain.Block.Boundary
  (dropBoundaryConsensusData, dropBoundaryExtraHeaderData)
import Cardano.Chain.Block.ExtraBodyData (ExtraBodyData)
import Cardano.Chain.Block.ExtraHeaderData
  (ExtraHeaderData(..), ExtraHeaderDataError, verifyExtraHeaderData)
import Cardano.Chain.Block.Proof (Proof(..), mkProof)
import Cardano.Chain.Common (Attributes, ChainDifficulty(..))
import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting
  ( EpochIndex
  , EpochSlots
  , FlatSlotId(..)
  , SlotId(..)
  , WithEpochSlots(WithEpochSlots)
  , flattenSlotId
  , unflattenSlotId
  )
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId(..)
  , ProxySignature
  , PublicKey
  , SecretKey
  , SignTag(..)
  , hashHexF
  , proxySign
  , proxyVerifyDecoded
  , pskIssuerPk
  , unsafeAbstractHash
  )


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

type Header = AHeader ()

data AHeader a = AHeader
  { headerProtocolMagicId :: !ProtocolMagicId
  , aHeaderPrevHash     :: !(Annotated HeaderHash a)
  -- ^ Pointer to the header of the previous block
  , aHeaderProof        :: !(Annotated Proof a)
  -- ^ Proof of body
  , headerConsensusData :: !(AConsensusData a)
  -- ^ Consensus data to verify consensus algorithm
  , aHeaderExtraData    :: !(Annotated ExtraHeaderData a)
  -- ^ Any extra data
  , headerAnnotation    :: a
  } deriving (Eq, Show, Generic, NFData, Functor)

headerPrevHash :: AHeader a -> HeaderHash
headerPrevHash = unAnnotated . aHeaderPrevHash

headerProof :: AHeader a -> Proof
headerProof = unAnnotated . aHeaderProof

headerExtraData :: AHeader a -> ExtraHeaderData
headerExtraData = unAnnotated . aHeaderExtraData

instance B.Buildable (WithEpochSlots Header) where
  build (WithEpochSlots es header) = renderHeader es header

renderHeader :: EpochSlots -> Header -> Builder
renderHeader es header = bprint
  ( "Header:\n"
  . "    hash: "
  . hashHexF
  . "\n"
  . "    previous block: "
  . hashHexF
  . "\n"
  . "    slot: "
  . build
  . "\n"
  . "    difficulty: "
  . int
  . "\n"
  . "    leader: "
  . build
  . "\n"
  . "    signature: "
  . build
  . "\n"
  . build
  )
  headerHash
  (headerPrevHash header)
  (consensusSlot consensus)
  (unChainDifficulty $ consensusDifficulty consensus)
  (consensusLeaderKey consensus)
  (consensusSignature consensus)
  (headerExtraData header)
 where
  headerHash :: HeaderHash
  headerHash = hashHeader es header
  consensus  = headerConsensusData header

-- | Encode a header, without taking in to account deprecated epoch boundary
-- blocks.
toCBORHeader' :: EpochSlots -> Header -> Encoding
toCBORHeader' es h =
  encodeListLen 5
    <> toCBOR (headerProtocolMagicId h)
    <> toCBOR (headerPrevHash h)
    <> toCBOR (headerProof h)
    <> toCBORConsensusData es (headerConsensusData h)
    <> toCBOR (headerExtraData h)

fromCBORHeader' :: EpochSlots -> Decoder s Header
fromCBORHeader' epochSlots = void <$> fromCBORAHeader epochSlots

fromCBORAHeader :: EpochSlots -> Decoder s (AHeader ByteSpan)
fromCBORAHeader epochSlots = do
  Annotated (pm, prevHash, proof, cd, extraData) byteSpan <-
    annotatedDecoder $ do
      enforceSize "Header" 5
      (,,,,)
        <$> fromCBOR
        <*> fromCBORAnnotated
        <*> fromCBORAnnotated
        <*> fromCBORAConsensus epochSlots
        <*> fromCBORAnnotated
  pure $ AHeader pm prevHash proof cd extraData byteSpan

instance Decoded (AHeader ByteString) where
  type BaseType (AHeader ByteString) = Header
  recoverBytes = headerAnnotation


-- | Smart constructor for 'Header'
mkHeader
  :: ProtocolMagicId
  -> Either GenesisHash Header
  -> EpochSlots
  -- ^ Number of slots per epoch. This is needed to convert the slot number to
  -- the legacy format used in 'ToSign', where a slot is identified by the
  -- epoch to which it belongs and the offset within that epoch (counted in
  -- number of slots).
  -> FlatSlotId
  -> SecretKey
  -- ^ The 'SecretKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SecretKey'
  -> Body
  -> ExtraHeaderData
  -> Header
mkHeader pm prevHeader epochSlots = mkHeaderExplicit
  pm
  prevHash
  difficulty
  epochSlots
 where
  prevHash   = either genesisHeaderHash (hashHeader epochSlots) prevHeader
  difficulty = either
    (const $ ChainDifficulty 0)
    (succ . consensusDifficulty . headerConsensusData)
    prevHeader

-- | Extract the genesis hash and cast it into a header hash.
genesisHeaderHash :: GenesisHash -> HeaderHash
genesisHeaderHash = coerce . unGenesisHash

-- | Make a 'Header' for a given slot, with a given body, parent hash,
--   and difficulty. This takes care of some signing and consensus data.
mkHeaderExplicit
  :: ProtocolMagicId
  -> HeaderHash
  -- ^ Parent
  -> ChainDifficulty
  -> EpochSlots
  -- ^ See 'mkHeader'.
  -> FlatSlotId
  -> SecretKey
  -- ^ The 'SecretKey' used for signing the block
  -> Delegation.Certificate
  -- ^ A certificate of delegation from a genesis key to the 'SecretKey'
  -> Body
  -> ExtraHeaderData
  -> Header
mkHeaderExplicit pm prevHash difficulty epochSlots slotId sk dlgCert body extra
  = AHeader
    pm
    (Annotated prevHash ())
    (Annotated proof ())
    consensus
    (Annotated extra ())
    ()
 where
  proof  = mkProof body

  toSign = ToSign prevHash proof epochAndSlotCount difficulty extra

  epochAndSlotCount = unflattenSlotId epochSlots slotId

  signature =
    BlockSignature $ proxySign pm SignMainBlockHeavy sk dlgCert toSign

  leaderPk  = pskIssuerPk dlgCert

  consensus = consensusData slotId leaderPk difficulty signature

headerSlot :: AHeader a -> FlatSlotId
headerSlot = consensusSlot . headerConsensusData

headerLeaderKey :: AHeader a -> PublicKey
headerLeaderKey = consensusLeaderKey . headerConsensusData

headerDifficulty :: AHeader a -> ChainDifficulty
headerDifficulty = consensusDifficulty . headerConsensusData

headerSignature :: AHeader a -> BlockSignature
headerSignature = consensusSignature . headerConsensusData

headerProtocolVersion :: AHeader a -> ProtocolVersion
headerProtocolVersion = ehdProtocolVersion . headerExtraData

headerSoftwareVersion :: AHeader a -> SoftwareVersion
headerSoftwareVersion = ehdSoftwareVersion . headerExtraData

headerAttributes :: AHeader a -> Attributes ()
headerAttributes = ehdAttributes . headerExtraData

headerEBDataProof :: AHeader a -> Hash ExtraBodyData
headerEBDataProof = ehdEBDataProof . headerExtraData

headerToSign :: EpochSlots -> AHeader a -> ToSign
headerToSign epochSlots h = ToSign
  (headerPrevHash h)
  (headerProof h)
  (unflattenSlotId epochSlots $ headerSlot h)
  (headerDifficulty h)
  (headerExtraData h)

headerLength :: AHeader ByteString -> Int64
headerLength = fromIntegral . BS.length . headerAnnotation

data HeaderError
  = HeaderExtraDataError ExtraHeaderDataError
  | HeaderInvalidSignature BlockSignature
  deriving (Eq, Show)

instance B.Buildable HeaderError where
  build = \case
    HeaderExtraDataError err -> bprint
      ("ExtraHeaderData was invalid while checking Header.\n Error: " . build)
      err
    HeaderInvalidSignature sig ->
      bprint ("Invalid signature while checking Header.\n" . build) sig


-- | Verify a main block header in isolation
verifyHeader
  :: MonadError HeaderError m
  => ProtocolMagicId
  -> EpochSlots
  -> AHeader ByteString
  -> m ()
verifyHeader pm epochSlots header = do
  -- Previous header hash is always valid.
  -- Body proof is just a bunch of hashes, which is always valid (although must
  -- be checked against the actual body, in verifyBlock. Extra header data
  -- requires validation.
  liftEither . first HeaderExtraDataError $ verifyExtraHeaderData
    (headerExtraData header)
  -- Internal consistency: is the signature in the consensus data really for
  -- this block?
  verifyBlockSignature (consensusSignature consensus)
    `orThrowError` HeaderInvalidSignature (consensusSignature consensus)
 where
  verifyBlockSignature (BlockSignature proxySig) =
    proxyVerifyDecoded pm SignMainBlockHeavy (const True) signed proxySig
  signed    = recoverSignedBytes epochSlots header
  consensus = headerConsensusData header

-- | Encode a 'Header' accounting for deprecated epoch boundary blocks
toCBORHeader :: EpochSlots -> Header -> Encoding
toCBORHeader epochSlots h =
  encodeListLen 2 <> toCBOR (1 :: Word) <> toCBORHeader' epochSlots h

fromCBORHeader :: EpochSlots -> Decoder s (Maybe Header)
fromCBORHeader epochSlots = do
  enforceSize "Header" 2
  fromCBOR @Word >>= \case
    0 -> do
      void dropBoundaryHeader
      pure Nothing
    1 -> Just <$!> fromCBORHeader' epochSlots
    t -> cborError $ DecoderErrorUnknownTag "Header" (fromIntegral t)

--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

-- | 'Hash' of block header
type HeaderHash = Hash Header

-- | Specialized formatter for 'HeaderHash'
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | These bytes must be prepended when hashing raw boundary header data
--
--   In the Byron release, hashes were taken over a data type that was never
--   directly serialized to the blockchain, so these magic bytes cannot be
--   determined from the raw header data.
--
--   These bytes are from `encodeListLen 2 <> toCBOR (1 :: Word8)`
wrapHeaderBytes :: ByteString -> ByteString
wrapHeaderBytes = mappend "\130\SOH"

-- | Hash the serialised representation of a `Header`
--
--   For backwards compatibility we have to take the hash of the header
--   serialised with 'toCBORHeader'
hashHeader :: EpochSlots -> Header -> HeaderHash
hashHeader es = unsafeAbstractHash . serializeEncoding . toCBORHeader es

--------------------------------------------------------------------------------
-- BoundaryHeader
--------------------------------------------------------------------------------

dropBoundaryHeader :: Decoder s HeaderHash
dropBoundaryHeader = do
  enforceSize "BoundaryHeader" 5
  dropInt32
  -- HeaderHash
  hh <- fromCBOR
  -- BoundaryBodyProof
  dropBytes
  dropBoundaryConsensusData
  dropBoundaryExtraHeaderData
  pure hh

-- | These bytes must be prepended when hashing raw boundary header data
--
--   In the Byron release, hashes were taken over a data type that was never
--   directly serialized to the blockchain, so these magic bytes cannot be
--   determined from the raw header data.
wrapBoundaryBytes :: ByteString -> ByteString
wrapBoundaryBytes = mappend "\130\NUL"


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

-- | Signature of the 'Block'
--
--   We use a heavyweight delegation scheme, so the signature has two parts:
--
--   1. A delegation certificate from a genesis key to the block signer
--   2. The actual signature over `ToSign`
newtype BlockSignature = BlockSignature
  { unBlockSignature :: ProxySignature EpochIndex ToSign
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

instance B.Buildable BlockSignature where
  build (BlockSignature s) = bprint ("BlockSignature: ".build) s

instance ToCBOR BlockSignature where
  toCBOR (BlockSignature pxy) =
    -- Tag 0 was previously used for BlockSignature (no delegation)
    -- Tag 1 was previously used for BlockPSignatureLight
    encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR pxy

instance FromCBOR BlockSignature where
  fromCBOR = do
    enforceSize "BlockSignature" 2
    fromCBOR >>= \case
      2 -> BlockSignature <$> fromCBOR
      t -> cborError $ DecoderErrorUnknownTag "BlockSignature" t


--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

-- | Produces the ByteString that was signed in the block
recoverSignedBytes
  :: EpochSlots -> AHeader ByteString -> Annotated ToSign ByteString
recoverSignedBytes es h = Annotated toSign bytes
 where
  bytes = BS.concat
    [ "\133"
    -- This is the value of Codec.CBOR.Write.toLazyByteString (encodeListLen 5)
    -- It is hard coded here because the signed bytes included it as an implementation artifact
    , (annotation . aHeaderPrevHash) h
    , (annotation . aHeaderProof) h
    , (annotation . aConsensusSlot . headerConsensusData) h
    , (annotation . aConsensusDifficulty . headerConsensusData) h
    , (annotation . aHeaderExtraData) h
    ]
  toSign = ToSign
    (headerPrevHash h)
    (headerProof h)
    (unflattenSlotId es $ headerSlot h)
    (headerDifficulty h)
    (headerExtraData h)

-- | Data to be signed in 'Block'
data ToSign = ToSign
  { _msHeaderHash  :: !HeaderHash
  -- ^ Hash of previous header in the chain
  , _msBodyProof   :: !Proof
  , _msSlot        :: !SlotId
  , _msChainDiff   :: !ChainDifficulty
  , _msExtraHeader :: !ExtraHeaderData
  } deriving (Eq, Show, Generic)

instance ToCBOR ToSign where
  toCBOR mts =
    encodeListLen 5
      <> toCBOR (_msHeaderHash mts)
      <> toCBOR (_msBodyProof mts)
      <> toCBOR (_msSlot mts)
      <> toCBOR (_msChainDiff mts)
      <> toCBOR (_msExtraHeader mts)

instance FromCBOR ToSign where
  fromCBOR = do
    enforceSize "ToSign" 5
    ToSign <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR


--------------------------------------------------------------------------------
-- ConsensusData
--------------------------------------------------------------------------------

type ConsensusData = AConsensusData ()

consensusData
  :: FlatSlotId
  -> PublicKey
  -> ChainDifficulty
  -> BlockSignature
  -> ConsensusData
consensusData slotNo pk cd bs =
  AConsensusData (Annotated slotNo ()) pk (Annotated cd ()) bs

data AConsensusData a = AConsensusData
  { aConsensusSlot       :: !(Annotated FlatSlotId a)
  -- ^ Id of the slot for which this block was generated
  , consensusLeaderKey   :: !PublicKey
  -- ^ Public key of the slot leader. It's essential to have it here, because
  --   FTS gives us only hash of public key (aka 'StakeholderId').
  , aConsensusDifficulty :: !(Annotated ChainDifficulty a)
  -- ^ Difficulty of chain ending in this block
  , consensusSignature   :: !BlockSignature
  -- ^ Signature given by slot leader
  } deriving (Generic, Show, Eq, Functor)
    deriving anyclass NFData

fromCBORAConsensus :: EpochSlots -> Decoder s (AConsensusData ByteSpan)
fromCBORAConsensus epochSlots = do
  enforceSize "ConsensusData" 4
  -- Next, we decode a 'SlotId' into a 'FlatSlotId': the `SlotId` used in
  -- 'AConsensusData' is encoded as a epoch and slot-count pair.
  epochAndSlotCount :: Annotated SlotId ByteSpan <- fromCBORAnnotated
  pk           <- fromCBOR
  annChaiDifficulty <- fromCBORAnnotated
  consensusSig <- fromCBOR
  let slotNo = first (flattenSlotId epochSlots) epochAndSlotCount
  pure $! AConsensusData slotNo pk annChaiDifficulty consensusSig

consensusSlot :: AConsensusData a -> FlatSlotId
consensusSlot = unAnnotated . aConsensusSlot

consensusDifficulty :: AConsensusData a -> ChainDifficulty
consensusDifficulty = unAnnotated . aConsensusDifficulty

toCBORConsensusData :: EpochSlots -> ConsensusData -> Encoding
toCBORConsensusData es cd =
  encodeListLen 4
    <> toCBOR (unflattenSlotId es $ consensusSlot cd)
    <> toCBOR (consensusLeaderKey cd)
    <> toCBOR (consensusDifficulty cd)
    <> toCBOR (consensusSignature cd)

fromCBORConsensusData :: EpochSlots -> Decoder s ConsensusData
fromCBORConsensusData epochSlots =
  (fmap . fmap) (const ()) (fromCBORAConsensus epochSlots)
