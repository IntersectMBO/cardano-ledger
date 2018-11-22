{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Header
  ( Header
  , AHeader
  , headerPrevHash
  , headerProof
  , mkHeader
  , mkHeaderExplicit
  , headerSlot
  , headerLeaderKey
  , headerDifficulty
  , headerSignature
  , headerBlockVersion
  , headerSoftwareVersion
  , headerAttributes
  , headerEBDataProof
  , encodeHeader
  , decodeHeader
  , decodeAHeader
  , HeaderError(..)
  , verifyHeader
  , HeaderHash
  , headerHashF
  , hashHeader
  , BlockSignature(..)
  , dropBoundaryHeader
  , ToSign(..)
  , ConsensusData
  , consensusData
  , verifyConsensusData
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import qualified Data.ByteString as BS
import Formatting (Format, bprint, build, int)
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
import Cardano.Chain.Common (Attributes, ChainDifficulty)
import Cardano.Chain.Delegation.HeavyDlgIndex
  (ProxySKBlockInfo, ProxySKHeavy, ProxySigHeavy)
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Slotting (SlotId(..), slotIdF)
import Cardano.Chain.Update.BlockVersion (BlockVersion)
import Cardano.Chain.Update.SoftwareVersion (SoftwareVersion)
import Cardano.Crypto
  ( Hash
  , ProtocolMagic(..)
  , PublicKey
  , SecretKey
  , SignTag(..)
  , Signature
  , hashHexF
  , isSelfSignedPsk
  , proxySign
  , proxyVerifyDecoded
  , psigPsk
  , sign
  , toPublic
  , unsafeAbstractHash
  , verifySignatureDecoded
  )


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

type Header = AHeader ()

data AHeader a = AHeader
  { headerProtocolMagic :: !ProtocolMagic
  , aHeaderPrevHash     :: !(Annotated HeaderHash a)
  -- ^ Pointer to the header of the previous block
  , aHeaderProof        :: !(Annotated Proof a)
  -- ^ Proof of body
  , headerConsensusData :: !(AConsensusData a)
  -- ^ Consensus data to verify consensus algorithm
  , aHeaderExtraData    :: !(Annotated ExtraHeaderData a)
  -- ^ Any extra data
  } deriving (Eq, Show, Generic, NFData, Functor)

headerPrevHash :: AHeader a -> HeaderHash
headerPrevHash = unAnnotated . aHeaderPrevHash

headerProof :: AHeader a -> Proof
headerProof = unAnnotated . aHeaderProof

headerExtraData :: AHeader a -> ExtraHeaderData
headerExtraData = unAnnotated . aHeaderExtraData

instance B.Buildable Header where
  build header = bprint
    ( "Header:\n"
    . "    hash: " . hashHexF . "\n"
    . "    previous block: " . hashHexF . "\n"
    . "    slot: " . slotIdF . "\n"
    . "    difficulty: " . int . "\n"
    . "    leader: " . build . "\n"
    . "    signature: " . build . "\n"
    . build
    )
    headerHash
    (headerPrevHash header)
    (consensusSlot consensus)
    (consensusDifficulty consensus)
    (consensusLeaderKey consensus)
    (consensusSignature consensus)
    (headerExtraData header)
   where
    headerHash :: HeaderHash
    headerHash = hashHeader header
    consensus  = headerConsensusData header

instance Bi Header where
  encode h =
    encodeListLen 5
      <> encode (getProtocolMagic (headerProtocolMagic h))
      <> encode (headerPrevHash h)
      <> encode (headerProof h)
      <> encode (headerConsensusData h)
      <> encode (headerExtraData h)

  decode = void <$> decodeAHeader

decodeAHeader :: Decoder s (AHeader ByteSpan)
decodeAHeader = do
  enforceSize "Header" 5
  AHeader
    <$> (ProtocolMagic <$> decode)
    <*> decodeAnnotated
    <*> decodeAnnotated
    <*> decodeAConsensus
    <*> decodeAnnotated


-- | Smart constructor for 'Header'
mkHeader
  :: ProtocolMagic
  -> Either GenesisHash Header
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> Body
  -> ExtraHeaderData
  -> Header
mkHeader pm prevHeader = mkHeaderExplicit pm prevHash difficulty
 where
  prevHash   = either getGenesisHash hashHeader prevHeader
  difficulty = either
    (const 0)
    (succ . consensusDifficulty . headerConsensusData)
    prevHeader

-- | Make a 'Header' for a given slot, with a given body, parent hash,
--   and difficulty. This takes care of some signing and consensus data.
mkHeaderExplicit
  :: ProtocolMagic
  -> HeaderHash
  -- ^ Parent
  -> ChainDifficulty
  -> SlotId
  -> SecretKey
  -> ProxySKBlockInfo
  -> Body
  -> ExtraHeaderData
  -> Header
mkHeaderExplicit pm prevHash difficulty slotId sk pske body extra = AHeader
  pm
  (Annotated prevHash ())
  (Annotated proof ())
  consensus
  (Annotated extra ())
 where
  proof = mkProof body
  makeSignature :: ToSign -> (ProxySKHeavy, PublicKey) -> BlockSignature
  makeSignature toSign (psk, _) =
    BlockPSignatureHeavy $ proxySign pm SignMainBlockHeavy sk psk toSign
  signature =
    let toSign = ToSign prevHash proof slotId difficulty extra
    in
      maybe
        (BlockSignature $ sign pm SignMainBlock sk toSign)
        (makeSignature toSign)
        pske
  leaderPk  = maybe (toPublic sk) snd pske
  consensus = consensusData slotId leaderPk difficulty signature

headerSlot :: AHeader a -> SlotId
headerSlot = consensusSlot . headerConsensusData

headerLeaderKey :: AHeader a -> PublicKey
headerLeaderKey = consensusLeaderKey . headerConsensusData

headerDifficulty :: AHeader a -> ChainDifficulty
headerDifficulty = consensusDifficulty . headerConsensusData

headerSignature :: AHeader a -> BlockSignature
headerSignature = consensusSignature . headerConsensusData

headerBlockVersion :: AHeader a -> BlockVersion
headerBlockVersion = ehdBlockVersion . headerExtraData

headerSoftwareVersion :: AHeader a -> SoftwareVersion
headerSoftwareVersion = ehdSoftwareVersion . headerExtraData

headerAttributes :: AHeader a -> Attributes ()
headerAttributes = ehdAttributes . headerExtraData

headerEBDataProof :: AHeader a -> Hash ExtraBodyData
headerEBDataProof = ehdEBDataProof . headerExtraData

data HeaderError
  = HeaderConsensusError ConsensusError
  | HeaderExtraDataError ExtraHeaderDataError
  | HeaderInvalidSignature BlockSignature
  deriving (Eq, Show)

instance B.Buildable HeaderError where
  build = \case
    HeaderConsensusError err -> bprint
      ("ConsensusData was invalid while checking Header.\n Error: " . build)
      err
    HeaderExtraDataError err -> bprint
      ("ExtraHeaderData was invalid while checking Header.\n Error: " . build)
      err
    HeaderInvalidSignature sig ->
      bprint ("Invalid signature while checking Header.\n" . build) sig


-- | Verify a main block header in isolation
verifyHeader
  :: MonadError HeaderError m => ProtocolMagic -> AHeader ByteString -> m ()
verifyHeader pm header = do
  -- Previous header hash is always valid.
  -- Body proof is just a bunch of hashes, which is always valid (although must
  -- be checked against the actual body, in verifyBlock. Consensus data and
  -- extra header data require validation.
  liftEither . first HeaderConsensusError $ verifyConsensusData consensus
  liftEither . first HeaderExtraDataError $ verifyExtraHeaderData
    (headerExtraData header)
  -- Internal consistency: is the signature in the consensus data really for
  -- this block?
  unless (verifyBlockSignature $ consensusSignature consensus)
    $ throwError (HeaderInvalidSignature $ consensusSignature consensus)
 where
  verifyBlockSignature (BlockSignature sig) = verifySignatureDecoded
    pm
    SignMainBlock
    (consensusLeaderKey consensus)
    signed
    sig
  verifyBlockSignature (BlockPSignatureHeavy proxySig) =
    proxyVerifyDecoded pm SignMainBlockHeavy proxySig (const True) signed
  signed    = recoverSignedBytes header
  consensus = headerConsensusData header

encodeHeader :: Header -> Encoding
encodeHeader h = encodeListLen 2 <> encode (1 :: Word) <> encode h

decodeHeader :: Decoder s (Maybe Header)
decodeHeader = do
  enforceSize "Header" 2
  decode @Word >>= \case
    0 -> do
      dropBoundaryHeader
      pure Nothing
    1 -> Just <$!> decode
    t -> cborError $ DecoderErrorUnknownTag "Header" (fromIntegral t)

--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

-- | 'Hash' of block header
type HeaderHash = Hash Header

-- | Specialized formatter for 'HeaderHash'
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | Hash the serialised representation of a `Header`
--
--   For backwards compatibility we have to take the hash of the header
--   serialised with 'encodeHeader'
hashHeader :: Header -> HeaderHash
hashHeader = unsafeAbstractHash . serializeEncoding . encodeHeader


--------------------------------------------------------------------------------
-- BoundaryHeader
--------------------------------------------------------------------------------

dropBoundaryHeader :: Dropper s
dropBoundaryHeader = do
  enforceSize "BoundaryHeader" 5
  dropInt32
  -- HeaderHash
  dropBytes
  -- BoundaryBodyProof
  dropBytes
  dropBoundaryConsensusData
  dropBoundaryExtraHeaderData


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

-- | Signature of the block. Can be either regular signature from the issuer or
--   delegated signature having a constraint on epoch indices (it means the
--   signature is valid only if block's slot id has epoch inside the constrained
--   interval).
data BlockSignature
  = BlockSignature (Signature ToSign)
  | BlockPSignatureHeavy (ProxySigHeavy ToSign)
  deriving (Show, Eq, Generic)

instance NFData BlockSignature

instance B.Buildable BlockSignature where
  build (BlockSignature s)       = bprint ("BlockSignature: ".build) s
  build (BlockPSignatureHeavy s) = bprint ("BlockPSignatureHeavy: ".build) s

instance Bi BlockSignature where
  encode input = case input of
    BlockSignature sig -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
    -- Tag 1 was previously used for BlockPSignatureLight
    BlockPSignatureHeavy pxy ->
      encodeListLen 2 <> encode (2 :: Word8) <> encode pxy

  decode = do
    enforceSize "BlockSignature" 2
    decode >>= \case
      0 -> BlockSignature <$> decode
      2 -> BlockPSignatureHeavy <$> decode
      t -> cborError $ DecoderErrorUnknownTag "BlockSignature" t

--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

-- | Produces the ByteString that was signed in the block
recoverSignedBytes :: AHeader ByteString -> Annotated ToSign ByteString
recoverSignedBytes h = Annotated toSign bytes
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
    (headerSlot h)
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

instance Bi ToSign where
  encode mts =
    encodeListLen 5
      <> encode (_msHeaderHash mts)
      <> encode (_msBodyProof mts)
      <> encode (_msSlot mts)
      <> encode (_msChainDiff mts)
      <> encode (_msExtraHeader mts)

  decode = do
    enforceSize "ToSign" 5
    ToSign <$> decode <*> decode <*> decode <*> decode <*> decode


--------------------------------------------------------------------------------
-- ConsensusData
--------------------------------------------------------------------------------

type ConsensusData = AConsensusData ()

consensusData
  :: SlotId -> PublicKey -> ChainDifficulty -> BlockSignature -> ConsensusData
consensusData sid pk cd bs =
  AConsensusData (Annotated sid ()) pk (Annotated cd ()) bs

data AConsensusData a = AConsensusData
  { aConsensusSlot       :: !(Annotated SlotId a)
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

decodeAConsensus :: Decoder s (AConsensusData ByteSpan)
decodeAConsensus = do
  enforceSize "ConsensusData" 4
  AConsensusData <$> decodeAnnotated <*> decode <*> decodeAnnotated <*> decode

consensusSlot :: AConsensusData a -> SlotId
consensusSlot = unAnnotated . aConsensusSlot

consensusDifficulty :: AConsensusData a -> ChainDifficulty
consensusDifficulty = unAnnotated . aConsensusDifficulty

instance Bi ConsensusData where
  encode cd =
    encodeListLen 4
      <> encode (consensusSlot cd)
      <> encode (consensusLeaderKey cd)
      <> encode (consensusDifficulty cd)
      <> encode (consensusSignature cd)

  decode = (fmap.fmap) (const ()) decodeAConsensus

data ConsensusError = ConsensusSelfSignedPSK
  deriving (Show, Eq)

instance B.Buildable ConsensusError where
  build = \case
    ConsensusSelfSignedPSK ->
      bprint "Self-signed ProxySecretKey in ConsensusData"

-- | Verify the consensus data in isolation
verifyConsensusData :: MonadError ConsensusError m => AConsensusData a -> m ()
verifyConsensusData mcd = when (selfSignedProxy $ consensusSignature mcd)
  $ throwError ConsensusSelfSignedPSK
 where
  selfSignedProxy (BlockSignature       _  ) = False
  selfSignedProxy (BlockPSignatureHeavy sig) = isSelfSignedPsk $ psigPsk sig
