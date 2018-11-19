{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Chain.Block.Header
  ( Header(..)
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
  , HeaderError(..)
  , verifyHeader
  , HeaderHash
  , headerHashF
  , hashHeader
  , BlockSignature(..)
  , dropBoundaryHeader
  , ToSign(..)
  , ConsensusData(..)
  , verifyConsensusData
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import Formatting (Format, bprint, build, int)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Bi(..)
  , Decoder
  , DecoderError(..)
  , Dropper
  , Encoding
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
  , checkSig
  , hashHexF
  , isSelfSignedPsk
  , proxySign
  , proxyVerify
  , psigPsk
  , sign
  , toPublic
  , unsafeAbstractHash
  )


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

data Header = Header
  { headerProtocolMagic :: !ProtocolMagic
  , headerPrevHash      :: !HeaderHash
  -- ^ Pointer to the header of the previous block
  , headerProof         :: !Proof
  -- ^ Proof of body
  , headerConsensusData :: !ConsensusData
  -- ^ Consensus data to verify consensus algorithm
  , headerExtraData     :: !ExtraHeaderData
  -- ^ Any extra data
  } deriving (Eq, Show, Generic, NFData)

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
  encode header =
    encodeListLen 5
      <> encode (getProtocolMagic (headerProtocolMagic header))
      <> encode (headerPrevHash header)
      <> encode (headerProof header)
      <> encode (headerConsensusData header)
      <> encode (headerExtraData header)

  decode = do
    enforceSize "Header" 5
    Header
      <$> (ProtocolMagic <$> decode)
      <*> decode
      <*> decode
      <*> decode
      <*> decode

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
mkHeaderExplicit pm prevHash difficulty slotId sk pske body extra = Header
  pm
  prevHash
  proof
  consensus
  extra
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
  consensus = ConsensusData
    { consensusSlot       = slotId
    , consensusLeaderKey  = leaderPk
    , consensusDifficulty = difficulty
    , consensusSignature  = signature
    }

headerSlot :: Header -> SlotId
headerSlot = consensusSlot . headerConsensusData

headerLeaderKey :: Header -> PublicKey
headerLeaderKey = consensusLeaderKey . headerConsensusData

headerDifficulty :: Header -> ChainDifficulty
headerDifficulty = consensusDifficulty . headerConsensusData

headerSignature :: Header -> BlockSignature
headerSignature = consensusSignature . headerConsensusData

headerBlockVersion :: Header -> BlockVersion
headerBlockVersion = ehdBlockVersion . headerExtraData

headerSoftwareVersion :: Header -> SoftwareVersion
headerSoftwareVersion = ehdSoftwareVersion . headerExtraData

headerAttributes :: Header -> Attributes ()
headerAttributes = ehdAttributes . headerExtraData

headerEBDataProof :: Header -> Hash ExtraBodyData
headerEBDataProof = ehdEBDataProof . headerExtraData

data HeaderError
  = HeaderConsensusError ConsensusError
  | HeaderExtraDataError ExtraHeaderDataError
  | HeaderInvalidSignature BlockSignature

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
verifyHeader :: MonadError HeaderError m => ProtocolMagic -> Header -> m ()
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
  verifyBlockSignature (BlockSignature sig) =
    checkSig pm SignMainBlock (consensusLeaderKey consensus) signature sig
  verifyBlockSignature (BlockPSignatureHeavy proxySig) =
    proxyVerify pm SignMainBlockHeavy proxySig (const True) signature

  signature = ToSign
    (headerPrevHash header)
    (headerProof header)
    (consensusSlot consensus)
    (consensusDifficulty consensus)
    (headerExtraData header)

  consensus = headerConsensusData header

encodeHeader :: Header -> Encoding
encodeHeader header = encodeListLen 2 <> encode (1 :: Word) <> encode header

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

data ConsensusData = ConsensusData
  { consensusSlot       :: !SlotId
  -- ^ Id of the slot for which this block was generated
  , consensusLeaderKey  :: !PublicKey
  -- ^ Public key of the slot leader. It's essential to have it here, because
  --   FTS gives us only hash of public key (aka 'StakeholderId').
  , consensusDifficulty :: !ChainDifficulty
  -- ^ Difficulty of chain ending in this block
  , consensusSignature  :: !BlockSignature
  -- ^ Signature given by slot leader
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData

instance Bi ConsensusData where
  encode cd =
    encodeListLen 4
      <> encode (consensusSlot cd)
      <> encode (consensusLeaderKey cd)
      <> encode (consensusDifficulty cd)
      <> encode (consensusSignature cd)

  decode = do
    enforceSize "ConsensusData" 4
    ConsensusData <$> decode <*> decode <*> decode <*> decode

data ConsensusError = ConsensusSelfSignedPSK

instance B.Buildable ConsensusError where
  build = \case
    ConsensusSelfSignedPSK ->
      bprint "Self-signed ProxySecretKey in ConsensusData"

-- | Verify the consensus data in isolation
verifyConsensusData :: MonadError ConsensusError m => ConsensusData -> m ()
verifyConsensusData mcd = when (selfSignedProxy $ consensusSignature mcd)
  $ throwError ConsensusSelfSignedPSK
 where
  selfSignedProxy (BlockSignature       _  ) = False
  selfSignedProxy (BlockPSignatureHeavy sig) = isSelfSignedPsk $ psigPsk sig
