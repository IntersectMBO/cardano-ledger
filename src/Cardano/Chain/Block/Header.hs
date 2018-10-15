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
       ( Header (..)
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
       , verifyHeader

       , HeaderHash
       , headerHashF
       , hashHeader

       , BlockSignature (..)

       , dropBoundaryHeader

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
                     MainExtraBodyData, MainExtraHeaderData (..),
                     MainProof (..), mkMainProof)
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
-- Header
--------------------------------------------------------------------------------

data Header = Header
  { headerProtocolMagic :: !ProtocolMagic
  , headerPrevHash      :: !HeaderHash
  -- ^ Pointer to the header of the previous block
  , headerProof         :: !MainProof
  -- ^ Proof of body
  , headerConsensusData :: !MainConsensusData
  -- ^ Consensus data to verify consensus algorithm
  , headerExtraData     :: !MainExtraHeaderData
  -- ^ Any extra data
  } deriving (Eq, Show, Generic, NFData)

instance B.Buildable Header where
  build header = bprint
    ( "Header:\n"
    % "    hash: " % hashHexF % "\n"
    % "    previous block: " % hashHexF % "\n"
    % "    slot: " % slotIdF % "\n"
    % "    difficulty: " % int % "\n"
    % "    leader: " % build % "\n"
    % "    signature: " % build % "\n"
    % build
    )
    headerHash
    (headerPrevHash header)
    (_mcdSlot consensus)
    (_mcdDifficulty consensus)
    (_mcdLeaderKey consensus)
    (_mcdSignature consensus)
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
  -> MainBody
  -> MainExtraHeaderData
  -> Header
mkHeader pm prevHeader = mkHeaderExplicit pm prevHash difficulty
 where
  prevHash = either getGenesisHash hashHeader prevHeader
  difficulty =
    either (const 0) (succ . _mcdDifficulty . headerConsensusData) prevHeader

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
  -> MainBody
  -> MainExtraHeaderData
  -> Header
mkHeaderExplicit pm prevHash difficulty slotId sk pske body extra =
  Header pm prevHash proof consensus extra
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

headerSlot :: Header -> SlotId
headerSlot = _mcdSlot . headerConsensusData

headerLeaderKey :: Header -> PublicKey
headerLeaderKey = _mcdLeaderKey . headerConsensusData

headerDifficulty :: Header -> ChainDifficulty
headerDifficulty = _mcdDifficulty . headerConsensusData

headerSignature :: Header -> BlockSignature
headerSignature = _mcdSignature . headerConsensusData

headerBlockVersion :: Header -> BlockVersion
headerBlockVersion = _mehBlockVersion . headerExtraData

headerSoftwareVersion :: Header -> SoftwareVersion
headerSoftwareVersion = _mehSoftwareVersion . headerExtraData

headerAttributes :: Header -> BlockHeaderAttributes
headerAttributes = _mehAttributes . headerExtraData

headerEBDataProof :: Header -> Hash MainExtraBodyData
headerEBDataProof = _mehEBDataProof . headerExtraData

-- | Verify a main block header in isolation
verifyHeader
  :: MonadError Text m => ProtocolMagic -> Header -> m ()
verifyHeader pm header = do
  -- Previous header hash is always valid.
  -- Body proof is just a bunch of hashes, which is always valid (although
  -- must be checked against the actual body, in verifyMainBlock.
  -- Consensus data and extra header data require validation.
  verifyMainConsensusData consensus
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
    (headerPrevHash header)
    (headerProof header)
    (_mcdSlot consensus)
    (_mcdDifficulty consensus)
    (headerExtraData header)

  epochId   = siEpoch $ _mcdSlot consensus

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
    enforceSize "MainConsensusData" 4
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
-- MainConsensusData lenses
--------------------------------------------------------------------------------

makeLenses 'MainConsensusData


----------------------------------------------------------------------------
-- MainToSign lenses
----------------------------------------------------------------------------

makeLenses ''MainToSign
