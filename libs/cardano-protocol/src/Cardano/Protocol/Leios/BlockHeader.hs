{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Block header associated with Leios.
--
-- The Leios block header is the Praos block header with two additional fields
-- appended. Everything else is identical to the Praos header, see
-- "Cardano.Protocol.Praos.BlockHeader" for details.
module Cardano.Protocol.Leios.BlockHeader (
  Header (HeaderConstr),
  HeaderRaw (..),
  HeaderBody (..),
  mkHeader,
  headerBody,
  headerSig,
  headerHash,
  headerSize,
) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (
  SignableRepresentation (getSignableRepresentation),
 )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (mkVersion32)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  Version,
  decodeFixedSized,
  decodeNullStrictMaybe,
  decodeRecordNamed,
  encodeFixedSized,
  encodeListLen,
  encodeNullStrictMaybe,
  serialize',
  unCBORGroup,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (
  BlockHeaderVersionInfo (..),
  EbReferencesAnnouncement,
  EraBlockHeader (..),
  LeiosEraBlockHeader (..),
  blockHeaderL,
 )
import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Hashes (
  EraIndependentBlockBody,
  EraIndependentBlockHeader,
  HASH,
  HashAnnotated (..),
  SafeToHash,
  extractHash,
  originalBytesSize,
 )
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey, hashKey)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  mkMemoizedEra,
 )
import Cardano.Protocol.Crypto (Crypto, KES, VRF)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to)
import NoThunks.Class (NoThunks (..))

data HeaderBody crypto = HeaderBody
  { hbBlockNo :: !BlockNo
  -- ^ block number
  , hbSlotNo :: !SlotNo
  -- ^ block slot
  , hbPrev :: !PrevHash
  -- ^ Hash of the previous block header
  , hbVk :: !(VKey BlockIssuer)
  -- ^ verification key of block issuer
  , hbVrfVk :: !(VRF.VerKeyVRF (VRF crypto))
  -- ^ VRF verification key for block issuer
  , hbVrfRes :: !(VRF.CertifiedVRF (VRF crypto) InputVRF)
  -- ^ Certified VRF value
  , hbBodySize :: !Word32
  -- ^ Size of the block body
  , hbBodyHash :: !(Hash.Hash HASH EraIndependentBlockBody)
  -- ^ Hash of block body
  , hbOCert :: !(OCert crypto)
  -- ^ operational certificate
  , hbVersionInfo :: !BlockHeaderVersionInfo
  -- ^ version information reported by the block producer
  , hbBlockBodyContainsLeiosCert :: !Bool
  -- ^ whether the block body contains a Leios certificate
  , hbEbReferencesAnnouncement :: !(StrictMaybe EbReferencesAnnouncement)
  -- ^ Announcement of Endorser Block (EB) references
  }
  deriving (Generic)

hbVersionInfoL :: Lens' (HeaderBody crypto) BlockHeaderVersionInfo
hbVersionInfoL = lens hbVersionInfo (\hb vi -> hb {hbVersionInfo = vi})

hbEbReferencesAnnouncementL :: Lens' (HeaderBody crypto) (StrictMaybe EbReferencesAnnouncement)
hbEbReferencesAnnouncementL =
  lens hbEbReferencesAnnouncement (\hb ma -> hb {hbEbReferencesAnnouncement = ma})

headerBodyEncodingVersion :: HeaderBody crypto -> Version
headerBodyEncodingVersion =
  fromMaybe maxBound . mkVersion32 . bhviHighestSupportedMajorVersion . hbVersionInfo

deriving instance Crypto crypto => Show (HeaderBody crypto)

deriving instance Crypto crypto => Eq (HeaderBody crypto)

instance
  Crypto crypto =>
  SignableRepresentation (HeaderBody crypto)
  where
  getSignableRepresentation hb = serialize' (headerBodyEncodingVersion hb) hb

instance
  Crypto crypto =>
  NoThunks (HeaderBody crypto)

data HeaderRaw crypto = HeaderRaw
  { headerRawBody :: !(HeaderBody crypto)
  , headerRawSig :: !(KES.SignedKES (KES crypto) (HeaderBody crypto))
  }
  deriving (Show, Generic)

instance Crypto c => Eq (HeaderRaw c) where
  h1 == h2 =
    headerRawSig h1 == headerRawSig h2
      && headerRawBody h1 == headerRawBody h2

instance
  Crypto crypto =>
  NoThunks (HeaderRaw crypto)

newtype Header crypto = HeaderConstr (MemoBytes (HeaderRaw crypto))
  deriving (Generic)
  deriving newtype (Eq, Show, NoThunks, Plain.ToCBOR, SafeToHash)

instance Memoized (Header crypto) where
  type RawType (Header crypto) = HeaderRaw crypto

type instance MemoHashIndex (HeaderRaw crypto) = EraIndependentBlockHeader

instance HashAnnotated (Header crypto) EraIndependentBlockHeader where
  hashAnnotated = getMemoSafeHash

mkHeader ::
  forall era crypto proxy.
  (Era era, Crypto crypto) =>
  proxy era ->
  HeaderBody crypto ->
  KES.SignedKES (KES crypto) (HeaderBody crypto) ->
  Header crypto
mkHeader _ body sig = mkMemoizedEra @era $ HeaderRaw body sig

headerBody :: Header crypto -> HeaderBody crypto
headerBody = headerRawBody . getMemoRawType

headerSig :: Header crypto -> KES.SignedKES (KES crypto) (HeaderBody crypto)
headerSig = headerRawSig . getMemoRawType

headerSize :: Header crypto -> Int
headerSize = originalBytesSize

headerHash ::
  Header crypto ->
  Hash.Hash HASH EraIndependentBlockHeader
headerHash = extractHash . hashAnnotated

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance Crypto crypto => EncCBOR (HeaderBody crypto) where
  encCBOR
    HeaderBody
      { hbBlockNo
      , hbSlotNo
      , hbPrev
      , hbVk
      , hbVrfVk
      , hbVrfRes
      , hbBodySize
      , hbBodyHash
      , hbOCert
      , hbVersionInfo
      , hbBlockBodyContainsLeiosCert
      , hbEbReferencesAnnouncement
      } =
      encodeListLen 12
        <> encCBOR hbBlockNo
        <> encCBOR hbSlotNo
        <> encCBOR hbPrev
        <> encCBOR hbVk
        <> encodeFixedSized hbVrfVk
        <> encCBOR hbVrfRes
        <> encCBOR hbBodySize
        <> encCBOR hbBodyHash
        <> encCBOR hbOCert
        <> encCBOR hbVersionInfo
        <> encCBOR hbBlockBodyContainsLeiosCert
        <> encodeNullStrictMaybe encCBOR hbEbReferencesAnnouncement

instance Crypto crypto => DecCBOR (HeaderBody crypto) where
  decCBOR =
    decodeRecordNamed "HeaderBody" (const 12) $
      HeaderBody
        <$> decCBOR
        <*> decCBOR
        <*> decCBOR
        <*> decCBOR
        <*> decodeFixedSized
        <*> decCBOR
        <*> decCBOR
        <*> decCBOR
        <*> (unCBORGroup <$> decCBOR)
        <*> decCBOR
        <*> decCBOR
        <*> decodeNullStrictMaybe decCBOR

instance Crypto crypto => EncCBOR (HeaderRaw crypto) where
  encCBOR (HeaderRaw body sig) =
    encodeListLen 2 <> encCBOR body <> encodeFixedSized sig

instance Crypto crypto => DecCBOR (HeaderRaw crypto) where
  decCBOR =
    decodeRecordNamed "HeaderRaw" (const 2) $
      HeaderRaw <$> decCBOR <*> decodeFixedSized

instance Crypto crypto => DecCBOR (Annotator (HeaderRaw crypto)) where
  decCBOR = pure <$> decCBOR

instance Crypto c => EncCBOR (Header c)

deriving via
  Mem (HeaderRaw c)
  instance
    Crypto c => DecCBOR (Annotator (Header c))

headerBodyL :: (Era era, Crypto c) => proxy era -> Lens' (Header c) (HeaderBody c)
headerBodyL p = lens headerBody (\h b -> mkHeader p b (headerSig h))

instance (Crypto c, Era era) => EraBlockHeader (Header c) era where
  blockHeaderSizeBlockHeaderG =
    blockHeaderL . to originalBytesSize
  blockIssuerBlockHeaderG =
    blockHeaderL . headerBodyL (Proxy @era) . to (hashKey . hbVk)
  blockBodySizeBlockHeaderL =
    blockHeaderL . headerBodyL (Proxy @era) . lens hbBodySize (\hb sz -> hb {hbBodySize = sz})
  blockBodyHashBlockHeaderL =
    blockHeaderL . headerBodyL (Proxy @era) . lens hbBodyHash (\hb h -> hb {hbBodyHash = h})
  slotNoBlockHeaderL =
    blockHeaderL . headerBodyL (Proxy @era) . lens hbSlotNo (\hb sn -> hb {hbSlotNo = sn})

instance (Crypto c, Era era) => LeiosEraBlockHeader (Header c) era where
  versionInfoBlockHeaderL =
    blockHeaderL . headerBodyL (Proxy @era) . hbVersionInfoL
  ebReferencesAnnouncementBlockHeaderL =
    blockHeaderL . headerBodyL (Proxy @era) . hbEbReferencesAnnouncementL
