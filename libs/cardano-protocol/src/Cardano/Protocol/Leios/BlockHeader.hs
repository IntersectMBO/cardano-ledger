{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Block header associated with Leios.
--
-- The Leios header body is the Praos header body with the protocol version
-- replaced by 'BlockHeaderVersionInfo' and two additional fields appended. Unlike
-- the Praos header body, it is memoized, and its KES signature is made and
-- checked over its original bytes. See "Cardano.Protocol.Praos.BlockHeader" for
-- the remaining details.
module Cardano.Protocol.Leios.BlockHeader (
  Header (HeaderConstr),
  HeaderRaw (..),
  HeaderBody (
    HeaderBodyConstr,
    HeaderBody,
    hbBlockNo,
    hbSlotNo,
    hbPrev,
    hbVk,
    hbVrfVk,
    hbVrfRes,
    hbBodySize,
    hbBodyHash,
    hbOCert,
    hbVersionInfo,
    hbBlockBodyContainsLeiosCert,
    hbEbReferencesAnnouncement
  ),
  HeaderBodyRaw (..),
  mkHeader,
  mkHeaderBody,
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
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  decodeFixedSized,
  decodeNullStrictMaybe,
  decodeRecordNamed,
  encodeFixedSized,
  encodeListLen,
  encodeNullStrictMaybe,
  unCBORGroup,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (
  BlockHeaderVersionInfo,
  EbReferencesAnnouncement,
  EraBlockHeader (..),
  LeiosEraBlockHeader (..),
  blockHeaderL,
 )
import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Hashes (
  EraIndependentBlockBody,
  EraIndependentBlockHeader,
  EraIndependentBlockHeaderBody,
  HASH,
  HashAnnotated (..),
  SafeToHash (..),
  extractHash,
 )
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey, hashKey)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  memoRawTypeL,
  mkMemoizedEra,
 )
import Cardano.Protocol.Crypto (Crypto, KES, VRF)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to)
import NoThunks.Class (NoThunks (..))

data HeaderBodyRaw crypto = HeaderBodyRaw
  { hbrBlockNo :: !BlockNo
  -- ^ block number
  , hbrSlotNo :: !SlotNo
  -- ^ block slot
  , hbrPrev :: !PrevHash
  -- ^ Hash of the previous block header
  , hbrVk :: !(VKey BlockIssuer)
  -- ^ verification key of block issuer
  , hbrVrfVk :: !(VRF.VerKeyVRF (VRF crypto))
  -- ^ VRF verification key for block issuer
  , hbrVrfRes :: !(VRF.CertifiedVRF (VRF crypto) InputVRF)
  -- ^ Certified VRF value
  , hbrBodySize :: !Word32
  -- ^ Size of the block body
  , hbrBodyHash :: !(Hash.Hash HASH EraIndependentBlockBody)
  -- ^ Hash of block body
  , hbrOCert :: !(OCert crypto)
  -- ^ operational certificate
  , hbrVersionInfo :: !BlockHeaderVersionInfo
  -- ^ version information reported by the block producer
  , hbrBlockBodyContainsLeiosCert :: !Bool
  -- ^ whether the block body contains a Leios certificate
  , hbrEbReferencesAnnouncement :: !(StrictMaybe EbReferencesAnnouncement)
  -- ^ Announcement of Endorser Block (EB) references
  }
  deriving (Generic)

hbrVersionInfoL :: Lens' (HeaderBodyRaw crypto) BlockHeaderVersionInfo
hbrVersionInfoL = lens hbrVersionInfo (\hbr vi -> hbr {hbrVersionInfo = vi})

hbrEbReferencesAnnouncementL :: Lens' (HeaderBodyRaw crypto) (StrictMaybe EbReferencesAnnouncement)
hbrEbReferencesAnnouncementL =
  lens hbrEbReferencesAnnouncement (\hbr ma -> hbr {hbrEbReferencesAnnouncement = ma})

deriving instance Crypto crypto => Show (HeaderBodyRaw crypto)

deriving instance Crypto crypto => Eq (HeaderBodyRaw crypto)

instance
  Crypto crypto =>
  NoThunks (HeaderBodyRaw crypto)

newtype HeaderBody crypto = HeaderBodyConstr (MemoBytes (HeaderBodyRaw crypto))
  deriving (Generic)
  deriving newtype (Eq, Show, NoThunks, Plain.ToCBOR, SafeToHash)

instance Memoized (HeaderBody crypto) where
  type RawType (HeaderBody crypto) = HeaderBodyRaw crypto

type instance MemoHashIndex (HeaderBodyRaw crypto) = EraIndependentBlockHeaderBody

instance SignableRepresentation (HeaderBody crypto) where
  getSignableRepresentation = originalBytes

pattern HeaderBody ::
  BlockNo ->
  SlotNo ->
  PrevHash ->
  VKey BlockIssuer ->
  VRF.VerKeyVRF (VRF crypto) ->
  VRF.CertifiedVRF (VRF crypto) InputVRF ->
  Word32 ->
  Hash.Hash HASH EraIndependentBlockBody ->
  OCert crypto ->
  BlockHeaderVersionInfo ->
  Bool ->
  StrictMaybe EbReferencesAnnouncement ->
  HeaderBody crypto
pattern HeaderBody
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
  } <-
  ( getMemoRawType ->
      HeaderBodyRaw
        { hbrBlockNo = hbBlockNo
        , hbrSlotNo = hbSlotNo
        , hbrPrev = hbPrev
        , hbrVk = hbVk
        , hbrVrfVk = hbVrfVk
        , hbrVrfRes = hbVrfRes
        , hbrBodySize = hbBodySize
        , hbrBodyHash = hbBodyHash
        , hbrOCert = hbOCert
        , hbrVersionInfo = hbVersionInfo
        , hbrBlockBodyContainsLeiosCert = hbBlockBodyContainsLeiosCert
        , hbrEbReferencesAnnouncement = hbEbReferencesAnnouncement
        }
    )

{-# COMPLETE HeaderBody #-}

mkHeaderBody ::
  forall era crypto proxy.
  (Era era, Crypto crypto) =>
  proxy era ->
  HeaderBodyRaw crypto ->
  HeaderBody crypto
mkHeaderBody _ = mkMemoizedEra @era

data HeaderRaw crypto = HeaderRaw
  { headerRawBody :: !(HeaderBody crypto)
  , headerRawSig :: !(KES.SignedKES (KES crypto) (HeaderBody crypto))
  }
  deriving (Generic)

deriving instance Crypto crypto => Show (HeaderRaw crypto)

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

instance Crypto crypto => EncCBOR (HeaderBodyRaw crypto) where
  encCBOR
    HeaderBodyRaw
      { hbrBlockNo
      , hbrSlotNo
      , hbrPrev
      , hbrVk
      , hbrVrfVk
      , hbrVrfRes
      , hbrBodySize
      , hbrBodyHash
      , hbrOCert
      , hbrVersionInfo
      , hbrBlockBodyContainsLeiosCert
      , hbrEbReferencesAnnouncement
      } =
      encodeListLen 12
        <> encCBOR hbrBlockNo
        <> encCBOR hbrSlotNo
        <> encCBOR hbrPrev
        <> encCBOR hbrVk
        <> encodeFixedSized hbrVrfVk
        <> encCBOR hbrVrfRes
        <> encCBOR hbrBodySize
        <> encCBOR hbrBodyHash
        <> encCBOR hbrOCert
        <> encCBOR hbrVersionInfo
        <> encCBOR hbrBlockBodyContainsLeiosCert
        <> encodeNullStrictMaybe encCBOR hbrEbReferencesAnnouncement

instance Crypto crypto => DecCBOR (HeaderBodyRaw crypto) where
  decCBOR =
    decodeRecordNamed "HeaderBody" (const 12) $
      HeaderBodyRaw
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

instance Crypto crypto => DecCBOR (Annotator (HeaderBodyRaw crypto)) where
  decCBOR = pure <$> decCBOR

instance Crypto crypto => EncCBOR (HeaderBody crypto)

deriving via
  Mem (HeaderBodyRaw crypto)
  instance
    Crypto crypto => DecCBOR (Annotator (HeaderBody crypto))

instance Crypto crypto => EncCBOR (HeaderRaw crypto) where
  encCBOR (HeaderRaw body sig) =
    encodeListLen 2 <> encCBOR body <> encodeFixedSized sig

instance Crypto crypto => DecCBOR (Annotator (HeaderRaw crypto)) where
  decCBOR =
    decodeRecordNamed "HeaderRaw" (const 2) $ do
      body <- decCBOR
      sig <- decodeFixedSized
      pure $ HeaderRaw <$> body <*> pure sig

instance Crypto c => EncCBOR (Header c)

deriving via
  Mem (HeaderRaw c)
  instance
    Crypto c => DecCBOR (Annotator (Header c))

headerBodyL :: (Era era, Crypto c) => proxy era -> Lens' (Header c) (HeaderBody c)
headerBodyL p = lens headerBody (\h b -> mkHeader p b (headerSig h))

headerBodyRawL ::
  forall era c proxy.
  (Era era, Crypto c) =>
  proxy era ->
  Lens' (Header c) (HeaderBodyRaw c)
headerBodyRawL p = headerBodyL p . memoRawTypeL @era

instance (Crypto c, Era era) => EraBlockHeader (Header c) era where
  blockHeaderSizeBlockHeaderG =
    blockHeaderL . to originalBytesSize
  blockIssuerBlockHeaderG =
    blockHeaderL . headerBodyL (Proxy @era) . to (hashKey . hbVk)
  blockBodySizeBlockHeaderL =
    blockHeaderL . headerBodyRawL (Proxy @era) . lens hbrBodySize (\hbr sz -> hbr {hbrBodySize = sz})
  blockBodyHashBlockHeaderL =
    blockHeaderL . headerBodyRawL (Proxy @era) . lens hbrBodyHash (\hbr h -> hbr {hbrBodyHash = h})
  slotNoBlockHeaderL =
    blockHeaderL . headerBodyRawL (Proxy @era) . lens hbrSlotNo (\hbr sn -> hbr {hbrSlotNo = sn})

instance (Crypto c, Era era) => LeiosEraBlockHeader (Header c) era where
  versionInfoBlockHeaderL =
    blockHeaderL . headerBodyRawL (Proxy @era) . hbrVersionInfoL
  ebReferencesAnnouncementBlockHeaderL =
    blockHeaderL . headerBodyRawL (Proxy @era) . hbrEbReferencesAnnouncementL
