{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Block header associated with Leios.
--
-- The Leios block header is the Praos block header with two additional fields
-- appended. Everything else is identical to the Praos header, see
-- "Cardano.Protocol.Praos.BlockHeader" for details.
module Cardano.Protocol.Leios.BlockHeader (
  Header (Header, headerBody, headerSig),
  HeaderBody (..),
  EbAnnouncement (..),
  headerHash,
  headerSize,
) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (
  SignableRepresentation (getSignableRepresentation),
 )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (ProtVer (pvMajor))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  decodeNullStrictMaybe,
  encodeNullStrictMaybe,
  serialize',
  unCBORGroup,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Binary.Crypto (
  decodeSignedKES,
  decodeVerKeyVRF,
  encodeSignedKES,
  encodeVerKeyVRF,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..), EraBlockHeader (..))
import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Hashes (
  EraIndependentBlockBody,
  EraIndependentBlockHeader,
  EraIndependentEb,
  HASH,
  HashAnnotated (..),
  SafeHash,
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
  mkMemoized,
 )
import Cardano.Protocol.Crypto (Crypto, KES, VRF)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Control.DeepSeq (NFData)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (lens, to)
import NoThunks.Class (NoThunks (..))

-- | Announcement of an Endorser Block (EB).
data EbAnnouncement = EbAnnouncement
  { ebAnnouncementHash :: !(SafeHash EraIndependentEb)
  , ebAnnouncementSize :: !Word32
  -- ^ Size of the EB block closure
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks, NFData)

instance EncCBOR EbAnnouncement where
  encCBOR (EbAnnouncement h s) =
    encode $
      Rec EbAnnouncement
        !> To h
        !> To s

instance DecCBOR EbAnnouncement where
  decCBOR =
    decode $
      RecD EbAnnouncement
        <! From
        <! From

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
  , hbProtVer :: !ProtVer
  -- ^ protocol version
  , hbBlockBodyContainsLeiosCert :: !Bool
  -- ^ whether the block body contains a Leios certificate
  , hbEbAnnouncement :: !(StrictMaybe EbAnnouncement)
  -- ^ Announcement of Endorser Block (EB)
  }
  deriving (Generic)

deriving instance Crypto crypto => Show (HeaderBody crypto)

deriving instance Crypto crypto => Eq (HeaderBody crypto)

instance
  Crypto crypto =>
  SignableRepresentation (HeaderBody crypto)
  where
  getSignableRepresentation hb = serialize' (pvMajor (hbProtVer hb)) hb

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

pattern Header ::
  Crypto crypto =>
  HeaderBody crypto ->
  KES.SignedKES (KES crypto) (HeaderBody crypto) ->
  Header crypto
pattern Header {headerBody, headerSig} <- (getMemoRawType -> HeaderRaw headerBody headerSig)
  where
    Header body sig = mkMemoized (pvMajor (hbProtVer body)) $ HeaderRaw body sig

{-# COMPLETE Header #-}

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
      , hbProtVer
      , hbBlockBodyContainsLeiosCert
      , hbEbAnnouncement
      } =
      encode $
        Rec HeaderBody
          !> To hbBlockNo
          !> To hbSlotNo
          !> To hbPrev
          !> To hbVk
          !> E encodeVerKeyVRF hbVrfVk
          !> To hbVrfRes
          !> To hbBodySize
          !> To hbBodyHash
          !> To hbOCert
          !> To hbProtVer
          !> To hbBlockBodyContainsLeiosCert
          !> E (encodeNullStrictMaybe encCBOR) hbEbAnnouncement

instance Crypto crypto => DecCBOR (HeaderBody crypto) where
  decCBOR =
    decode $
      RecD HeaderBody
        <! From
        <! From
        <! From
        <! From
        <! D decodeVerKeyVRF
        <! From
        <! From
        <! From
        <! mapCoder unCBORGroup From
        <! From
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

encodeHeaderRaw ::
  Crypto crypto =>
  HeaderRaw crypto ->
  Encode (Closed Dense) (HeaderRaw crypto)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance Crypto crypto => EncCBOR (HeaderRaw crypto) where
  encCBOR = encode . encodeHeaderRaw

instance Crypto crypto => DecCBOR (HeaderRaw crypto) where
  decCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance Crypto crypto => DecCBOR (Annotator (HeaderRaw crypto)) where
  decCBOR = pure <$> decCBOR

instance Crypto c => EncCBOR (Header c)

deriving via
  Mem (HeaderRaw c)
  instance
    Crypto c => DecCBOR (Annotator (Header c))

deriving newtype instance Crypto c => DecCBOR (Header c)

instance (Crypto c, Era era) => EraBlockHeader (Header c) era where
  blockIssuerBlockHeaderG =
    to (\(Block (Header hb _) _) -> hashKey (hbVk hb))
  blockHeaderSizeBlockHeaderG =
    to (\(Block hdr _) -> originalBytesSize hdr)
  blockBodySizeBlockHeaderL =
    lens
      (\(Block (Header hb _) _) -> hbBodySize hb)
      ( \(Block (Header hb sig) body) sz ->
          Block (Header hb {hbBodySize = sz} sig) body
      )
  blockBodyHashBlockHeaderL =
    lens
      (\(Block (Header hb _) _) -> hbBodyHash hb)
      ( \(Block (Header hb sig) body) h ->
          Block (Header hb {hbBodyHash = h} sig) body
      )
  slotNoBlockHeaderL =
    lens
      (\(Block (Header hb _) _) -> hbSlotNo hb)
      ( \(Block (Header hb sig) body) s ->
          Block (Header hb {hbSlotNo = s} sig) body
      )
  protVerBlockHeaderL =
    lens
      (\(Block (Header hb _) _) -> hbProtVer hb)
      ( \(Block (Header hb sig) body) pv ->
          Block (Header hb {hbProtVer = pv} sig) body
      )
