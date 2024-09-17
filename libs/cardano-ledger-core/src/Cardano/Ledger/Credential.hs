{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Credential (
  Credential (KeyHashObj, ScriptHashObj),
  PaymentCredential,
  credKeyHash,
  credKeyHashWitness,
  credScriptHash,
  credToText,
  parseCredential,
  Ptr (Ptr),
  mkPtrNormalized,
  ptrSlotNo,
  ptrTxIx,
  ptrCertIx,
  SlotNo32 (..),
  StakeCredential,
  StakeReference (..),
  normalizePtr,
)
where

import Cardano.Crypto.Hash (hashFromTextAsHex, hashToTextAsHex)
import Cardano.Ledger.BaseTypes (CertIx (..), SlotNo (..), TxIx (..), integralToBounded)
import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (..),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  FromCBOR (..),
  ToCBOR (..),
  ifDecoderVersionAtLeast,
  natVersion,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  KeyHash (..),
  KeyRole (..),
  asWitness,
 )
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  FromJSONKeyFunction (..),
  KeyValue,
  ToJSON,
  ToJSONKey (..),
  object,
  pairs,
  (.:),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Default (Default (..))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.MemPack
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (Quiet))

-- | Script hash or key hash for a payment or a staking object.
--
-- Note that credentials (unlike raw key hashes) do appear to vary from era to
-- era, since they reference the hash of a script, which can change. This
-- parameter is a phantom, however, so in actuality the instances will remain
-- the same.
data Credential (kr :: KeyRole) c
  = ScriptHashObj !(ScriptHash c)
  | KeyHashObj !(KeyHash kr c)
  deriving (Show, Eq, Generic, Ord)

instance Crypto c => NFData (Credential r c)

instance (Crypto c, Typeable kr) => MemPack (Credential kr c) where
  packedByteCount = \case
    ScriptHashObj hash -> 1 + packedByteCount hash
    KeyHashObj hash -> 1 + packedByteCount hash
  packM = \case
    ScriptHashObj hash -> packM (0 :: Word8) >> packM hash
    KeyHashObj hash -> packM (1 :: Word8) >> packM hash
  {-# INLINE packM #-}
  unpackM =
    unpackM >>= \case
      0 -> ScriptHashObj <$> unpackM
      1 -> KeyHashObj <$> unpackM
      n -> fail $ "Unrecognized Tag: " ++ show (n :: Word8)
  {-# INLINE unpackM #-}

instance Crypto e => Default (Credential r e) where
  def = KeyHashObj def

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoThunks (Credential kr c)

instance Crypto c => ToJSON (Credential kr c) where
  toJSON (ScriptHashObj hash) =
    Aeson.object
      [ "scriptHash" .= hash
      ]
  toJSON (KeyHashObj hash) =
    Aeson.object
      [ "keyHash" .= hash
      ]

instance Crypto c => FromJSON (Credential kr c) where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum [parser1 obj, parser2 obj]
    where
      parser1 obj = ScriptHashObj <$> (obj .: "scriptHash" <|> obj .: "script hash")
      parser2 obj = KeyHashObj <$> (obj .: "keyHash" <|> obj .: "key hash")

instance Crypto c => ToJSONKey (Credential kr c) where
  toJSONKey = toJSONKeyText credToText

instance Crypto c => FromJSONKey (Credential kr c) where
  fromJSONKey = FromJSONKeyTextParser parseCredential

parseCredential ::
  (MonadFail m, Crypto c) =>
  T.Text ->
  m (Credential kr c)
parseCredential t = case T.splitOn "-" t of
  ["scriptHash", hash] ->
    maybe
      (badHash hash)
      (pure . ScriptHashObj . ScriptHash)
      (hashFromTextAsHex hash)
  ["keyHash", hash] ->
    maybe
      (badHash hash)
      (pure . KeyHashObj . KeyHash)
      (hashFromTextAsHex hash)
  _ -> fail $ "Invalid credential: " <> show t
  where
    badHash h = fail $ "Invalid hash: " <> show h

credToText :: Credential kr c -> T.Text
credToText (ScriptHashObj (ScriptHash hash)) = "scriptHash-" <> hashToTextAsHex hash
credToText (KeyHashObj (KeyHash has)) = "keyHash-" <> hashToTextAsHex has

type PaymentCredential c = Credential 'Payment c

type StakeCredential c = Credential 'Staking c

credKeyHash :: Credential r c -> Maybe (KeyHash r c)
credKeyHash = \case
  KeyHashObj hk -> Just hk
  ScriptHashObj _ -> Nothing

-- | Convert a KeyHash into a Witness KeyHash. Does nothing for Script credentials.
credKeyHashWitness :: Credential r c -> Maybe (KeyHash 'Witness c)
credKeyHashWitness = credKeyHash . asWitness

-- | Extract ScriptHash from a Credential. Returns Nothing for KeyHashes
credScriptHash :: Credential kr c -> Maybe (ScriptHash c)
credScriptHash = \case
  ScriptHashObj hs -> Just hs
  KeyHashObj _ -> Nothing

data StakeReference c
  = StakeRefBase !(StakeCredential c)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, Ord)

instance NoThunks (StakeReference c)

deriving instance Crypto c => ToJSON (StakeReference c)

instance Crypto c => ToJSON (StakeReference c)
instance Crypto c => NFData (StakeReference c)
instance Crypto c => NoThunks (StakeReference c)

-- | Pointers have been deprecated and aren't used anymore. For this reason we can safely use
-- `Word32` for slots in pointers
newtype SlotNo32 = SlotNo32 Word32
  deriving stock (Show, Generic)
  deriving newtype
    (Eq, Ord, Num, Bounded, NFData, NoThunks, EncCBOR, DecCBOR, FromCBOR, ToCBOR, FromJSON, ToJSON)

-- | Pointer to a slot number, transaction index and an index in certificate
-- list.
data Ptr = Ptr {-# UNPACK #-} !SlotNo32 {-# UNPACK #-} !TxIx {-# UNPACK #-} !CertIx
  deriving (Eq, Ord, Generic)
  deriving (EncCBOR, DecCBOR) via CBORGroup Ptr

instance NFData Ptr
instance NoThunks Ptr

-- | Convert any invalid `Ptr` to a `Ptr` that contains all zeros for its fields. Any
-- pointer that contains a `SlotNo`, `TxIx` or `CertIx` that is too large to fit into
-- `Word32`, `Word16` and `Word16` respectively is considered to be an invalid
-- `Ptr`. Valid `Ptr`s will be returned unmodified.
--
-- /Note/ - This is in no way related to dangling pointers, with an exception that any
-- invalid `Ptr` is guarateed to be a dangling `Ptr`.
normalizePtr :: Ptr -> Ptr
normalizePtr ptr = ptr
{-# DEPRECATED
  normalizePtr
  "Starting with Conway era all Pointers are now normalized and this logic has been moved into the decoder"
  #-}

-- | Construct a valid `Ptr`, while protecting against overflow. Constructs a `Ptr` with
-- all zeros for its fields, whenever either one of them doesn't fit in without overflowing. Any
-- pointer that contains a `SlotNo`, `TxIx` or `CertIx` that is too large to fit into `Word32`,
-- `Word16` and `Word16` respectively is considered to be an invalid `Ptr` and result in all values
-- to be clamped to zero. In case of all valid arguments the `Ptr`s will be constructed with values
-- unmodified.
--
-- /Note/ - This functionality is in no way related to dangling pointers, with an exception that any
-- invalid `Ptr` is guarateed to be a dangling `Ptr`.
mkPtrNormalized :: Word64 -> Word64 -> Word64 -> Ptr
mkPtrNormalized slotNo txIx certIx =
  fromMaybe (Ptr (SlotNo32 0) (TxIx 0) (CertIx 0)) $ do
    slotNo32 <- integralToBounded slotNo
    txIx16 <- integralToBounded txIx
    certIx16 <- integralToBounded certIx
    pure $ Ptr (SlotNo32 slotNo32) (TxIx txIx16) (CertIx certIx16)

instance ToCBOR Ptr where
  toCBOR (Ptr slotNo txIx certIx) = toCBOR (slotNo, txIx, certIx)

instance FromCBOR Ptr where
  fromCBOR = do
    (slotNo, txIx, certIx) <- fromCBOR
    pure $ Ptr slotNo txIx certIx

instance ToJSON Ptr where
  toJSON = object . toPtrPair
  toEncoding = pairs . mconcat . toPtrPair

instance ToJSONKey Ptr

toPtrPair :: KeyValue e a => Ptr -> [a]
toPtrPair (Ptr slotNo txIndex certIndex) =
  [ "slot" .= slotNo
  , "txIndex" .= txIndex
  , "certIndex" .= certIndex
  ]

instance Show Ptr where
  showsPrec n (Ptr slotNo txIx certIx)
    | n < 1 = inner
    | otherwise = ('(' :) . inner . (")" ++)
    where
      inner =
        ("Ptr (" ++)
          . shows slotNo
          . (") (" ++)
          . shows txIx
          . (") (" ++)
          . shows certIx
          . (')' :)

ptrSlotNo :: Ptr -> SlotNo
ptrSlotNo (Ptr (SlotNo32 sn) _ _) = SlotNo (fromIntegral @Word32 @Word64 sn)

ptrTxIx :: Ptr -> TxIx
ptrTxIx (Ptr _ txIx _) = txIx

ptrCertIx :: Ptr -> CertIx
ptrCertIx (Ptr _ _ cIx) = cIx

-- NOTE: Credential serialization is unversioned, because it is needed for node-to-client
-- communication. It would be ok to change it in the future, but that will require change
-- in consensus
instance (Typeable kr, Crypto c) => EncCBOR (Credential kr c)

instance (Typeable kr, Crypto c) => DecCBOR (Credential kr c)

instance (Typeable kr, Crypto c) => ToCBOR (Credential kr c) where
  toCBOR = \case
    KeyHashObj kh -> Plain.encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj hs -> Plain.encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR hs

instance (Typeable kr, Crypto c) => FromCBOR (Credential kr c) where
  fromCBOR = Plain.decodeRecordSum "Credential" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, KeyHashObj x)
      1 -> do
        x <- fromCBOR
        pure (2, ScriptHashObj x)
      k -> Plain.invalidKey k

instance EncCBORGroup Ptr where
  encCBORGroup (Ptr sl txIx certIx) =
    encCBOR sl
      <> encCBOR txIx
      <> encCBOR certIx
  encodedGroupSizeExpr size_ proxy =
    encodedSizeExpr size_ (ptrSlotNo <$> proxy)
      + encodedSizeExpr size_ (ptrTxIx <$> proxy)
      + encodedSizeExpr size_ (ptrCertIx <$> proxy)

  listLen _ = 3
  listLenBound _ = 3

instance DecCBORGroup Ptr where
  decCBORGroup = do
    let decPtrStrict = Ptr <$> decCBOR <*> decCBOR <*> decCBOR
        decPtrNormalized = mkPtrNormalized <$> decCBOR <*> decCBOR <*> decCBOR
    ifDecoderVersionAtLeast (natVersion @7) decPtrStrict decPtrNormalized
