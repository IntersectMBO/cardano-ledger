{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
) where

import Cardano.Crypto.Hash (hashFromTextAsHex, hashToTextAsHex)
import Cardano.Ledger.BaseTypes (
  CertIx (..),
  KeyValuePairs (..),
  SlotNo (..),
  ToKeyValuePairs (..),
  TxIx (..),
  integralToBounded,
 )
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
  ToJSON,
  ToJSONKey (..),
  (.:),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.MemPack
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import System.Random.Stateful (Random, Uniform (..), UniformRange (..))

-- | Script hash or key hash for a payment or a staking object.
--
-- Note that credentials (unlike raw key hashes) do appear to vary from era to
-- era, since they reference the hash of a script, which can change. This
-- parameter is a phantom, however, so in actuality the instances will remain
-- the same.
data Credential (kr :: KeyRole)
  = ScriptHashObj !ScriptHash
  | KeyHashObj !(KeyHash kr)
  deriving (Show, Eq, Generic, Ord)

instance NFData (Credential r)

instance Typeable kr => MemPack (Credential kr) where
  packedByteCount = \case
    ScriptHashObj hash -> packedTagByteCount + packedByteCount hash
    KeyHashObj hash -> packedTagByteCount + packedByteCount hash
  packM = \case
    ScriptHashObj hash -> packTagM 0 >> packM hash
    KeyHashObj hash -> packTagM 1 >> packM hash
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> ScriptHashObj <$> unpackM
      1 -> KeyHashObj <$> unpackM
      n -> unknownTagM @(Credential kr) n
  {-# INLINE unpackM #-}

instance Default (Credential r) where
  def = KeyHashObj def

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoThunks (Credential kr)

instance ToJSON (Credential kr) where
  toJSON = \case
    ScriptHashObj hash ->
      Aeson.object ["scriptHash" .= hash]
    KeyHashObj hash ->
      Aeson.object ["keyHash" .= hash]

instance FromJSON (Credential kr) where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum [parser1 obj, parser2 obj]
    where
      parser1 obj = ScriptHashObj <$> (obj .: "scriptHash" <|> obj .: "script hash")
      parser2 obj = KeyHashObj <$> (obj .: "keyHash" <|> obj .: "key hash")

instance ToJSONKey (Credential kr) where
  toJSONKey = toJSONKeyText credToText

instance FromJSONKey (Credential kr) where
  fromJSONKey = FromJSONKeyTextParser parseCredential

parseCredential ::
  MonadFail m =>
  T.Text ->
  m (Credential kr)
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

credToText :: Credential kr -> T.Text
credToText (ScriptHashObj (ScriptHash hash)) = "scriptHash-" <> hashToTextAsHex hash
credToText (KeyHashObj (KeyHash has)) = "keyHash-" <> hashToTextAsHex has

type PaymentCredential = Credential 'Payment

type StakeCredential = Credential 'Staking

credKeyHash :: Credential r -> Maybe (KeyHash r)
credKeyHash = \case
  KeyHashObj hk -> Just hk
  ScriptHashObj _ -> Nothing

-- | Convert a KeyHash into a Witness KeyHash. Does nothing for Script credentials.
credKeyHashWitness :: Credential r -> Maybe (KeyHash 'Witness)
credKeyHashWitness = credKeyHash . asWitness

-- | Extract ScriptHash from a Credential. Returns Nothing for KeyHashes
credScriptHash :: Credential kr -> Maybe ScriptHash
credScriptHash = \case
  ScriptHashObj hs -> Just hs
  KeyHashObj _ -> Nothing

data StakeReference
  = StakeRefBase !StakeCredential
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, Ord)

instance ToJSON StakeReference

instance NFData StakeReference

instance NoThunks StakeReference

-- | Pointers have been deprecated and aren't used anymore. For this reason we can safely use
-- `Word32` for slots in pointers
newtype SlotNo32 = SlotNo32 Word32
  deriving stock (Show, Generic)
  deriving newtype
    (Eq, Ord, Num, Bounded, NFData, NoThunks, EncCBOR, DecCBOR, FromCBOR, ToCBOR, FromJSON, ToJSON)

instance Random SlotNo32

instance Uniform SlotNo32 where
  uniformM g = SlotNo32 <$> uniformM g

instance UniformRange SlotNo32 where
  uniformRM r g = SlotNo32 <$> uniformRM (coerce r) g

-- | Pointer to a slot number, transaction index and an index in certificate
-- list.
data Ptr = Ptr {-# UNPACK #-} !SlotNo32 {-# UNPACK #-} !TxIx {-# UNPACK #-} !CertIx
  deriving (Eq, Ord, Generic)
  deriving (EncCBOR, DecCBOR) via CBORGroup Ptr
  deriving (ToJSON) via KeyValuePairs Ptr

instance Uniform Ptr where
  uniformM g = Ptr <$> uniformM g <*> (TxIx <$> uniformM g) <*> (CertIx <$> uniformM g)

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

instance ToJSONKey Ptr

instance ToKeyValuePairs Ptr where
  toKeyValuePairs (Ptr slotNo txIndex certIndex) =
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
instance Typeable kr => EncCBOR (Credential kr)

instance Typeable kr => DecCBOR (Credential kr)

instance Typeable kr => ToCBOR (Credential kr) where
  toCBOR = \case
    KeyHashObj kh -> Plain.encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj hs -> Plain.encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR hs

instance Typeable kr => FromCBOR (Credential kr) where
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
    ifDecoderVersionAtLeast (natVersion @9) decPtrStrict decPtrNormalized
