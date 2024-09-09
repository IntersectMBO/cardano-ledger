{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Credential (
  Credential (KeyHashObj, ScriptHashObj),
  GenesisCredential (..),
  PaymentCredential,
  credKeyHash,
  credKeyHashWitness,
  credScriptHash,
  credToText,
  parseCredential,
  Ptr (Ptr),
  ptrSlotNo,
  ptrTxIx,
  ptrCertIx,
  StakeCredential,
  StakeReference (..),
  normalizePtr,
)
where

import Cardano.Crypto.Hash (hashFromTextAsHex, hashToTextAsHex)
import Cardano.Ledger.BaseTypes (CertIx (..), SlotNo (..), TxIx (..))
import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (..),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  FromCBOR (..),
  ToCBOR (..),
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
import Data.Default.Class (Default (..))
import Data.Foldable (asum)
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
  deriving (Show, Eq, Generic, NFData, Ord)

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
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoThunks (StakeReference c)

deriving instance Crypto c => ToJSON (StakeReference c)

-- TODO: implement this optimization:
-- We expect that `SlotNo` will fit into `Word32` for a very long time,
-- because we can assume that the rate at which it is incremented isn't going to
-- increase in the near future. Therefore with current rate we should be fine for
-- another 134 years. I suggest to remove this optimization in about a
-- hundred years or thereabouts, so around a year 2122 would be good.
--
-- Compaction works in a following manner. Total 8 bytes: first 4 bytes are for
-- SlotNo (s0-s3), followed by 2 bytes for CertIx (c0-c1) and 2 more bytes for TxIx (t0-t1).
--
-- @@@
--
-- ┏━━┯━━┯━━┯━━┯━━┯━━┯━━┯━━┓
-- ┃s3 s2 s1 s0┊c1 c0┊t1 t0┃
-- ┗━━┷━━┷━━┷━━┷━━┷━━┷━━┷━━┛
--
-- @@@
-- newtype Ptr = PtrCompact Word64

-- | Pointer to a slot number, transaction index and an index in certificate
-- list.
data Ptr = Ptr !SlotNo !TxIx !CertIx
  deriving (Eq, Ord, Generic, NFData, NoThunks)
  deriving (EncCBOR, DecCBOR) via CBORGroup Ptr

-- | Convert any invalid `Ptr` to a `Ptr` that contains all zeros for its fields. Any
-- pointer that contains a `SlotNo`, `TxIx` or `CertIx` that is too large to fit into
-- `Word32`, `Word16` and `Word16` respectively is considered to be an invalid
-- `Ptr`. Valid `Ptr`s will be returned unmodified.
--
-- /Note/ - This is in no way related to dangling pointers, with an exception that any
-- invalid `Ptr` is guarateed to be a dangling `Ptr`.
normalizePtr :: Ptr -> Ptr
normalizePtr ptr@(Ptr (SlotNo slotNo) (TxIx txIx) (CertIx certIx))
  | slotNo > fromIntegral (maxBound :: Word32)
      || txIx > fromIntegral (maxBound :: Word16)
      || certIx > fromIntegral (maxBound :: Word16) =
      Ptr (SlotNo 0) (TxIx 0) (CertIx 0)
  | otherwise = ptr

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

{- TODO: Uncomment this once Mainnet is ready for Ptr optimization.

-- | With this pattern synonym we can recover actual values from compacted version of `Ptr`.
pattern Ptr :: SlotNo -> TxIx -> CertIx -> Ptr
pattern Ptr slotNo txIx certIx <-
  (viewPtr -> (slotNo, txIx, certIx))

{-# COMPLETE Ptr #-}

-- | `Ptr` relies on compact representation for memory efficiency and therefore
-- it will return `Nothing` if `SlotNo` takes up more than 32 bits, which is
-- totally fine for at least another 100 years.
mkPtr :: SlotNo -> TxIx -> CertIx -> Maybe Ptr
mkPtr (SlotNo slotNo) (TxIx txIx) (CertIx certIx)
  | slotNo > fromIntegral (maxBound :: Word32) = Nothing
  | otherwise =
      Just
        $! PtrCompact
          ( (slotNo `shiftL` 32) .|. (fromIntegral txIx `shiftL` 16)
              .|. fromIntegral certIx
          )

viewPtr :: Ptr -> (SlotNo, TxIx, CertIx)
viewPtr (PtrCompact ptr) =
  (SlotNo (ptr `shiftR` 32), TxIx (fromIntegral (ptr `shiftR` 16)), CertIx (fromIntegral ptr))
-}

ptrSlotNo :: Ptr -> SlotNo
ptrSlotNo (Ptr sn _ _) = sn

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
    slotNo <- decCBOR
    txIx <- decCBOR
    certIx <- decCBOR
    pure $ Ptr slotNo txIx certIx

-- case mkPtr slotNo txIx certIx of
--   Nothing -> fail $ "SlotNo is too far into the future: " ++ show slotNo
--   Just ptr -> pure ptr

newtype GenesisCredential c = GenesisCredential
  { unGenesisCredential :: KeyHash 'Genesis c
  }
  deriving (Generic)
  deriving newtype (ToCBOR, EncCBOR)
  deriving (Show) via Quiet (GenesisCredential c)

instance Ord (GenesisCredential c) where
  compare (GenesisCredential gh) (GenesisCredential gh') = compare gh gh'

instance Eq (GenesisCredential c) where
  (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'
