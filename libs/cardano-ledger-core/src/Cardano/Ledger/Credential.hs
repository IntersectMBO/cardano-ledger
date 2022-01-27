{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    GenesisCredential (..),
    Ix,
    PaymentCredential,
    Ptr (..),
    StakeCredential,
    StakeReference (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.BaseTypes (TxIx, invalidKey)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys
  ( HasKeyRole (..),
    KeyHash,
    KeyRole (..),
  )
import Cardano.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeRecordSum,
  )
import Cardano.Ledger.Slot (SlotNo (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (asum)
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | Script hash or key hash for a payment or a staking object.
--
-- Note that credentials (unlike raw key hashes) do appear to vary from era to
-- era, since they reference the hash of a script, which can change. This
-- parameter is a phantom, however, so in actuality the instances will remain
-- the same.
data Credential (kr :: KeyRole) crypto
  = ScriptHashObj !(ScriptHash crypto)
  | KeyHashObj !(KeyHash kr crypto)
  deriving (Show, Eq, Generic, NFData, Ord)

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoThunks (Credential kr crypto)

instance CC.Crypto crypto => ToJSON (Credential kr crypto) where
  toJSON (ScriptHashObj hash) =
    Aeson.object
      [ "script hash" .= hash
      ]
  toJSON (KeyHashObj hash) =
    Aeson.object
      [ "key hash" .= hash
      ]

instance CC.Crypto crypto => FromJSON (Credential kr crypto) where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum [parser1 obj, parser2 obj]
    where
      parser1 obj = ScriptHashObj <$> obj .: "script hash"
      parser2 obj = KeyHashObj <$> obj .: "key hash"

instance CC.Crypto crypto => ToJSONKey (Credential kr crypto)

instance CC.Crypto crypto => FromJSONKey (Credential kr crypto)

type PaymentCredential crypto = Credential 'Payment crypto

type StakeCredential crypto = Credential 'Staking crypto

data StakeReference crypto
  = StakeRefBase !(StakeCredential crypto)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoThunks (StakeReference crypto)

type Ix = Word64

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr !SlotNo !TxIx !Ix
  deriving (Show, Eq, Ord, Generic, NFData, NoThunks)
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

instance
  (Typeable kr, CC.Crypto crypto) =>
  ToCBOR (Credential kr crypto)
  where
  toCBOR = \case
    KeyHashObj kh -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj hs -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR hs

instance
  (Typeable kr, CC.Crypto crypto) =>
  FromCBOR (Credential kr crypto)
  where
  fromCBOR = decodeRecordSum "Credential" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, KeyHashObj x)
      1 -> do
        x <- fromCBOR
        pure (2, ScriptHashObj x)
      k -> invalidKey k

instance ToCBORGroup Ptr where
  toCBORGroup (Ptr sl txIx certIx) =
    toCBOR sl
      <> toCBOR txIx
      <> toCBOR certIx
  encodedGroupSizeExpr size_ proxy =
    encodedSizeExpr size_ (getSlotNo <$> proxy)
      + encodedSizeExpr size_ (getIx1 <$> proxy)
      + encodedSizeExpr size_ (getIx2 <$> proxy)
    where
      getSlotNo :: Ptr -> SlotNo
      getSlotNo (Ptr a _ _) = a
      getIx1 :: Ptr -> TxIx
      getIx1 (Ptr _ x _) = x
      getIx2 :: Ptr -> Ix
      getIx2 (Ptr _ _ x) = x

  listLen _ = 3
  listLenBound _ = 3

instance FromCBORGroup Ptr where
  fromCBORGroup = Ptr <$> fromCBOR <*> fromCBOR <*> fromCBOR

newtype GenesisCredential crypto = GenesisCredential
  { unGenesisCredential :: KeyHash 'Genesis crypto
  }
  deriving (Generic)
  deriving (Show) via Quiet (GenesisCredential crypto)

instance Ord (GenesisCredential crypto) where
  compare (GenesisCredential gh) (GenesisCredential gh') = compare gh gh'

instance Eq (GenesisCredential crypto) where
  (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance
  CC.Crypto crypto =>
  ToCBOR (GenesisCredential crypto)
  where
  toCBOR (GenesisCredential kh) =
    toCBOR kh
