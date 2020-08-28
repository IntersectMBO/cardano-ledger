{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Shelley.Spec.Ledger.Credential
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
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NFData, Natural, NoUnexpectedThunks, Typeable, Word8, asum)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Keys
  ( HasKeyRole (..),
    KeyHash,
    KeyRole (..),
  )
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeRecordSum,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))

-- | Script hash or key hash for a payment or a staking object.
data Credential (kr :: KeyRole) era
  = ScriptHashObj {-# UNPACK #-} !(ScriptHash era)
  | KeyHashObj {-# UNPACK #-} !(KeyHash kr era)
  deriving (Show, Eq, Generic, NFData, Ord)

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoUnexpectedThunks (Credential kr era)

instance
  Era era =>
  ToJSON (Credential kr era)
  where
  toJSON (ScriptHashObj hash) =
    Aeson.object
      [ "script hash" .= hash
      ]
  toJSON (KeyHashObj hash) =
    Aeson.object
      [ "key hash" .= hash
      ]

instance Era era => FromJSON (Credential kr era) where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum [parser1 obj, parser2 obj]
    where
      parser1 obj = ScriptHashObj <$> obj .: "script hash"
      parser2 obj = KeyHashObj <$> obj .: "key hash"

instance Era era => ToJSONKey (Credential kr era)

instance Era era => FromJSONKey (Credential kr era)

type PaymentCredential era = Credential 'Payment era

type StakeCredential era = Credential 'Staking era

data StakeReference era
  = StakeRefBase !(StakeCredential era)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoUnexpectedThunks (StakeReference era)

type Ix = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr !SlotNo !Ix !Ix
  deriving (Show, Eq, Ord, Generic, NFData, NoUnexpectedThunks)
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

instance
  (Typeable kr, Era era) =>
  ToCBOR (Credential kr era)
  where
  toCBOR = \case
    KeyHashObj kh -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj hs -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR hs

instance
  (Typeable kr, Era era) =>
  FromCBOR (Credential kr era)
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
      <> toCBOR (fromInteger (toInteger txIx) :: Word)
      <> toCBOR (fromInteger (toInteger certIx) :: Word)
  encodedGroupSizeExpr size_ proxy =
    encodedSizeExpr size_ (getSlotNo <$> proxy)
      + encodedSizeExpr size_ (getIx1 <$> proxy)
      + encodedSizeExpr size_ (getIx2 <$> proxy)
    where
      getSlotNo :: Ptr -> SlotNo
      getSlotNo (Ptr a _ _) = a
      getIx1, getIx2 :: Ptr -> Ix
      getIx1 (Ptr _ x _) = x
      getIx2 (Ptr _ _ x) = x

  listLen _ = 3
  listLenBound _ = 3

instance FromCBORGroup Ptr where
  fromCBORGroup = Ptr <$> fromCBOR <*> fromCBOR <*> fromCBOR

newtype GenesisCredential era = GenesisCredential
  { unGenesisCredential ::
      KeyHash 'Genesis era
  }
  deriving (Generic)
  deriving (Show) via Quiet (GenesisCredential era)

instance Ord (GenesisCredential era) where
  compare (GenesisCredential gh) (GenesisCredential gh') = compare gh gh'

instance Eq (GenesisCredential era) where
  (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance
  (Typeable era, Era era) =>
  ToCBOR (GenesisCredential era)
  where
  toCBOR (GenesisCredential kh) =
    toCBOR kh
