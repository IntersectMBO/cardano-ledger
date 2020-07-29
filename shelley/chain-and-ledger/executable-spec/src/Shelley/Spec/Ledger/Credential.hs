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
  ( Credential (..),
    GenesisCredential (..),
    Ix,
    PaymentCredential,
    Ptr (..),
    StakeCredential,
    StakeReference (..),
    credentialUsesScript,
    stakeReferenceUsesScript,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Prelude (NFData, Natural, NoUnexpectedThunks, Typeable, Word8, asum)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Crypto (Crypto)
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
data Credential (kr :: KeyRole) crypto
  = ScriptHashObj {-# UNPACK #-} !(ScriptHash crypto)
  | KeyHashObj {-# UNPACK #-} !(KeyHash kr crypto)
  deriving (Show, Eq, Generic, NFData, Ord)

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoUnexpectedThunks (Credential kr crypto)

instance
  Crypto crypto =>
  ToJSON (Credential kr crypto)
  where
  toJSON (ScriptHashObj hash) =
    Aeson.object
      [ "script hash" .= hash
      ]
  toJSON (KeyHashObj hash) =
    Aeson.object
      [ "key hash" .= hash
      ]

instance Crypto crypto => FromJSON (Credential kr crypto) where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum [parser1 obj, parser2 obj]
    where
      parser1 obj = ScriptHashObj <$> obj .: "script hash"
      parser2 obj = KeyHashObj <$> obj .: "key hash"

instance Crypto crypto => ToJSONKey (Credential kr crypto)

instance Crypto crypto => FromJSONKey (Credential kr crypto)

type PaymentCredential crypto = Credential 'Payment crypto

type StakeCredential crypto = Credential 'Staking crypto

data StakeReference crypto
  = StakeRefBase !(StakeCredential crypto)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoUnexpectedThunks (StakeReference crypto)

type Ix = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr !SlotNo !Ix !Ix
  deriving (Show, Eq, Ord, Generic, NFData, NoUnexpectedThunks)
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

instance
  (Typeable kr, Crypto crypto) =>
  ToCBOR (Credential kr crypto)
  where
  toCBOR = \case
    KeyHashObj kh -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj hs -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR hs

instance
  (Typeable kr, Crypto crypto) =>
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

newtype GenesisCredential crypto = GenesisCredential
  { unGenesisCredential ::
      KeyHash 'Genesis crypto
  }
  deriving (Generic)
  deriving (Show) via Quiet (GenesisCredential crypto)

instance Ord (GenesisCredential crypto) where
  compare (GenesisCredential gh) (GenesisCredential gh') = compare gh gh'

instance Eq (GenesisCredential crypto) where
  (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (GenesisCredential crypto)
  where
  toCBOR (GenesisCredential kh) =
    toCBOR kh

credentialUsesScript :: Credential kr crypto -> Bool
credentialUsesScript ScriptHashObj {} = True
credentialUsesScript KeyHashObj {} = False

stakeReferenceUsesScript :: StakeReference crypto -> Bool
stakeReferenceUsesScript (StakeRefBase cred) = credentialUsesScript cred
stakeReferenceUsesScript (StakeRefPtr _) = False
stakeReferenceUsesScript StakeRefNull = False
