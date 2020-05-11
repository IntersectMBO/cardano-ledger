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
    RewardAcnt (..),
    StakeCredential,
    StakeReference (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeWord, encodeListLen)
import Cardano.Prelude (NFData, Natural, NoUnexpectedThunks, Typeable, Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (HasKeyRole (..), KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeRecordNamed,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))

-- | Script hash or key hash for a payment or a staking object.
data Credential (kr :: KeyRole) crypto
  = ScriptHashObj !(ScriptHash crypto)
  | KeyHashObj !(KeyHash kr crypto)
  deriving (Show, Eq, Generic, NFData, Ord)

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoUnexpectedThunks (Credential kr crypto)

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
  (Typeable kr, Typeable crypto, Crypto crypto) =>
  ToCBOR (Credential kr crypto)
  where
  toCBOR = \case
    KeyHashObj kh -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj hs -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR hs

instance
  (Typeable kr, Crypto crypto) =>
  FromCBOR (Credential kr crypto)
  where
  fromCBOR = decodeRecordNamed "Credential" (const 2) $
    decodeWord >>= \case
      0 -> KeyHashObj <$> fromCBOR
      1 -> ScriptHashObj <$> fromCBOR
      k -> invalidKey k

instance ToCBORGroup Ptr where
  toCBORGroup (Ptr sl txIx certIx) =
    toCBOR sl
      <> toCBOR (fromInteger (toInteger txIx) :: Word)
      <> toCBOR (fromInteger (toInteger certIx) :: Word)
  encodedGroupSizeExpr size_ proxy =
        encodedSizeExpr size_ (getSlotNo <$> proxy)
      + encodedSizeExpr size_ (getIx1    <$> proxy)
      + encodedSizeExpr size_ (getIx2    <$> proxy)
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

-- | An account based address for rewards
newtype RewardAcnt crypto = RewardAcnt
  { getRwdCred :: Credential 'Staking crypto
  }
  deriving (Show, Eq, Generic, Ord)
  deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

newtype GenesisCredential crypto = GenesisCredential (KeyHash 'Genesis crypto)
  deriving (Show, Generic)

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
