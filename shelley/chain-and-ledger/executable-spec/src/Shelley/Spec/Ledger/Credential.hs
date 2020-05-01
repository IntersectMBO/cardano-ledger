{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language DerivingVia #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- for deriving NFData SlotNo

module Shelley.Spec.Ledger.Credential
  ( Credential (..)
  , GenesisCredential (..)
  , Ix
  , PaymentCredential
  , Ptr (..)
  , RewardAcnt (..)
  , StakeCredential
  , StakeReference (..)
  ) where


import           Cardano.Prelude (NFData, Natural, NoUnexpectedThunks, Typeable, Word8)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeWord)
import           GHC.Generics (Generic)

import           Shelley.Spec.Ledger.Crypto (Crypto)
import           Shelley.Spec.Ledger.Scripts (ScriptHash)
import           Shelley.Spec.Ledger.Serialization (CBORGroup (..), FromCBORGroup (..), decodeRecordNamed,
                     ToCBORGroup (..))

import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Orphans ()
import           Shelley.Spec.Ledger.Slot (SlotNo (..))
import           Shelley.Spec.Ledger.Keys (GenKeyHash, KeyHash)

-- | Script hash or key hash for a payment or a staking object.
data Credential crypto =
    ScriptHashObj !(ScriptHash crypto)
  | KeyHashObj    !(KeyHash crypto)
    deriving (Show, Eq, Generic, NFData, Ord)
    deriving ToCBOR via (CBORGroup (Credential crypto))

instance NoUnexpectedThunks (Credential crypto)

type PaymentCredential crypto = Credential crypto
type StakeCredential crypto = Credential crypto

data StakeReference crypto
  = StakeRefBase !(StakeCredential crypto)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoUnexpectedThunks (StakeReference crypto)


type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr !SlotNo !Ix !Ix
  deriving (Show, Eq, Ord, Generic, NFData, NoUnexpectedThunks)
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

instance (Typeable crypto, Crypto crypto)
  => ToCBORGroup (Credential crypto) where
  listLen _ = 2
  toCBORGroup = \case
    KeyHashObj     kh -> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj  hs -> toCBOR (1 :: Word8) <> toCBOR hs

instance (Crypto crypto) =>
  FromCBOR (Credential crypto) where
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
  listLen _ = 3

instance FromCBORGroup Ptr where
  fromCBORGroup = Ptr <$> fromCBOR <*> fromCBOR <*> fromCBOR


-- |An account based address for rewards
newtype RewardAcnt crypto = RewardAcnt
  { getRwdCred :: Credential crypto
  } deriving (Show, Eq, Generic, Ord)
    deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

newtype GenesisCredential crypto = GenesisCredential (GenKeyHash crypto)
  deriving (Show, Generic)

instance Ord (GenesisCredential crypto)
  where compare (GenesisCredential gh) (GenesisCredential gh')  = compare gh gh'

instance Eq (GenesisCredential crypto)
  where (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance (Typeable crypto, Crypto crypto)
  => ToCBOR (GenesisCredential crypto)
  where toCBOR (GenesisCredential kh) =
          toCBOR kh


