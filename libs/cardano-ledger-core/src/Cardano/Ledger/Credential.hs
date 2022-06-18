{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    GenesisCredential (..),
    PaymentCredential,
    Ptr (Ptr),
    ptrSlotNo,
    ptrTxIx,
    ptrCertIx,
    StakeCredential,
    StakeReference (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.BaseTypes
  ( CertIx (..),
    TxIx (..),
    invalidKey,
  )
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
import Data.Aeson
  ( FromJSON (..),
    FromJSONKey,
    ToJSON (..),
    ToJSONKey,
    (.:),
    (.=),
  )
import qualified Data.Aeson as Aeson
-- import Data.Bits (Bits (shiftL, shiftR, (.|.)))
import Data.Foldable (asum)
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
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

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
          . (") " ++)
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
    encodedSizeExpr size_ (ptrSlotNo <$> proxy)
      + encodedSizeExpr size_ (ptrTxIx <$> proxy)
      + encodedSizeExpr size_ (ptrCertIx <$> proxy)

  listLen _ = 3
  listLenBound _ = 3

instance FromCBORGroup Ptr where
  fromCBORGroup = do
    slotNo <- fromCBOR
    txIx <- fromCBOR
    certIx <- fromCBOR
    pure $ Ptr slotNo txIx certIx

-- case mkPtr slotNo txIx certIx of
--   Nothing -> fail $ "SlotNo is too far into the future: " ++ show slotNo
--   Just ptr -> pure ptr

newtype GenesisCredential crypto = GenesisCredential
  { unGenesisCredential :: KeyHash 'Genesis crypto
  }
  deriving (Generic)
  deriving (Show) via Quiet (GenesisCredential crypto)

instance Ord (GenesisCredential crypto) where
  compare (GenesisCredential gh) (GenesisCredential gh') = compare gh gh'

instance Eq (GenesisCredential crypto) where
  (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance CC.Crypto crypto => ToCBOR (GenesisCredential crypto) where
  toCBOR (GenesisCredential kh) = toCBOR kh
