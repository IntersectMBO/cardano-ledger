{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.TxIn (
  TxId (..),
  TxIn (TxIn),
  mkTxInPartial,
  txInToText,
  TxIx,
) where

import Cardano.Crypto.Hash.Class (hashToTextAsHex)
import Cardano.Ledger.BaseTypes (TxIx (..), mkTxIxPartial)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  TokenType (..),
  decodeMemPack,
  decodeRecordNamed,
  encodeListLen,
  peekTokenType,
 )
import Cardano.Ledger.Hashes (EraIndependentTxBody, SafeHash, extractHash)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import Data.MemPack
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))

-- ===================================================================================
-- Because we expect other Era's to import and use TxId, TxIn, TxOut, we use the weakest
-- constraint possible when deriving their instances. A Stronger constraint, Gathering
-- many constraints together, like:  type Strong = (C1 x, C2 x, ..., Cn x)
-- may make this file look systematic by having things like:
-- derving instance (Strong x) => Foo x,  for many Foo (Eq, Show, NfData, etc) BUT this
-- forces unnecessary requirements on any new Era which tries to embed one of these
-- types in their own datatypes, if they then try and derive (Foo TheirDataType).
-- ====================================================================================

-- | A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId {unTxId :: SafeHash EraIndependentTxBody}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, ToJSON, FromJSON, EncCBOR, DecCBOR, NFData, MemPack)

instance ToJSON TxIn where
  toJSON = toJSON . txInToText
  toEncoding = toEncoding . txInToText

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText txInToText

txInToText :: TxIn -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToTextAsHex (extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

-- | The input of a UTxO.
data TxIn = TxIn !TxId {-# UNPACK #-} !TxIx
  deriving (Generic, Eq, Ord, Show)

instance MemPack TxIn where
  packedByteCount (TxIn txId txIx) = packedByteCount txId + packedByteCount txIx
  {-# INLINE packedByteCount #-}
  packM (TxIn txId txIx) = packM txId >> packM txIx
  {-# INLINE packM #-}
  unpackM = TxIn <$> unpackM <*> unpackM
  {-# INLINE unpackM #-}

-- | Construct `TxIn` while throwing an error for an out of range `TxIx`. Make
-- sure to use it only for testing.
mkTxInPartial :: HasCallStack => TxId -> Integer -> TxIn
mkTxInPartial txId = TxIn txId . mkTxIxPartial

instance NFData TxIn

instance NoThunks TxIn

instance EncCBOR TxIn where
  encCBOR (TxIn txId index) =
    encodeListLen 2
      <> encCBOR txId
      <> encCBOR index

instance DecCBOR TxIn where
  decCBOR =
    decodeRecordNamed
      "TxIn"
      (const 2)
      (TxIn <$> decCBOR <*> decCBOR)

instance DecShareCBOR TxIn where
  decShareCBOR _ =
    peekTokenType >>= \case
      TypeBytes -> decodeMemPack
      TypeBytesIndef -> decodeMemPack
      _ -> decCBOR
