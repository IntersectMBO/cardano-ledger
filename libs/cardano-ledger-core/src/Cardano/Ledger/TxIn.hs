{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.TxIn (
  TxId (..),
  _unTxId,
  TxIn (TxIn),
  mkTxInPartial,
  txInToText,
  TxIx,
)
where

import Cardano.Crypto.Hash.Class (hashToTextAsHex)
import Cardano.HeapWords (HeapWords (..))
import qualified Cardano.HeapWords as HW
import Cardano.Ledger.BaseTypes (TxIx (..), mkTxIxPartial)
import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
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
newtype TxId c = TxId {unTxId :: SafeHash c EraIndependentTxBody}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, ToJSON, FromJSON)

_unTxId :: TxId c -> SafeHash c EraIndependentTxBody
_unTxId = unTxId
{-# DEPRECATED _unTxId "In favor of `unTxId`" #-}

deriving newtype instance Crypto c => HeapWords (TxId c)

deriving newtype instance Crypto c => EncCBOR (TxId c)

deriving newtype instance Crypto c => DecCBOR (TxId c)

deriving newtype instance Crypto c => NFData (TxId c)

instance Crypto c => HeapWords (TxIn c) where
  heapWords (TxIn txId _) =
    2 + HW.heapWords txId + 1 {- txIx -}

instance Crypto c => ToJSON (TxIn c) where
  toJSON = toJSON . txInToText
  toEncoding = toEncoding . txInToText

instance Crypto c => ToJSONKey (TxIn c) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: TxIn c -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToTextAsHex (extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

-- | The input of a UTxO.
data TxIn c = TxIn !(TxId c) {-# UNPACK #-} !TxIx
  deriving (Generic)

-- | Construct `TxIn` while throwing an error for an out of range `TxIx`. Make
-- sure to use it only for testing.
mkTxInPartial :: HasCallStack => TxId c -> Integer -> TxIn c
mkTxInPartial txId = TxIn txId . mkTxIxPartial

deriving instance Eq (TxIn c)

deriving instance Ord (TxIn c)

deriving instance Show (TxIn c)

deriving instance Crypto c => NFData (TxIn c)

instance NoThunks (TxIn c)

instance Crypto c => EncCBOR (TxIn c) where
  encCBOR (TxIn txId index) =
    encodeListLen 2
      <> encCBOR txId
      <> encCBOR index

instance Crypto c => DecCBOR (TxIn c) where
  decCBOR =
    decodeRecordNamed
      "TxIn"
      (const 2)
      (TxIn <$> decCBOR <*> decCBOR)
