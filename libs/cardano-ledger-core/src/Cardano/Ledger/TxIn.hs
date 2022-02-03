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

module Cardano.Ledger.TxIn
  ( TxId (..),
    TxIn (TxIn),
    mkTxInPartial,
    TxIx,
    txid,
  )
where

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (..), encodeListLen)
import Cardano.Ledger.BaseTypes (TxIx (..), mkTxIxPartial, txIxToInt)
import Cardano.Ledger.Core (TxBody)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Prelude (HeapWords (..), NFData)
import qualified Cardano.Prelude as HW
import Data.Compact.HashMap (Keyed (..))
import Data.Compact.SplitMap as SMap
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | Compute the id of a transaction.
txid ::
  forall era.
  Era era =>
  TxBody era ->
  TxId (Crypto era)
txid = TxId . hashAnnotated

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
newtype TxId crypto = TxId {_unTxId :: SafeHash crypto EraIndependentTxBody}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, HeapWords)

deriving newtype instance CC.Crypto crypto => Keyed (TxId crypto)

deriving newtype instance CC.Crypto crypto => ToCBOR (TxId crypto)

deriving newtype instance CC.Crypto crypto => FromCBOR (TxId crypto)

deriving newtype instance CC.Crypto crypto => NFData (TxId crypto)

instance CC.Crypto crypto => HeapWords (TxIn crypto) where
  heapWords (TxIn txId _) =
    2 + HW.heapWords txId + 1 {- txIx -}

-- | The input of a UTxO.
data TxIn crypto = TxIn !(TxId crypto) {-# UNPACK #-} !TxIx
  deriving (Generic)

-- | Construct `TxIn` while throwing an error for an out of range `TxIx`. Make
-- sure to use it only for testing.
mkTxInPartial :: HW.HasCallStack => TxId crypto -> Integer -> TxIn crypto
mkTxInPartial txId = TxIn txId . mkTxIxPartial

instance CC.Crypto crypto => Split (TxIn crypto) where
  splitKey (TxIn txId txIx) = (txIxToInt txIx, toKey txId)
  joinKey txIx key =
    -- `fromIntegral` is safe here, since we have only valid values in the SplitMap:
    TxIn (fromKey key) (TxIx (fromIntegral txIx))

deriving instance Eq (TxIn crypto)

deriving instance Ord (TxIn crypto)

deriving instance Show (TxIn crypto)

deriving instance CC.Crypto crypto => NFData (TxIn crypto)

instance NoThunks (TxIn crypto)

instance CC.Crypto crypto => ToCBOR (TxIn crypto) where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance CC.Crypto crypto => FromCBOR (TxIn crypto) where
  fromCBOR =
    decodeRecordNamed
      "TxIn"
      (const 2)
      (TxIn <$> fromCBOR <*> fromCBOR)
