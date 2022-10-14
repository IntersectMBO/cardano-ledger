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
  )
where

import Cardano.HeapWords (HeapWords (..))
import qualified Cardano.HeapWords as HW
import Cardano.Ledger.BaseTypes (TxIx (..), mkTxIxPartial)
import Cardano.Ledger.Binary (FromCBOR (fromCBOR), ToCBOR (..), decodeRecordNamed, encodeListLen)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData)
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
newtype TxId c = TxId {_unTxId :: SafeHash c EraIndependentTxBody}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks)

deriving newtype instance CC.Crypto c => HeapWords (TxId c)

deriving newtype instance CC.Crypto c => ToCBOR (TxId c)

deriving newtype instance CC.Crypto c => FromCBOR (TxId c)

deriving newtype instance CC.Crypto c => NFData (TxId c)

instance CC.Crypto c => HeapWords (TxIn c) where
  heapWords (TxIn txId _) =
    2 + HW.heapWords txId + 1 {- txIx -}

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

deriving instance CC.Crypto c => NFData (TxIn c)

instance NoThunks (TxIn c)

instance CC.Crypto c => ToCBOR (TxIn c) where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance CC.Crypto c => FromCBOR (TxIn c) where
  fromCBOR =
    decodeRecordNamed
      "TxIn"
      (const 2)
      (TxIn <$> fromCBOR <*> fromCBOR)
