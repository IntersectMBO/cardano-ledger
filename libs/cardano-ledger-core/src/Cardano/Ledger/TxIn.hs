{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.TxIn
  ( TxId (..),
    TxIn (TxIn, ..),
    txid,
  )
where

import Cardano.Binary
  ( DecoderError (DecoderErrorCustom),
    FromCBOR (fromCBOR),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Core (TxBody)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Prelude (HeapWords (..), NFData, cborError)
import qualified Cardano.Prelude as HW
import Control.Monad (when)
import Data.Compact.HashMap (Keyed (..))
import Data.Compact.SplitMap as SMap
import Data.Text as T (pack)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

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
  heapWords (TxIn txId txIx) =
    2 + HW.heapWords txId + HW.heapWordsUnpacked txIx

-- | The input of a UTxO.
data TxIn crypto = TxInCompact !(TxId crypto) {-# UNPACK #-} !Int
  deriving (Generic)

instance CC.Crypto crypto => Split (TxIn crypto) where
  splitKey (TxInCompact txId txIx) = (txIx, toKey txId)
  joinKey txIx key = TxInCompact (fromKey key) txIx

pattern TxIn ::
  TxId crypto ->
  Natural -> -- TODO We might want to change this to Int generally
  TxIn crypto
pattern TxIn addr index <-
  TxInCompact addr (fromIntegral -> index)
  where
    TxIn addr index =
      TxInCompact addr (fromIntegral index)

{-# COMPLETE TxIn #-}

deriving instance Eq (TxIn crypto)

deriving instance Ord (TxIn crypto)

deriving instance Show (TxIn crypto)

deriving instance CC.Crypto crypto => NFData (TxIn crypto)

instance NoThunks (TxIn crypto)

instance CC.Crypto crypto => ToCBOR (TxIn crypto) where
  toCBOR (TxInCompact txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance CC.Crypto crypto => FromCBOR (TxIn crypto) where
  fromCBOR =
    decodeRecordNamed
      "TxIn"
      (const 2)
      (TxInCompact <$> fromCBOR <*> txIxFromCBOR)
    where
      txIxFromCBOR = do
        w64 :: Word64 <- fromCBOR
        when (w64 > fromIntegral (maxBound :: Int)) $
          cborError $ DecoderErrorCustom "TxIn" ("Tx index is too big: " <> T.pack (show w64))
        pure $ fromIntegral w64
