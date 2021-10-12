{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    viewTxIn,
    txid,
  )
where

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (..), encodeListLen)
import qualified Cardano.Crypto.Hash.Class as HS
import Cardano.Ledger.Core (TxBody)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash
  ( SafeHash,
    extractHash,
    hashAnnotated,
    unsafeMakeSafeHash,
  )
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Prelude (HeapWords (..))
import qualified Cardano.Prelude as HW
import Control.DeepSeq (NFData (rnf))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), noThunksInValues)
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

deriving newtype instance CC.Crypto crypto => ToCBOR (TxId crypto)

deriving newtype instance CC.Crypto crypto => FromCBOR (TxId crypto)

deriving newtype instance CC.Crypto crypto => NFData (TxId crypto)

instance HeapWords (TxIn crypto) where
  heapWords (TxInCompact32 a _ _ _ ix) =
    6 + (4 * HW.heapWordsUnpacked a) + HW.heapWordsUnpacked ix
  heapWords (TxInCompactOther tid ix) =
    3 + HW.heapWords tid + HW.heapWordsUnpacked ix

-- | The input of a UTxO.
data TxIn crypto where
  TxInCompact32 ::
    HS.SizeHash (CC.HASH crypto) ~ 32 =>
    {-# UNPACK #-} !Word64 -> -- Hash part 1/4
    {-# UNPACK #-} !Word64 -> -- Hash part 2/4
    {-# UNPACK #-} !Word64 -> -- Hash part 3/4
    {-# UNPACK #-} !Word64 -> -- Hash part 4/4
    {-# UNPACK #-} !Word64 -> -- Index
    TxIn crypto
  TxInCompactOther :: !(TxId crypto) -> {-# UNPACK #-} !Word64 -> TxIn crypto

pattern TxIn ::
  CC.Crypto crypto =>
  TxId crypto ->
  Natural -> -- TODO We might want to change this to Word64 generally
  TxIn crypto
pattern TxIn tid index <-
  (viewTxIn -> (tid, index))
  where
    TxIn tid@(TxId sh) index =
      case HS.viewHash32 (extractHash sh) of
        HS.ViewHashNot32 -> TxInCompactOther tid (fromIntegral index)
        HS.ViewHash32 a b c d -> TxInCompact32 a b c d (fromIntegral index)

{-# COMPLETE TxIn #-}

viewTxIn :: TxIn crypto -> (TxId crypto, Natural)
viewTxIn (TxInCompactOther tid i) = (tid, fromIntegral i)
viewTxIn (TxInCompact32 a b c d i) = (tid, fromIntegral i)
  where
    tid = TxId (unsafeMakeSafeHash $ HS.unsafeMkHash32 a b c d)

instance Show (TxIn crypto) where
  showsPrec d (viewTxIn -> (tid, ix)) =
    showParen (d > app_prec) $
      showString "TxIn "
        . showsPrec (app_prec + 1) tid
        . showString " "
        . showsPrec (app_prec + 1) ix
    where
      app_prec = 10

instance Ord (TxIn crypto) where
  compare (TxInCompact32 a1 b1 c1 d1 i1) (TxInCompact32 a2 b2 c2 d2 i2) =
    compare a1 a2 <> compare b1 b2 <> compare c1 c2 <> compare d1 d2
      <> compare i1 i2
  compare (viewTxIn -> (id1, ix1)) (viewTxIn -> (id2, ix2)) =
    compare id1 id2 <> compare ix1 ix2

instance Eq (TxIn crypto) where
  (==) (TxInCompact32 a1 b1 c1 d1 i1) (TxInCompact32 a2 b2 c2 d2 i2) =
    (a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2) && (i1 == i2)
  (==) (viewTxIn -> (id1, ix1)) (viewTxIn -> (id2, ix2)) =
    (id1 == id2) && (ix1 == ix2)

instance CC.Crypto crypto => NFData (TxIn crypto) where
  rnf (TxInCompactOther tid _) = seq (rnf tid) ()
  rnf (TxInCompact32 _ _ _ _ _) = ()

instance NoThunks (TxIn crypto) where
  showTypeOf _ = "TxIn"
  wNoThunks c (TxInCompactOther tid _) = noThunksInValues c [tid]
  wNoThunks _ (TxInCompact32 _ _ _ _ _) = pure Nothing -- always in normal form

instance
  CC.Crypto crypto =>
  ToCBOR (TxIn crypto)
  where
  toCBOR (viewTxIn -> (txId, index)) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance
  CC.Crypto crypto =>
  FromCBOR (TxIn crypto)
  where
  fromCBOR =
    decodeRecordNamed
      "TxIn"
      (const 2)
      (TxIn <$> fromCBOR <*> fmap natural fromCBOR)
    where
      natural :: Word64 -> Natural
      natural = fromIntegral
