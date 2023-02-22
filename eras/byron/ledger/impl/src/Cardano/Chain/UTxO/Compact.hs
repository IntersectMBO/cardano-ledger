{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The UTxO is large and is kept in-memory. It is important to use as
-- small a representation as possible to keep overall memory use reasonable.
--
-- This module provides a special compact representation for data types
-- contained within the UTxO.
--
-- The idea here is that the compact representation is optimised only for
-- storage size and does not have to be the same as the representation used
-- when operating on the data. Conversion functions are to be used when
-- inserting and retrieving values from the UTxO.
module Cardano.Chain.UTxO.Compact (
  CompactTxIn (..),
  toCompactTxIn,
  fromCompactTxIn,
  CompactTxId,
  toCompactTxId,
  fromCompactTxId,
  CompactTxOut (..),
  toCompactTxOut,
  fromCompactTxOut,
)
where

import Cardano.Chain.Common.Compact (
  CompactAddress,
  fromCompactAddress,
  toCompactAddress,
 )
import Cardano.Chain.Common.Lovelace (Lovelace)
import Cardano.Chain.UTxO.Tx (TxId, TxIn (..), TxOut (..))
import Cardano.Crypto.Hashing (hashToBytes, unsafeHashFromBytes)
import Cardano.HeapWords (HeapWords (..), heapWordsUnpacked)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Data.Binary.Get (Get, getWord64le, runGet)
import Data.Binary.Put (Put, putWord64le, runPut)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxIn'.
--
-- Convert using 'toCompactTxIn' and 'fromCompactTxIn'.
data CompactTxIn
  = CompactTxInUtxo
      {-# UNPACK #-} !CompactTxId
      {-# UNPACK #-} !Word16
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, NoThunks)

instance HeapWords CompactTxIn where
  heapWords _ =
    -- We have
    --
    -- > data CompactTxIn = CompactTxInUtxo {-# UNPACK #-} !CompactTxId
    -- >                                    {-# UNPACK #-} !Word16
    --
    -- so 'CompactTxInUtxo' requires:
    --
    -- - 1 word for the 'CompactTxInUtxo' object header
    -- - 4 words (on a 64-bit arch) for the unpacked 'CompactTxId'
    -- - 1 word for the unpacked 'Word16'
    --
    -- +---------------------------------------------+
    -- │CompactTxInUtxo│Word#|Word#│Word#│Word#│Word#│
    -- +---------------------------------------------+
    --
    6

instance ToCBOR CompactTxIn where
  toCBOR = toByronCBOR

instance FromCBOR CompactTxIn where
  fromCBOR = fromByronCBOR

instance DecCBOR CompactTxIn where
  decCBOR = do
    enforceSize "CompactTxIn" 2
    CompactTxInUtxo
      <$> decCBOR
      <*> decCBOR

instance EncCBOR CompactTxIn where
  encCBOR (CompactTxInUtxo txId txIndex) =
    encodeListLen 2
      <> encCBOR txId
      <> encCBOR txIndex

toCompactTxIn :: TxIn -> CompactTxIn
toCompactTxIn (TxInUtxo txId txIndex) =
  CompactTxInUtxo (toCompactTxId txId) txIndex

fromCompactTxIn :: CompactTxIn -> TxIn
fromCompactTxIn (CompactTxInUtxo compactTxId txIndex) =
  TxInUtxo (fromCompactTxId compactTxId) txIndex

--------------------------------------------------------------------------------
-- Compact TxId
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxId'.
--
-- Convert using 'toCompactTxId' and 'fromCompactTxId'.
--
-- Compared to a normal 'TxId', this takes 5 heap words rather than 12.
data CompactTxId
  = CompactTxId
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (NFData, NoThunks)

instance HeapWords CompactTxId where
  heapWords _ =
    -- We have
    --
    -- > data CompactTxId = CompactTxId {-# UNPACK #-} !Word64
    -- >                                {-# UNPACK #-} !Word64
    -- >                                {-# UNPACK #-} !Word64
    -- >                                {-# UNPACK #-} !Word64
    --
    -- so 'CompactTxId' requires:
    --
    -- - 1 word for the 'CompactTxId' object header
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64'
    --
    -- +-----------------------------------+
    -- │CompactTxId│Word#│Word#│Word#│Word#│
    -- +-----------------------------------+
    --
    5

instance ToCBOR CompactTxId where
  toCBOR = toByronCBOR

instance FromCBOR CompactTxId where
  fromCBOR = fromByronCBOR

instance DecCBOR CompactTxId where
  decCBOR = do
    enforceSize "CompactTxId" 4
    CompactTxId
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR

instance EncCBOR CompactTxId where
  encCBOR (CompactTxId a b c d) =
    encodeListLen 4
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d

getCompactTxId :: Get CompactTxId
getCompactTxId =
  CompactTxId
    <$> getWord64le
    <*> getWord64le
    <*> getWord64le
    <*> getWord64le

putCompactTxId :: CompactTxId -> Put
putCompactTxId (CompactTxId a b c d) =
  putWord64le a
    >> putWord64le b
    >> putWord64le c
    >> putWord64le d

toCompactTxId :: TxId -> CompactTxId
toCompactTxId =
  runGet getCompactTxId . BSL.fromStrict . hashToBytes

fromCompactTxId :: CompactTxId -> TxId
fromCompactTxId =
  unsafeHashFromBytes . BSL.toStrict . runPut . putCompactTxId

--------------------------------------------------------------------------------
-- Compact TxOut
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxOut'.
--
-- Convert using 'toCompactTxOut' and 'fromCompactTxOut'.
data CompactTxOut
  = CompactTxOut
      {-# UNPACK #-} !CompactAddress
      {-# UNPACK #-} !Lovelace
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, NoThunks)

instance HeapWords CompactTxOut where
  heapWords (CompactTxOut compactAddr _) =
    -- We have
    --
    -- > data CompactTxOut = CompactTxOut {-# UNPACK #-} !CompactAddress
    -- >                                  {-# UNPACK #-} !Lovelace
    -- > newtype CompactAddress = CompactAddress ShortByteString
    -- > newtype Lovelace = Lovelace { getLovelace :: Word64 }
    --
    -- so @CompactTxOut {-# UNPACK #-} !CompactAddress {-# UNPACK #-} !Lovelace@
    -- requires:
    --
    -- - 1 word for the 'CompactTxOut' object header
    -- - 1 word for the pointer to the byte array object
    -- - 1 word (on a 64-bit arch) for the unpacked 'Word64' ('Lovelace')
    -- - the heap words required by the byte array object
    --
    -- Note that for the sake of uniformity, we use 'heapWordsUnpacked' to
    -- account for the level of indirection removed by the @UNPACK@ pragma.
    --
    -- +----------------------+
    -- │CompactTxOut│ * │Word#│
    -- +--------------+-------+
    --                |
    --                v
    --                +--------------+
    --                │BA#│sz│payload│
    --                +--------------+
    --
    3 + heapWordsUnpacked compactAddr

instance ToCBOR CompactTxOut where
  toCBOR = toByronCBOR

instance FromCBOR CompactTxOut where
  fromCBOR = fromByronCBOR

instance DecCBOR CompactTxOut where
  decCBOR = do
    enforceSize "CompactTxOut" 2
    CompactTxOut
      <$> decCBOR
      <*> decCBOR

instance EncCBOR CompactTxOut where
  encCBOR (CompactTxOut compactAddr lovelace) =
    encodeListLen 2
      <> encCBOR compactAddr
      <> encCBOR lovelace

toCompactTxOut :: TxOut -> CompactTxOut
toCompactTxOut (TxOut addr lovelace) =
  CompactTxOut (toCompactAddress addr) lovelace

fromCompactTxOut :: CompactTxOut -> TxOut
fromCompactTxOut (CompactTxOut compactAddr lovelace) =
  TxOut (fromCompactAddress compactAddr) lovelace
