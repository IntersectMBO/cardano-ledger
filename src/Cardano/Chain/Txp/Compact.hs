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
--

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Txp.Compact
  ( CompactTxIn
  , toCompactTxIn
  , fromCompactTxIn

  , CompactTxId
  , toCompactTxId
  , fromCompactTxId

  , CompactTxOut
  , toCompactTxOut
  , fromCompactTxOut
  )
where

import Cardano.Prelude

import Crypto.Hash (digestFromByteString)
import Cardano.Crypto.Hashing (AbstractHash (..))
import Data.Binary.Get (Get, getWord64le, runGet)
import Data.Binary.Put (Put, putWord64le, runPut)
import qualified Data.ByteArray as BA (convert)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)

import Cardano.Chain.Common.Compact
  ( CompactAddress
  , fromCompactAddress
  , toCompactAddress
  )
import Cardano.Chain.Common.Lovelace (Lovelace)
import Cardano.Chain.Txp.Tx (TxId, TxIn (..), TxOut (..))

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxIn'.
--
-- Convert using 'toCompactTxIn' and 'fromCompactTxIn'.
--
data CompactTxIn = CompactTxInUtxo {-# UNPACK #-} !CompactTxId
                                   {-# UNPACK #-} !Word32
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass NFData

instance HeapWords CompactTxIn where
  heapWords _ = 6

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
--
data CompactTxId = CompactTxId {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass NFData

instance HeapWords CompactTxId where
  heapWords _ = 5

getCompactTxId :: Get CompactTxId
getCompactTxId =
  CompactTxId <$> getWord64le
              <*> getWord64le
              <*> getWord64le
              <*> getWord64le

putCompactTxId :: CompactTxId -> Put
putCompactTxId (CompactTxId a b c d) =
  putWord64le a >> putWord64le b
                >> putWord64le c
                >> putWord64le d

toCompactTxId :: TxId -> CompactTxId
toCompactTxId txId =
  let bs = BA.convert txId :: ByteString
  in runGet getCompactTxId (BSL.fromStrict bs)

fromCompactTxId :: CompactTxId -> TxId
fromCompactTxId compactTxId =
  let bs = BSL.toStrict $ runPut (putCompactTxId compactTxId)
  in case digestFromByteString bs of
    Just d  -> AbstractHash d
    Nothing -> panic "fromCompactTxId: impossible: failed to reconstruct TxId from CompactTxId"

--------------------------------------------------------------------------------
-- Compact TxOut
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a 'TxOut'.
--
-- Convert using 'toCompactTxOut' and 'fromCompactTxOut'.
--
data CompactTxOut = CompactTxOut {-# UNPACK #-} !CompactAddress
                                 {-# UNPACK #-} !Lovelace
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass NFData

instance HeapWords CompactTxOut where
  heapWords (CompactTxOut compactAddr _) =
    3 + heapWordsUnpacked compactAddr

toCompactTxOut :: TxOut -> CompactTxOut
toCompactTxOut (TxOut addr lovelace) =
  CompactTxOut (toCompactAddress addr) lovelace

fromCompactTxOut :: CompactTxOut -> TxOut
fromCompactTxOut (CompactTxOut compactAddr lovelace) =
  TxOut (fromCompactAddress compactAddr) lovelace
