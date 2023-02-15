-- | /DEPRECATED/ - This module is deprecated and used in Byron era exclusively. For newer
-- implementation of this functionality use `Cardano.Ledger.Binary.Decoding.DecCBOR.dropCBOR`
--
-- A 'Dropper s' is a 'Decoder s ()', that is a decoder that returns nothing
--
--   We use 'Dropper's when we don't care about the result of decoding, for
--   example when we have deprecated some part of the serialised blockchain, but
--   still need to decode old blocks.
module Cardano.Ledger.Binary.Decoding.Drop (
  Dropper,
  dropBytes,
  dropInt32,
  dropList,
  dropMap,
  dropSet,
  dropTuple,
  dropTriple,
  dropWord8,
  dropWord64,
)
where

import Cardano.Ledger.Binary.Decoding.Decoder
import Control.Monad (replicateM_)
import Data.Functor (void)

type Dropper s = Decoder s ()

dropBytes :: Dropper s
dropBytes = void decodeBytes

dropInt32 :: Dropper s
dropInt32 = void decodeInt32

-- | Drop a list of values using the supplied `Dropper` for each element
dropList :: Dropper s -> Dropper s
dropList dropElems = do
  decodeListLenIndef
  decodeSequenceLenIndef const () id dropElems

dropMap :: Dropper s -> Dropper s -> Dropper s
dropMap dropKey dropValue = do
  n <- decodeMapLen
  replicateM_ n $ dropKey >> dropValue

dropSet :: Dropper s -> Dropper s
dropSet dropElem = do
  void decodeTag
  n <- decodeListLen
  replicateM_ n dropElem

dropTuple :: Dropper s -> Dropper s -> Dropper s
dropTuple dropA dropB = do
  decodeListLenOf 2
  dropA
  dropB

dropTriple :: Dropper s -> Dropper s -> Dropper s -> Dropper s
dropTriple dropA dropB dropC = do
  decodeListLenOf 3
  dropA
  dropB
  dropC

dropWord8 :: Dropper s
dropWord8 = void decodeWord8

dropWord64 :: Dropper s
dropWord64 = void decodeWord64
