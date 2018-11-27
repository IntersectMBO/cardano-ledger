-- | A 'Dropper s' is a 'Decoder s ()', that is a decoder that returns nothing
--
--   We use 'Dropper's when we don't care about the result of decoding, for
--   example when we have deprecated some part of the serialised blockchain, but
--   still need to decode old blocks.

module Cardano.Binary.Class.Drop
  ( Dropper
  , dropBytes
  , dropInt32
  , dropList
  , dropMap
  , dropSet
  , dropTuple
  , dropTriple
  , dropWord8
  , dropWord64
  )
where

import Cardano.Prelude

import qualified Codec.CBOR.Decoding as D


type Dropper s = D.Decoder s ()

dropBytes :: Dropper s
dropBytes = void D.decodeBytes

dropInt32 :: Dropper s
dropInt32 = void D.decodeInt32

-- | Drop a list of values using the supplied `Dropper` for each element
dropList :: Dropper s -> Dropper s
dropList dropElems = do
  D.decodeListLenIndef
  D.decodeSequenceLenIndef const () identity dropElems

dropMap :: Dropper s -> Dropper s -> Dropper s
dropMap dropKey dropValue = do
  n <- D.decodeMapLen
  replicateM_ n $ dropKey >> dropValue

dropSet :: Dropper s -> Dropper s
dropSet dropElem = do
  void D.decodeTag
  n <- D.decodeListLen
  replicateM_ n dropElem

dropTuple :: Dropper s -> Dropper s -> Dropper s
dropTuple dropA dropB = do
  D.decodeListLenOf 2
  dropA
  dropB

dropTriple :: Dropper s -> Dropper s -> Dropper s -> Dropper s
dropTriple dropA dropB dropC = do
  D.decodeListLenOf 3
  dropA
  dropB
  dropC

dropWord8 :: Dropper s
dropWord8 = void D.decodeWord8

dropWord64 :: Dropper s
dropWord64 = void D.decodeWord64
