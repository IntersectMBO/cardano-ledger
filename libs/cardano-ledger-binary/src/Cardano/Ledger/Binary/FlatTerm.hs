{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Binary.FlatTerm (
  C.FlatTerm,
  C.TermToken (..),
  toFlatTerm,
  fromFlatTerm,
  C.validFlatTerm,
)
where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import qualified Codec.CBOR.FlatTerm as C

toFlatTerm :: Version -> Encoding -> C.FlatTerm
toFlatTerm version = C.toFlatTerm . toPlainEncoding version

fromFlatTerm :: Version -> (forall s. Decoder s a) -> C.FlatTerm -> Either String a
fromFlatTerm version decoder = C.fromFlatTerm (toPlainDecoder version decoder)
