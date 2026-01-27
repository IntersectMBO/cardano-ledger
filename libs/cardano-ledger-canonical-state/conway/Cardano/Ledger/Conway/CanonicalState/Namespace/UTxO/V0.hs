{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.CanonicalState.Namespace.UTxO.V0 (
  module Cardano.Ledger.CanonicalState.Namespace.UTxO.V0,
  mkUtxoBabbage,
  Babbage.BabbageTxOut,
  ConwayEra,
) where

import Cardano.Ledger.Api (eraProtVerLow)
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Binary (decCBOR, encCBOR, toPlainDecoder, toPlainEncoding)
import Cardano.Ledger.CanonicalState.BasicTypes (DecodeOnChain (..), OnChain (..))
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.SCLS.NamespaceCodec
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString.Lazy as BL

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoIn
  type NamespaceEntry "utxo/v0" = UtxoOut (Babbage.BabbageTxOut ConwayEra)

mkUtxoBabbage :: Babbage.BabbageTxOut ConwayEra -> UtxoOut (Babbage.BabbageTxOut ConwayEra)
mkUtxoBabbage babbage =
  UtxoOut $
    OnChain
      babbage
      (toStrictByteString $ toPlainEncoding (eraProtVerLow @ConwayEra) (encCBOR babbage))

instance DecodeOnChain "utxo/v0" (Babbage.BabbageTxOut ConwayEra) where
  decodeOnChain b =
    case deserialiseFromBytes (toPlainDecoder (Just bl) (eraProtVerLow @ConwayEra) decCBOR) bl of
      Right (leftover, a)
        | BL.null leftover -> return a
        | otherwise -> fail "Leftover bytes when decoding TxOut"
      Left err -> fail $ "Failed to decode TxOut: " ++ show err
    where
      bl = BL.fromStrict b
