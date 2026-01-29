{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 (
  UtxoIn (..),
  UtxoOut (..),
  mkUtxo,
) where

import Cardano.Ledger.CanonicalState.BasicTypes
import Cardano.Ledger.Binary (encCBOR, decodeFull', serialize')
import Cardano.Ledger.Core (TxOut, EraScript, EraTxOut, eraProtVerLow)
import Cardano.Ledger.Hashes (originalBytes)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec
import Data.MemPack
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

-- | Key for the UTxO entry.
newtype UtxoIn
  = UtxoKeyIn TxIn
  deriving (Eq, Ord, Show)
  deriving (Generic)

instance IsKey UtxoIn where
  keySize = namespaceKeySize @"utxo/v0"
  packKeyM (UtxoKeyIn (TxIn (TxId a) b)) = do
    packByteStringM (originalBytes a)
    packM b
  unpackKeyM = do
    a <- unpackM -- FIXME read bytestirng and create unsafe hash
    b <- unpackM
    return $ UtxoKeyIn (TxIn a b)

instance CanonicalCBOREntryEncoder "utxo/v0" (UtxoOut era) where
  encodeEntry n = toCanonicalCBOR (Proxy @"utxo/v0") n

instance
  DecodeOnChain "utxo/v0" (TxOut era) =>
  CanonicalCBOREntryDecoder "utxo/v0" (UtxoOut era)
  where
  decodeEntry = fromCanonicalCBOR

instance ToCanonicalCBOR "utxo/v0" (UtxoOut era) where
  toCanonicalCBOR v (UtxoOut out) = toCanonicalCBOR v out

instance
  DecodeOnChain "utxo/v0" (TxOut era) =>
  FromCanonicalCBOR "utxo/v0" (UtxoOut era)
  where
  fromCanonicalCBOR = fmap UtxoOut <$> fromCanonicalCBOR

-- | As Utxo should represent an on chain data it's encoding should be
-- extactly the same as on the wire protocol. But because the structure
-- and types may not be the same in different eras we just keep a final
-- encoding here.
newtype UtxoOut era = UtxoOut (OnChain (TxOut era))

deriving instance
  Eq (TxOut era) =>
  Eq (UtxoOut era)

deriving instance
  Show (TxOut era) =>
  Show (UtxoOut era)

mkUtxo :: forall era . (EraTxOut era) => TxOut era -> UtxoOut era
mkUtxo txOut =
  UtxoOut $ OnChain txOut $ serialize' (eraProtVerLow @era) (encCBOR txOut)

instance (EraTxOut era, EraScript era, TxOut era ~ x) => DecodeOnChain "utxo/v0" x where
  decodeOnChain = either (fail . show) pure . decodeFull' (eraProtVerLow @era)
