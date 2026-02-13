{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.BasicTypes (
  OnChain (..),
  DecodeOnChain (..),
  CanonicalCoin (..),
) where

import Cardano.Ledger.BaseTypes (Anchor (..), StrictMaybe (..))
import Cardano.Ledger.CanonicalState.LedgerCBOR
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), peekTokenType)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Versioned
import qualified Codec.CBOR.Decoding as D
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)

-- | Wrapper type that tells that the type is the type that is kept on-chain
-- for such types we want to keep exactly the same encoding as on the wire.
--
-- We still tag the type with an original one to be able to distinguish between
-- them.
data OnChain (a :: Type) = OnChain {getValue :: !a, getWireEncoding :: !BS.ByteString}
  deriving stock (Generic)

instance Eq a => Eq (OnChain a) where
  (OnChain _ bs1) == (OnChain _ bs2) = bs1 == bs2

instance Ord a => Ord (OnChain a) where
  compare (OnChain _ bs1) (OnChain _ bs2) = compare bs1 bs2

instance Show a => Show (OnChain a) where
  show (OnChain a b) = show (Base16.encode b) ++ ":" ++ show a

instance ToCanonicalCBOR v (OnChain a) where
  toCanonicalCBOR v (OnChain _ bs) = toCanonicalCBOR v bs

instance DecodeOnChain v a => FromCanonicalCBOR v (OnChain a) where
  fromCanonicalCBOR = do
    Versioned bs <- fromCanonicalCBOR
    a <- decodeOnChain @v bs
    return $ Versioned (OnChain a bs)

-- | Helper types to encode on-chain types, it's used so
-- it would be possible to pass input bytestring to
-- `toPlainDecoder`.
class DecodeOnChain (v :: Symbol) (a :: Type) where
  decodeOnChain :: BS.ByteString -> CanonicalDecoder s a

-- | Wrapper for the coin type.
--
-- Despite the fact that Coin is on-chain type, we do not want to use
-- 'OnChain' wrapper for it. Because it's expected that if we keep chain
-- structure like transaction in canonical state, then we should keep entire
-- structure there and keep that as a whole, like 'UTxOut'.
newtype CanonicalCoin = CanonicalCoin {unCoin :: CompactForm Coin}
  deriving (Eq, Ord, Show, Generic)

instance FromCanonicalCBOR v CanonicalCoin where
  fromCanonicalCBOR = fmap (CanonicalCoin . CompactCoin) <$> fromCanonicalCBOR

instance ToCanonicalCBOR v CanonicalCoin where
  toCanonicalCBOR v (CanonicalCoin (CompactCoin c)) = toCanonicalCBOR v c

instance ToCanonicalCBOR v a => ToCanonicalCBOR v (StrictMaybe a) where
  toCanonicalCBOR v SNothing = toCanonicalCBOR v ()
  toCanonicalCBOR v (SJust x) = toCanonicalCBOR v x

instance FromCanonicalCBOR v a => FromCanonicalCBOR v (StrictMaybe a) where
  fromCanonicalCBOR = do
    mt <- peekTokenType
    case mt of
      D.TypeNull -> do
        Versioned () <- fromCanonicalCBOR
        pure (Versioned SNothing)
      _ -> fmap SJust <$> fromCanonicalCBOR

deriving via
  LedgerCBOR v Anchor
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v Anchor

deriving via
  LedgerCBOR v Anchor
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v Anchor

deriving via
  LedgerCBOR v ScriptHash
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v ScriptHash

deriving via
  LedgerCBOR v ScriptHash
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v ScriptHash
