{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.CanonicalState.BasicTypes (
  OnChain (..),
  DecodeOnChain (..),
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Versioned
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
