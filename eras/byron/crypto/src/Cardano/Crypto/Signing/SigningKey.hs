{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Crypto.Signing.SigningKey (
  SigningKey (..),
  toVerification,
  encCBORXPrv,
  decCBORXPrv,
  toCBORXPrv,
  fromCBORXPrv,
) where

import Cardano.Binary (
  Decoder,
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  toCborError,
 )
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..), shortVerificationKeyHexF)
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  fromPlainDecoder,
  fromPlainEncoding,
 )
import qualified Cardano.Ledger.Binary as V (Decoder, Encoding)
import Cardano.Prelude hiding (toCborError)
import Formatting (bprint)
import Formatting.Buildable
import qualified GHC.Show
import NoThunks.Class (InspectHeap (..), NoThunks (..))

-- | Wrapper around 'CC.XPrv'.
type SigningKey :: Type
newtype SigningKey = SigningKey
  { unSigningKey :: CC.XPrv
  }
  deriving newtype (NFData)
  deriving (NoThunks) via InspectHeap CC.XPrv

-- Note that there is deliberately no Eq instance. The cardano-crypto library
-- does not define one for XPrv.

-- Note that there is deliberately no Ord instance. The crypto libraries
-- encourage using key /hashes/ not keys for things like sets, map etc.

-- | Generate a verification key from a signing key. Fast (it just drops some bytes
-- off the signing key).
toVerification :: SigningKey -> VerificationKey
toVerification (SigningKey k) = VerificationKey (CC.toXPub k)

instance Show SigningKey where
  show sk = "<signing of " ++ show (toVerification sk) ++ ">"

instance Buildable SigningKey where
  build = bprint ("sec:" . shortVerificationKeyHexF) . toVerification

encCBORXPrv :: CC.XPrv -> V.Encoding
encCBORXPrv = fromPlainEncoding . toCBORXPrv

decCBORXPrv :: V.Decoder s CC.XPrv
decCBORXPrv = fromPlainDecoder fromCBORXPrv

toCBORXPrv :: CC.XPrv -> Encoding
toCBORXPrv = toCBOR . CC.unXPrv

fromCBORXPrv :: Decoder s CC.XPrv
fromCBORXPrv = toCborError . CC.xprv =<< fromCBOR @ByteString

instance ToCBOR SigningKey where
  toCBOR (SigningKey a) = toCBORXPrv a

instance FromCBOR SigningKey where
  fromCBOR = fmap SigningKey fromCBORXPrv

instance EncCBOR SigningKey

instance DecCBOR SigningKey
