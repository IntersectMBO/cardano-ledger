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
)
where

import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..), shortVerificationKeyHexF)
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  fromByronCBOR,
  toByronCBOR,
  toCborError,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
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

encCBORXPrv :: CC.XPrv -> Encoding
encCBORXPrv a = encCBOR $ CC.unXPrv a

decCBORXPrv :: Decoder s CC.XPrv
decCBORXPrv = toCborError . CC.xprv =<< decCBOR @ByteString

toCBORXPrv :: CC.XPrv -> Plain.Encoding
toCBORXPrv a = toCBOR $ CC.unXPrv a

fromCBORXPrv :: Plain.Decoder s CC.XPrv
fromCBORXPrv = toCborError . CC.xprv =<< fromCBOR @ByteString

instance ToCBOR SigningKey where
  toCBOR = toByronCBOR

instance FromCBOR SigningKey where
  fromCBOR = fromByronCBOR

instance EncCBOR SigningKey where
  encCBOR (SigningKey a) = encCBORXPrv a

instance DecCBOR SigningKey where
  decCBOR = fmap SigningKey decCBORXPrv
