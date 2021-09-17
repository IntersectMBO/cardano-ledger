{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Crypto.Signing.SigningKey
  ( SigningKey (..),
    toVerification,
    toCBORXPrv,
    fromCBORXPrv,
  )
where

import Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..), shortVerificationKeyHexF)
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Formatting (bprint)
import Formatting.Buildable
import qualified GHC.Show
import NoThunks.Class (InspectHeap (..), NoThunks (..))

-- | Wrapper around 'CC.XPrv'.
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

toCBORXPrv :: CC.XPrv -> Encoding
toCBORXPrv a = toCBOR $ CC.unXPrv a

fromCBORXPrv :: Decoder s CC.XPrv
fromCBORXPrv = toCborError . CC.xprv =<< fromCBOR @ByteString

instance ToCBOR SigningKey where
  toCBOR (SigningKey a) = toCBORXPrv a

instance FromCBOR SigningKey where
  fromCBOR = fmap SigningKey fromCBORXPrv
