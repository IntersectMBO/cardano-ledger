{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Crypto.Signing.SigningKey
  ( SigningKey(..)
  , toVerification
  , toCBORXPrv
  , fromCBORXPrv
  )
where

import Cardano.Prelude

import Formatting.Buildable
import qualified Cardano.Crypto.Wallet as CC
import qualified GHC.Show
import Formatting (bprint)

import Cardano.Binary (Decoder, Encoding, FromCBOR(..), ToCBOR(..))
import Cardano.Crypto.Signing.VerificationKey (VerificationKey(..), shortVerificationKeyHexF)
import Cardano.Crypto.Hashing (hash)


-- | Wrapper around 'CC.XPrv'.
newtype SigningKey = SigningKey CC.XPrv
    deriving (NFData)

-- | Generate a verification key from a signing key. Fast (it just drops some bytes
-- off the signing key).
toVerification :: SigningKey -> VerificationKey
toVerification (SigningKey k) = VerificationKey (CC.toXPub k)

-- | Direct comparison of signing keys is a security issue (cc @vincent)
instance Eq SigningKey where
  a == b = hash a == hash b

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
