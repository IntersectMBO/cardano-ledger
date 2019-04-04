{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Crypto.Signing.SecretKey
  ( SecretKey(..)
  , toPublic
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
import Cardano.Crypto.Signing.PublicKey (PublicKey(..), shortPublicKeyHexF)
import Cardano.Crypto.Hashing (hash)


-- | Wrapper around 'CC.XPrv'.
newtype SecretKey = SecretKey CC.XPrv
    deriving (NFData)

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (CC.toXPub k)

-- | Direct comparison of secret keys is a security issue (cc @vincent)
instance Eq SecretKey where
  a == b = hash a == hash b

instance Show SecretKey where
  show sk = "<secret of " ++ show (toPublic sk) ++ ">"

instance Buildable SecretKey where
  build = bprint ("sec:" . shortPublicKeyHexF) . toPublic

toCBORXPrv :: CC.XPrv -> Encoding
toCBORXPrv a = toCBOR $ CC.unXPrv a

fromCBORXPrv :: Decoder s CC.XPrv
fromCBORXPrv = toCborError . CC.xprv =<< fromCBOR @ByteString

instance ToCBOR SecretKey where
  toCBOR (SecretKey a) = toCBORXPrv a

instance FromCBOR SecretKey where
  fromCBOR = fmap SecretKey fromCBORXPrv
