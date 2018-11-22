{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Crypto.Signing.SecretKey
  ( SecretKey(..)
  , toPublic
  , encodeXPrv
  , decodeXPrv
  )
where

import Cardano.Prelude

import Formatting.Buildable
import qualified Cardano.Crypto.Wallet as CC
import qualified GHC.Show
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Formatting (bprint)

import Cardano.Binary.Class (Bi(..))
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

encodeXPrv :: CC.XPrv -> E.Encoding
encodeXPrv a = encode $ CC.unXPrv a

decodeXPrv :: D.Decoder s CC.XPrv
decodeXPrv = toCborError . CC.xprv =<< decode @ByteString

instance Bi SecretKey where
  encode (SecretKey a) = encodeXPrv a
  decode = fmap SecretKey decodeXPrv
