{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Signing done with public/private keys.

module Cardano.Crypto.Signing.Types.Signing
  (
       -- * Keys
    PublicKey(..)
  , SecretKey(..)
  , toPublic
  , encodeXPrv
  , decodeXPrv
  , formatFullPublicKey
  , fullPublicKeyF
  , fullPublicKeyHexF
  , shortPublicKeyHexF
  , parseFullPublicKey

       -- * Signing and verification
  , Signature(..)
  , fullSignatureHexF
  , parseFullSignature
  , Signed(..)

       -- * Proxy signature scheme
  , ProxyCert(..)
  , fullProxyCertHexF
  , parseFullProxyCert
  , AProxySecretKey(..)
  , ProxySecretKey
  , AProxySignature(..)
  , ProxySignature
  , pskOmega
  , unsafeProxySecretKey
  , decodeAProxySecretKey
  , isSelfSignedPsk
  )
where

import Cardano.Prelude hiding (show)

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Formatting
  (Format, bprint, build, fitLeft, formatToString, later, sformat, (%.))
import qualified Formatting.Buildable as B
import Prelude (show)
import Text.JSON.Canonical (JSValue(..))
import qualified Text.JSON.Canonical as TJC (FromJSON(..), ToJSON(..))

import Cardano.Binary.Class
  (Annotated(..), Bi(..), ByteSpan, decodeAnnotated, encodeListLen, enforceSize)
import Cardano.Crypto.Hashing (hash)
import Cardano.Crypto.Orphans ()


--------------------------------------------------------------------------------
-- Utilities for From/ToJSON instances
--------------------------------------------------------------------------------

fmsg :: Text -> Either Text a -> Either Text a
fmsg msg = first (("Unable to parse json " <> msg <> " reason: ") <>)

--------------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
--------------------------------------------------------------------------------

-- | Wrapper around 'CC.XPub'.
newtype PublicKey =
  PublicKey CC.XPub
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON PublicKey where
  toJSON = toJSON . sformat fullPublicKeyF

instance FromJSON PublicKey where
  parseJSON v =
    parseJSON v >>= toAesonError . fmsg "PublicKey" . parseFullPublicKey

instance Monad m => TJC.ToJSON m PublicKey where
  toJSON = pure . JSString . formatToString fullPublicKeyF

instance MonadError SchemaError m => TJC.FromJSON m PublicKey where
  fromJSON = parseJSString parseFullPublicKey

encodeXPub :: CC.XPub -> E.Encoding
encodeXPub a = encode $ CC.unXPub a

decodeXPub :: D.Decoder s CC.XPub
decodeXPub = toCborError . CC.xpub =<< decode

instance Bi PublicKey where
  encode (PublicKey a) = encodeXPub a
  decode = fmap PublicKey decodeXPub
  encodedSizeExpr _ _ = 66

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

instance B.Buildable PublicKey where
  build = bprint ("pub:" . shortPublicKeyHexF)

instance B.Buildable SecretKey where
  build = bprint ("sec:" . shortPublicKeyHexF) . toPublic

encodeXPrv :: CC.XPrv -> E.Encoding
encodeXPrv a = encode $ CC.unXPrv a

decodeXPrv :: D.Decoder s CC.XPrv
decodeXPrv = toCborError . CC.xprv =<< decode @ByteString

instance Bi SecretKey where
  encode (SecretKey a) = encodeXPrv a
  decode = fmap SecretKey decodeXPrv

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey (PublicKey pk) =
  Builder.fromString . BS.unpack . B64.encode . CC.unXPub $ pk

-- | Formatter for 'PublicKey' to show it in base64.
fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

-- | Formatter for 'PublicKey' to show it in hex.
fullPublicKeyHexF :: Format r (PublicKey -> r)
fullPublicKeyHexF = later $ \(PublicKey x) -> base16Builder . CC.unXPub $ x

-- | Formatter for 'PublicKey' to show it in hex, but only first 8 chars.
shortPublicKeyHexF :: Format r (PublicKey -> r)
shortPublicKeyHexF = fitLeft 8 %. fullPublicKeyHexF

-- | Parse 'PublicKey' from base64 encoded string.
parseFullPublicKey :: Text -> Either Text PublicKey
parseFullPublicKey s = do
  b <- first toS . B64.decode $ toS s
  PublicKey <$> first toS (CC.xpub b)

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

-- | Wrapper around 'CC.XSignature'.
newtype Signature a =
  Signature CC.XSignature
  deriving (Eq, Ord, Show, Generic, NFData)

instance B.Buildable (Signature a) where
  build _ = "<signature>"

instance FromJSON (Signature w) where
  parseJSON v =
    parseJSON v >>= toAesonError . fmsg "Signature" . parseFullSignature

instance ToJSON (Signature w) where
  toJSON = toJSON . sformat fullSignatureHexF

instance Monad m => TJC.ToJSON m (Signature w) where
  toJSON = pure . JSString . formatToString fullSignatureHexF

instance (Typeable x, MonadError SchemaError m) => TJC.FromJSON m (Signature x) where
  fromJSON = parseJSString parseFullSignature

-- | Formatter for 'Signature' to show it in hex.
fullSignatureHexF :: Format r (Signature a -> r)
fullSignatureHexF =
  later $ \(Signature x) -> base16Builder . CC.unXSignature $ x

-- | Parse 'Signature' from base16 encoded string.
parseFullSignature :: Text -> Either Text (Signature a)
parseFullSignature s = do
  b <- first (sformat build) $ parseBase16 s
  Signature <$> first toS (CC.xsignature b)

encodeXSignature :: CC.XSignature -> E.Encoding
encodeXSignature a = encode $ CC.unXSignature a

decodeXSignature :: D.Decoder s CC.XSignature
decodeXSignature = toCborError . CC.xsignature =<< decode

instance Typeable a => Bi (Signature a) where
  encode (Signature a) = encodeXSignature a
  decode = fmap Signature decodeXSignature

-- | Value and signature for this value.
data Signed a = Signed
  { signedValue :: !a
  -- ^ Value to be signed
  , signedSig   :: !(Signature a)
  -- ^ 'Signature' of 'signedValue'
  } deriving (Show, Eq, Ord, Generic)

instance Bi a => Bi (Signed a) where
  encode (Signed v s) = encodeListLen 2 <> encode v <> encode s
  decode = Signed <$ enforceSize "Signed" 2 <*> decode <*> decode

--------------------------------------------------------------------------------
-- Proxy signing
--------------------------------------------------------------------------------

-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert
  { unProxyCert :: CC.XSignature
  } deriving (Eq, Ord, Show, Generic, NFData)


instance B.Buildable (ProxyCert w) where
  build _ = "<proxy_cert>"

instance ToJSON (ProxyCert w) where
  toJSON = toJSON . sformat fullProxyCertHexF

instance FromJSON (ProxyCert w) where
  parseJSON v =
    parseJSON v >>= toAesonError . fmsg "Signature" . parseFullProxyCert

instance Monad m => TJC.ToJSON m (ProxyCert w) where
  toJSON = pure . JSString . formatToString fullProxyCertHexF

instance (Typeable w, MonadError SchemaError m) => TJC.FromJSON m (ProxyCert w) where
  fromJSON = parseJSString parseFullProxyCert

instance Typeable w => Bi (ProxyCert w) where
  encode (ProxyCert a) = encodeXSignature a
  decode = fmap ProxyCert decodeXSignature

-- | Formatter for 'ProxyCert' to show it in hex.
fullProxyCertHexF :: Format r (ProxyCert a -> r)
fullProxyCertHexF =
  later $ \(ProxyCert x) -> base16Builder . CC.unXSignature $ x

-- | Parse 'ProxyCert' from base16 encoded string.
parseFullProxyCert :: Text -> Either Text (ProxyCert a)
parseFullProxyCert s = do
  b <- first (sformat build) $ parseBase16 s
  ProxyCert <$> first toS (CC.xsignature b)

-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data AProxySecretKey w a = UnsafeAProxySecretKey
  { aPskOmega     :: Annotated w a
  , pskIssuerPk   :: PublicKey
  , pskDelegatePk :: PublicKey
  , pskCert       :: ProxyCert w
  } deriving (Eq, Ord, Show, Generic, Functor)

type ProxySecretKey w = AProxySecretKey w ()

unsafeProxySecretKey
  :: w -> PublicKey -> PublicKey -> ProxyCert w -> ProxySecretKey w
unsafeProxySecretKey w = UnsafeAProxySecretKey (Annotated w ())

pskOmega :: AProxySecretKey w a -> w
pskOmega = unAnnotated . aPskOmega

instance (NFData w, NFData a) => NFData (AProxySecretKey w a)

instance B.Buildable w => B.Buildable (AProxySecretKey w a) where
  build (UnsafeAProxySecretKey w iPk dPk _) = bprint
    ("ProxySk { w = " . build . ", iPk = " . build . ", dPk = " . build . " }")
    (unAnnotated w)
    iPk
    dPk

instance Bi w => Bi (ProxySecretKey w) where
  encode psk =
    encodeListLen 4
      <> encode (pskOmega psk)
      <> encode (pskIssuerPk psk)
      <> encode (pskDelegatePk psk)
      <> encode (pskCert psk)

  decode =  void <$> decodeAProxySecretKey

decodeAProxySecretKey :: Bi w => D.Decoder s (AProxySecretKey w ByteSpan)
decodeAProxySecretKey = do
  enforceSize "ProxySecretKey" 4
  UnsafeAProxySecretKey <$> decodeAnnotated <*> decode <*> decode <*> decode

-- | Delegate signature made with certificate-based permission. @w@
-- stays for message type used in proxy (ω in the implementation
-- notes), @s@ for type of message signed.
--
-- We add whole psk as a field because otherwise we can't verify sig
-- in heavyweight psk transitive delegation: i → x → d, we have psk
-- from x to d, slot leader is i.
type ProxySignature w s = AProxySignature w s ()

data AProxySignature w s a = AProxySignature
  { psigPsk :: AProxySecretKey w a
  , psigSig :: CC.XSignature
  } deriving (Eq, Ord, Show, Generic, Functor)


instance (NFData w, NFData a) => NFData (AProxySignature w s a)

instance B.Buildable w => B.Buildable (AProxySignature w s a) where
  build psig = bprint ("Proxy signature { psk = " . build . " }") (psigPsk psig)

instance (Typeable s, Bi w) => Bi (ProxySignature w s) where
  encode psig =
    encodeListLen 2 <> encode (psigPsk psig) <> encodeXSignature (psigSig psig)

  decode = void <$> decodeAProxySignature


decodeAProxySignature :: Bi w => D.Decoder s (AProxySignature w a ByteSpan)
decodeAProxySignature =
  AProxySignature
    <$  enforceSize "ProxySignature" 2
    <*> decodeAProxySecretKey
    <*> decodeXSignature


-- | Checks if delegate and issuer fields of proxy secret key are
-- equal.
isSelfSignedPsk :: ProxySecretKey w -> Bool
isSelfSignedPsk psk = pskIssuerPk psk == pskDelegatePk psk

-- These are *not* orphan instances, these types are defined in this file.
-- However these need to be defined here to avoid TemplateHaskell compile
-- phase errors.

instance ToJSON w => ToJSON (AProxySecretKey w ()) where
  toJSON psk = object
    [ "pskOmega" .= toJSON (pskOmega psk)
    , "pskIssuerPk" .= toJSON (pskIssuerPk psk)
    , "pskDelegatePk" .= toJSON (pskDelegatePk psk)
    , "pskCert" .= toJSON (pskCert psk)
    ]

instance FromJSON w => FromJSON (AProxySecretKey w ()) where
    parseJSON = withObject "ProxySecretKey" $ \v -> unsafeProxySecretKey
        <$> v .: "pskOmega"
        <*> v .: "pskIssuerPk"
        <*> v .: "pskDelegatePk"
        <*> v .: "pskCert"
