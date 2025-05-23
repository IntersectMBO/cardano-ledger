{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Crypto.Signing.Safe.PassPhrase (
  PassPhrase (..),
  emptyPassphrase,
  passphraseLength,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  fromByronCBOR,
  toByronCBOR,
  toCborError,
 )
import Cardano.Prelude hiding (toCborError)
import Data.ByteArray (ByteArray, ByteArrayAccess, ScrubbedBytes)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import Data.Default (Default (..))
import Formatting (int, sformat)
import Formatting.Buildable (Buildable (..))
import qualified Prelude

type PassPhrase :: Type
newtype PassPhrase
  = PassPhrase ScrubbedBytes
  deriving (Eq, Ord, Semigroup, Monoid, NFData, ByteArray, ByteArrayAccess)

passphraseLength :: Int
passphraseLength = 32

-- | Empty passphrase used in development
emptyPassphrase :: PassPhrase
emptyPassphrase = PassPhrase mempty

instance Show PassPhrase where
  show _ = "<passphrase>"

instance Buildable PassPhrase where
  build _ = "<passphrase>"

instance Default PassPhrase where
  def = emptyPassphrase

instance ToCBOR PassPhrase where
  toCBOR = toByronCBOR

instance FromCBOR PassPhrase where
  fromCBOR = fromByronCBOR

instance EncCBOR PassPhrase where
  encCBOR pp = encCBOR (ByteArray.convert pp :: ByteString)

instance DecCBOR PassPhrase where
  decCBOR = do
    bs <- decCBOR @ByteString
    let bl = BS.length bs
    -- Currently passphrase may be either 32-byte long or empty (for
    -- unencrypted keys).
    toCborError
      $ if bl == 0 || bl == passphraseLength
        then Right $ ByteArray.convert bs
        else
          Left
            $ sformat
              ("put@PassPhrase: expected length 0 or " . int . ", not " . int)
              passphraseLength
              bl

{-instance Monoid PassPhrase where
    mempty = PassPhrase mempty
    mappend (PassPhrase p1) (PassPhrase p2) = PassPhrase (p1 `mappend` p2)-}
