{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Update.SystemTag
  ( SystemTag(..)
  , SystemTagError(..)
  , checkSystemTag
  , systemTagMaxLength
  , osHelper
  , archHelper
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson (FromJSON(..))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveToJSON)
import Data.Char (isAscii)
import Data.Data (Data)
import qualified Data.Text as T
import Distribution.System (Arch(..), OS(..))
import Distribution.Text (display)
import Formatting (bprint, int, stext)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..))


-- | Tag of system for which update data is purposed, e.g. win64, mac32
newtype SystemTag = SystemTag
  { getSystemTag :: Text
  } deriving (Eq, Ord, Show, Generic, B.Buildable)

instance NFData SystemTag

instance FromJSON SystemTag where
  parseJSON v = SystemTag <$> parseJSON v

deriveToJSON defaultOptions ''SystemTag

instance Bi SystemTag where
  encode = encode . getSystemTag
  decode = SystemTag <$> decode

systemTagMaxLength :: Integral i => i
systemTagMaxLength = 10

data SystemTagError
  = SystemTagNotAscii Text
  | SystemTagTooLong Text
  deriving Data

instance B.Buildable SystemTagError where
  build = \case
    SystemTagNotAscii tag ->
      bprint ("SystemTag, " . stext . ", contains non-ascii characters") tag
    SystemTagTooLong tag -> bprint
      ("SystemTag, " . stext . ", exceeds limit of " . int)
      tag
      (systemTagMaxLength :: Int)

checkSystemTag :: MonadError SystemTagError m => SystemTag -> m ()
checkSystemTag (SystemTag tag)
  | T.length tag > systemTagMaxLength = throwError $ SystemTagTooLong tag
  | T.any (not . isAscii) tag = throwError $ SystemTagNotAscii tag
  | otherwise = pure ()

-- | Helper to turn an @OS@ into a @Text@ compatible with the @systemTag@
--   previously used in 'configuration.yaml'
osHelper :: OS -> Text
osHelper sys = case sys of
  Windows -> "win"
  OSX     -> "macos"
  Linux   -> "linux"
  _       -> toS $ display sys

-- | Helper to turn an @Arch@ into a @Text@ compatible with the @systemTag@
--   previously used in 'configuration.yaml'
archHelper :: Arch -> Text
archHelper archt = case archt of
  I386   -> "32"
  X86_64 -> "64"
  _      -> toS $ display archt
