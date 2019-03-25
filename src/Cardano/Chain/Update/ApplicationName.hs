{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Chain.Update.ApplicationName
  ( ApplicationName(..)
  , applicationNameMaxLength
  , ApplicationNameError(..)
  , checkApplicationName
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson (FromJSON(..))
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Char (isAscii)
import Data.Data (Data)
import qualified Data.Text as T
import Formatting (bprint, int, stext)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..))


newtype ApplicationName = ApplicationName
  { getApplicationName :: Text
  } deriving (Eq, Ord, Show, Generic, B.Buildable, NFData)

instance Bi ApplicationName where
  encode appName = encode (getApplicationName appName)
  decode = ApplicationName <$> decode

instance FromJSON ApplicationName where
  -- FIXME does the defaultOptions derived JSON encode directly as text? Or
  -- as an object with a single key?
  parseJSON v = ApplicationName <$> parseJSON v

data ApplicationNameError
  = ApplicationNameTooLong Text
  | ApplicationNameNotAscii Text
  deriving (Data, Eq, Show)

instance B.Buildable ApplicationNameError where
  build = \case
    ApplicationNameTooLong name -> bprint
      ("ApplicationName, " . stext . ", exceeds limit of " . int)
      name
      (applicationNameMaxLength :: Int)
    ApplicationNameNotAscii name -> bprint
      ("ApplicationName, " . stext . ", contains non-ascii characters")
      name

-- | Smart constructor of 'ApplicationName'
checkApplicationName
  :: MonadError ApplicationNameError m => ApplicationName -> m ()
checkApplicationName (ApplicationName appName)
  | T.length appName > applicationNameMaxLength = throwError
  $ ApplicationNameTooLong appName
  | T.any (not . isAscii) appName = throwError $ ApplicationNameNotAscii appName
  | otherwise = pure ()

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 12

deriveToJSON defaultOptions ''ApplicationName
