{-# LANGUAGE DeriveGeneric #-}

module LogResults (
  Option (..),
  Failure (..),
  LogResults,
) where

import Data.Aeson
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data Option = Option
  { optionName :: !Text
  , optionValue :: !Text
  }
  deriving (Eq, Ord, Show, Generic)

data Failure = Failure
  { failureSelector :: !Option
  , failureSeed :: !Option
  }
  deriving (Eq, Ord, Show, Generic)

type LogResults = Map Text [Failure]

instance ToJSON Option where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "option"}

instance ToJSON Failure where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "failure"}

instance FromJSON Option where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "option"}

instance FromJSON Failure where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "failure"}

relabel :: String -> String -> String
relabel p f = maybe f (camelTo2 '_') $ stripPrefix p f
