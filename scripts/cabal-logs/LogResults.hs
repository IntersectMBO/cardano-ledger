{-# LANGUAGE DeriveGeneric #-}

module LogResults (
  Option (..),
  Failure (..),
  SuiteRun (..),
  LogResults (..),
) where

import Data.Aeson
import Data.List (stripPrefix)
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

data SuiteRun = SuiteRun
  { suiteName :: !Text
  , suiteFailures :: ![Failure]
  }
  deriving (Eq, Ord, Show, Generic)

data LogResults = LogResults
  { logCompilerVersion :: ![Int]
  , logSuiteRuns :: ![SuiteRun]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Option where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "option"}

instance ToJSON Failure where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "failure"}

instance ToJSON SuiteRun where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "suite"}

instance ToJSON LogResults where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "log"}

instance FromJSON Option where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "option"}

instance FromJSON Failure where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "failure"}

instance FromJSON SuiteRun where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "suite"}

instance FromJSON LogResults where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "log"}

relabel :: String -> String -> String
relabel p f = maybe f (camelTo2 '_') $ stripPrefix p f
