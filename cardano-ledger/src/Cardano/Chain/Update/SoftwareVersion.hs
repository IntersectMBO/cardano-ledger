{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cardano.Chain.Update.SoftwareVersion
  ( SoftwareVersion(..)
  , SoftwareVersionError(..)
  , NumSoftwareVersion
  , checkSoftwareVersion
  )
where

import Cardano.Prelude
import qualified Prelude

import Control.Monad.Except (MonadError)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data)
import Formatting (bprint, build, formatToString, int, stext)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import Cardano.Chain.Update.ApplicationName


-- | Numeric software version associated with 'ApplicationName'
type NumSoftwareVersion = Word32

-- | Software version
data SoftwareVersion = SoftwareVersion
  { svAppName :: !ApplicationName
  , svNumber  :: !NumSoftwareVersion
  } deriving (Eq, Generic, Ord)
    deriving anyclass NFData

instance B.Buildable SoftwareVersion where
  build sv =
    bprint (stext . ":" . int) (unApplicationName $ svAppName sv) (svNumber sv)

instance Show SoftwareVersion where
  show = formatToString build

instance ToCBOR SoftwareVersion where
  toCBOR sv = encodeListLen 2 <> toCBOR (svAppName sv) <> toCBOR (svNumber sv)

instance FromCBOR SoftwareVersion where
  fromCBOR = do
    enforceSize "SoftwareVersion" 2
    SoftwareVersion <$> fromCBOR <*> fromCBOR

data SoftwareVersionError =
  SoftwareVersionApplicationNameError ApplicationNameError
  deriving (Data, Eq, Show)

instance B.Buildable SoftwareVersionError where
  build = \case
    SoftwareVersionApplicationNameError err -> bprint
      ( "ApplicationName was invalid when checking SoftwareVersion\n Error:"
      . build
      )
      err

-- | A software version is valid iff its application name is valid
checkSoftwareVersion
  :: MonadError SoftwareVersionError m => SoftwareVersion -> m ()
checkSoftwareVersion sv =
  checkApplicationName (svAppName sv)
    `wrapError` SoftwareVersionApplicationNameError

deriveJSON defaultOptions ''SoftwareVersion
