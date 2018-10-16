{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cardano.Chain.Update.SoftwareVersion
       ( SoftwareVersion (..)
       , NumSoftwareVersion
       , checkSoftwareVersion
       ) where

import           Cardano.Prelude
import qualified Prelude

import           Control.Monad.Except (MonadError)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting (bprint, build, formatToString, int, stext, (%))
import qualified Formatting.Buildable as B (Buildable (..))

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Update.ApplicationName


-- | Numeric software version associated with 'ApplicationName'
type NumSoftwareVersion = Word32

-- | Software version
data SoftwareVersion = SoftwareVersion
  { svAppName :: !ApplicationName
  , svNumber  :: !NumSoftwareVersion
  } deriving (Eq, Generic, Ord, Typeable)
    deriving anyclass NFData

instance B.Buildable SoftwareVersion where
  build sv =
    bprint (stext % ":" % int) (getApplicationName $ svAppName sv) (svNumber sv)

instance Show SoftwareVersion where
  show = formatToString build

instance Bi SoftwareVersion where
  encode sv = encodeListLen 2 <> encode (svAppName sv) <> encode (svNumber sv)

  decode = do
    enforceSize "SoftwareVersion" 2
    SoftwareVersion <$> decode <*> decode

-- | A software version is valid iff its application name is valid
checkSoftwareVersion :: MonadError Text m => SoftwareVersion -> m ()
checkSoftwareVersion sv = checkApplicationName (svAppName sv)

deriveJSON defaultOptions ''SoftwareVersion
