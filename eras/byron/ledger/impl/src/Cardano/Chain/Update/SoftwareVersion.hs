{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Update.SoftwareVersion
  ( SoftwareVersion (..),
    SoftwareVersionError (..),
    NumSoftwareVersion,
    checkSoftwareVersion,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeWord8,
    encodeListLen,
    enforceSize,
  )
import Cardano.Chain.Update.ApplicationName
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Data.Data (Data)
import Formatting (bprint, build, formatToString, int, stext)
import qualified Formatting.Buildable as B (Buildable (..))
import NoThunks.Class (NoThunks (..))
import qualified Prelude

-- | Numeric software version associated with 'ApplicationName'
type NumSoftwareVersion = Word32

-- | Software version
data SoftwareVersion = SoftwareVersion
  { svAppName :: !ApplicationName,
    svNumber :: !NumSoftwareVersion
  }
  deriving (Eq, Generic, Ord)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable SoftwareVersion where
  build sv =
    bprint (stext . ":" . int) (unApplicationName $ svAppName sv) (svNumber sv)

instance Show SoftwareVersion where
  show = formatToString build

-- Used for debugging purposes only
instance ToJSON SoftwareVersion

instance ToCBOR SoftwareVersion where
  toCBOR sv = encodeListLen 2 <> toCBOR (svAppName sv) <> toCBOR (svNumber sv)

  encodedSizeExpr f sv =
    1
      + encodedSizeExpr f (svAppName <$> sv)
      + encodedSizeExpr f (svNumber <$> sv)

instance FromCBOR SoftwareVersion where
  fromCBOR = do
    enforceSize "SoftwareVersion" 2
    SoftwareVersion <$> fromCBOR <*> fromCBOR

data SoftwareVersionError
  = SoftwareVersionApplicationNameError ApplicationNameError
  deriving (Data, Eq, Show)

instance ToCBOR SoftwareVersionError where
  toCBOR (SoftwareVersionApplicationNameError applicationNameError) =
    encodeListLen 2
      <> toCBOR (0 :: Word8)
      <> toCBOR applicationNameError

instance FromCBOR SoftwareVersionError where
  fromCBOR = do
    enforceSize "SoftwareVersionError" 2
    tag <- decodeWord8
    case tag of
      0 -> SoftwareVersionApplicationNameError <$> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "SoftwareVersionError" tag

instance B.Buildable SoftwareVersionError where
  build = \case
    SoftwareVersionApplicationNameError err ->
      bprint
        ( "ApplicationName was invalid when checking SoftwareVersion\n Error:"
            . build
        )
        err

-- | A software version is valid iff its application name is valid
checkSoftwareVersion ::
  MonadError SoftwareVersionError m => SoftwareVersion -> m ()
checkSoftwareVersion sv =
  checkApplicationName (svAppName sv)
    `wrapError` SoftwareVersionApplicationNameError
