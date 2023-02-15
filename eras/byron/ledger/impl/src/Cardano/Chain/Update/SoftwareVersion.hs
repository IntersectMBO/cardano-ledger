{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.SoftwareVersion (
  SoftwareVersion (..),
  SoftwareVersionError (..),
  NumSoftwareVersion,
  checkSoftwareVersion,
)
where

import Cardano.Chain.Update.ApplicationName
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecoderError (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  cborError,
  decodeWord8,
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude hiding (cborError)
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
  { svAppName :: !ApplicationName
  , svNumber :: !NumSoftwareVersion
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
  toCBOR = toByronCBOR

instance FromCBOR SoftwareVersion where
  fromCBOR = fromByronCBOR

instance EncCBOR SoftwareVersion where
  encCBOR sv = encodeListLen 2 <> encCBOR (svAppName sv) <> encCBOR (svNumber sv)

  encodedSizeExpr f sv =
    1
      + encodedSizeExpr f (svAppName <$> sv)
      + encodedSizeExpr f (svNumber <$> sv)

instance DecCBOR SoftwareVersion where
  decCBOR = do
    enforceSize "SoftwareVersion" 2
    SoftwareVersion <$> decCBOR <*> decCBOR

data SoftwareVersionError
  = SoftwareVersionApplicationNameError ApplicationNameError
  deriving (Data, Eq, Show)

instance ToCBOR SoftwareVersionError where
  toCBOR = toByronCBOR

instance FromCBOR SoftwareVersionError where
  fromCBOR = fromByronCBOR

instance EncCBOR SoftwareVersionError where
  encCBOR (SoftwareVersionApplicationNameError applicationNameError) =
    encodeListLen 2
      <> encCBOR (0 :: Word8)
      <> encCBOR applicationNameError

instance DecCBOR SoftwareVersionError where
  decCBOR = do
    enforceSize "SoftwareVersionError" 2
    tag <- decodeWord8
    case tag of
      0 -> SoftwareVersionApplicationNameError <$> decCBOR
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
