{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Update.ApplicationName (
  ApplicationName (..),
  applicationNameMaxLength,
  ApplicationNameError (..),
  checkApplicationName,
)
where

import Cardano.Ledger.Binary (
  Case (..),
  DecCBOR (..),
  Decoder,
  DecoderError (..),
  EncCBOR (..),
  cborError,
  decodeListLen,
  decodeWord8,
  encodeListLen,
  matchSize,
  szCases,
 )
import Cardano.Prelude hiding (cborError)
import Data.Aeson (ToJSON)
import Data.Data (Data)
import qualified Data.Text as T
import Formatting (bprint, int, stext)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

newtype ApplicationName = ApplicationName
  { unApplicationName :: Text
  }
  deriving (Eq, Ord, Show, Generic, B.Buildable, NFData, NoThunks)

instance EncCBOR ApplicationName where
  encCBOR appName = encCBOR (unApplicationName appName)
  encodedSizeExpr _ _ =
    1
      + szCases
        [ Case "minBound" 0
        , Case "maxBound" (fromInteger applicationNameMaxLength)
        ]

instance DecCBOR ApplicationName where
  decCBOR = ApplicationName <$> decCBOR

data ApplicationNameError
  = ApplicationNameTooLong Text
  | ApplicationNameNotAscii Text
  deriving (Data, Eq, Show)

-- Used for debugging purposes only
instance ToJSON ApplicationName

instance EncCBOR ApplicationNameError where
  encCBOR err = case err of
    ApplicationNameTooLong appName ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR appName
    ApplicationNameNotAscii appName ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR appName

instance DecCBOR ApplicationNameError where
  decCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "ApplicationNameError" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> ApplicationNameTooLong <$> decCBOR
      1 -> checkSize 2 >> ApplicationNameNotAscii <$> decCBOR
      _ -> cborError $ DecoderErrorUnknownTag "ApplicationNameError" tag

instance B.Buildable ApplicationNameError where
  build = \case
    ApplicationNameTooLong name ->
      bprint
        ("ApplicationName, " . stext . ", exceeds limit of " . int)
        name
        (applicationNameMaxLength :: Int)
    ApplicationNameNotAscii name ->
      bprint
        ("ApplicationName, " . stext . ", contains non-ascii characters")
        name

-- | Smart constructor of 'ApplicationName'
checkApplicationName ::
  MonadError ApplicationNameError m => ApplicationName -> m ()
checkApplicationName (ApplicationName appName)
  | T.length appName > applicationNameMaxLength =
      throwError $
        ApplicationNameTooLong appName
  | T.any (not . isAscii) appName = throwError $ ApplicationNameNotAscii appName
  | otherwise = pure ()

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 12
