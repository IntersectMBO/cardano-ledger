{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Update.ApplicationName
  ( ApplicationName (..),
    applicationNameMaxLength,
    ApplicationNameError (..),
    checkApplicationName,
  )
where

import Cardano.Binary
  ( Case (..),
    Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    matchSize,
    szCases,
  )
import Cardano.Prelude
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

instance ToCBOR ApplicationName where
  toCBOR appName = toCBOR (unApplicationName appName)
  encodedSizeExpr _ _ =
    1
      + szCases
        [ Case "minBound" 0,
          Case "maxBound" (fromInteger applicationNameMaxLength)
        ]

instance FromCBOR ApplicationName where
  fromCBOR = ApplicationName <$> fromCBOR

data ApplicationNameError
  = ApplicationNameTooLong Text
  | ApplicationNameNotAscii Text
  deriving (Data, Eq, Show)

-- Used for debugging purposes only
instance ToJSON ApplicationName

instance ToCBOR ApplicationNameError where
  toCBOR err = case err of
    ApplicationNameTooLong appName ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR appName
    ApplicationNameNotAscii appName ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR appName

instance FromCBOR ApplicationNameError where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "ApplicationNameError" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> ApplicationNameTooLong <$> fromCBOR
      1 -> checkSize 2 >> ApplicationNameNotAscii <$> fromCBOR
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
