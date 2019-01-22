{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Block.ExtraHeaderData
  ( ExtraHeaderData(..)
  , ExtraHeaderDataError(..)
  , verifyExtraHeaderData
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import Formatting (bprint, build, builder)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Block.ExtraBodyData (ExtraBodyData)
import Cardano.Chain.Common (Attributes, areAttributesKnown)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion
  (SoftwareVersion, SoftwareVersionError, checkSoftwareVersion)
import Cardano.Crypto (Hash)


-- | Represents block header extra data
data ExtraHeaderData = ExtraHeaderData
  { ehdProtocolVersion :: !ProtocolVersion
  -- ^ Protocol version used by this block
  , ehdSoftwareVersion :: !SoftwareVersion
  -- ^ Software version used by this block
  , ehdAttributes      :: !(Attributes ())
  -- ^ Header attributes
  , ehdEBDataProof     :: !(Hash ExtraBodyData)
  -- ^ Extra body data Hash
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable ExtraHeaderData where
  build mehd = bprint
    ("    block: v" . build . "\n" . "    software: " . build . "\n" . builder)
    (ehdProtocolVersion mehd)
    (ehdSoftwareVersion mehd)
    formattedExtra
   where
    formattedExtra
      | areAttributesKnown (ehdAttributes mehd) = mempty
      | otherwise = bprint
        ("    attributes: " . build . "\n")
        (ehdAttributes mehd)

instance Bi ExtraHeaderData where
  encode ehd =
    encodeListLen 4
      <> encode (ehdProtocolVersion ehd)
      <> encode (ehdSoftwareVersion ehd)
      <> encode (ehdAttributes ehd)
      <> encode (ehdEBDataProof ehd)

  decode = do
    enforceSize "ExtraHeaderData" 4
    ExtraHeaderData <$> decode <*> decode <*> decode <*> decode

data ExtraHeaderDataError =
  ExtraHeaderDataSoftwareVersionError SoftwareVersionError
  deriving (Eq, Show)

instance B.Buildable ExtraHeaderDataError where
  build = \case
    ExtraHeaderDataSoftwareVersionError err -> bprint
      ( "SoftwareVersion was invalid while checking ExtraHeaderData.\n Error: "
      . build
      )
      err

verifyExtraHeaderData
  :: MonadError ExtraHeaderDataError m => ExtraHeaderData -> m ()
verifyExtraHeaderData ehd =
  liftEither . first ExtraHeaderDataSoftwareVersionError $ checkSoftwareVersion
    (ehdSoftwareVersion ehd)
