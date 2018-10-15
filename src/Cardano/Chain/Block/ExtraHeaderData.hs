{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Block.ExtraHeaderData
       ( ExtraHeaderData (..)
       , verifyExtraHeaderData
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import           Formatting (bprint, build, builder, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Block.ExtraBodyData (ExtraBodyData)
import           Cardano.Chain.Common (Attributes, areAttributesKnown)
import           Cardano.Chain.Update.BlockVersion (BlockVersion)
import           Cardano.Chain.Update.SoftwareVersion (SoftwareVersion,
                     checkSoftwareVersion)
import           Cardano.Crypto (Hash)


-- | Represents block header extra data
data ExtraHeaderData = ExtraHeaderData
  { ehdBlockVersion    :: !BlockVersion
  -- ^ Version of block
  , ehdSoftwareVersion :: !SoftwareVersion
  -- ^ Software version
  , ehdAttributes      :: !(Attributes ())
  -- ^ Header attributes
  , ehdEBDataProof     :: !(Hash ExtraBodyData)
  -- ^ Extra body data Hash
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable ExtraHeaderData where
  build mehd = bprint
    ("    block: v" % build % "\n" % "    software: " % build % "\n" % builder)
    (ehdBlockVersion mehd)
    (ehdSoftwareVersion mehd)
    formattedExtra
   where
    formattedExtra
      | areAttributesKnown (ehdAttributes mehd) = mempty
      | otherwise = bprint
        ("    attributes: " % build % "\n")
        (ehdAttributes mehd)

instance Bi ExtraHeaderData where
  encode ehd =
    encodeListLen 4
      <> encode (ehdBlockVersion ehd)
      <> encode (ehdSoftwareVersion ehd)
      <> encode (ehdAttributes ehd)
      <> encode (ehdEBDataProof ehd)

  decode = do
    enforceSize "ExtraHeaderData" 4
    ExtraHeaderData <$> decode <*> decode <*> decode <*> decode

verifyExtraHeaderData :: MonadError Text m => ExtraHeaderData -> m ()
verifyExtraHeaderData ehd = checkSoftwareVersion (ehdSoftwareVersion ehd)
