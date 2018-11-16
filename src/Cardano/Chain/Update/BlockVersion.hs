{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cardano.Chain.Update.BlockVersion
  ( BlockVersion(..)
  )
where

import Cardano.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Formatting (bprint, shown)
import Formatting.Buildable (Buildable(..))
import qualified Prelude

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)


-- | Communication protocol version
data BlockVersion = BlockVersion
  { bvMajor :: !Word16
  , bvMinor :: !Word16
  , bvAlt   :: !Word8
  } deriving (Eq, Generic, Ord)
    deriving anyclass NFData

instance Show BlockVersion where
  show bv =
    intercalate "." [show (bvMajor bv), show (bvMinor bv), show (bvAlt bv)]

instance Buildable BlockVersion where
  build = bprint shown

instance Bi BlockVersion where
  encode bv =
    encodeListLen 3 <> encode (bvMajor bv) <> encode (bvMinor bv) <> encode
      (bvAlt bv)

  decode = do
    enforceSize "BlockVersion" 3
    BlockVersion <$> decode <*> decode <*> decode

deriveJSON defaultOptions ''BlockVersion
