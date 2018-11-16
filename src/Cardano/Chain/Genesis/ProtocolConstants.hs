{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Genesis.ProtocolConstants
  ( GenesisProtocolConstants(..)
  )
where

import Cardano.Prelude

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Text.JSON.Canonical
  (FromJSON(..), Int54, JSValue(..), ToJSON(..), fromJSField, mkObject)


import Cardano.Chain.ProtocolConstants (VssMaxTTL(..), VssMinTTL(..))
import Cardano.Crypto (ProtocolMagic(..))


-- | 'GensisProtocolConstants' are not really part of genesis global state,
-- but they affect consensus, so they are part of 'GenesisSpec' and
-- 'GenesisData'.
data GenesisProtocolConstants = GenesisProtocolConstants
  { -- | Security parameter from the paper.
    gpcK             :: !Int
    -- | Magic constant for separating real/testnet.
  , gpcProtocolMagic :: !ProtocolMagic
    -- | VSS certificates max timeout to live (number of epochs).
  , gpcVssMaxTTL     :: !VssMaxTTL
    -- | VSS certificates min timeout to live (number of epochs).
  , gpcVssMinTTL     :: !VssMinTTL
  } deriving (Show, Eq, Generic)

instance Monad m => ToJSON m GenesisProtocolConstants where
  toJSON gpc = mkObject
  -- 'k' definitely won't exceed the limit
    [ ("k"            , pure . JSNum . fromIntegral $ gpcK gpc)
    , ("protocolMagic", toJSON . getProtocolMagic $ gpcProtocolMagic gpc)
    , ("vssMaxTTL"    , toJSON $ gpcVssMaxTTL gpc)
    , ("vssMinTTL"    , toJSON $ gpcVssMinTTL gpc)
    ]

instance MonadError SchemaError m => FromJSON m GenesisProtocolConstants where
  fromJSON obj =
    GenesisProtocolConstants
      <$> (fromIntegral @Int54 <$> fromJSField obj "k")
      <*> (ProtocolMagic <$> fromJSField obj "protocolMagic")
      <*> fromJSField obj "vssMaxTTL"
      <*> fromJSField obj "vssMinTTL"

deriveJSON defaultOptions ''GenesisProtocolConstants
