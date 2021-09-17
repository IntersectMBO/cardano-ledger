{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Common.NetworkMagic
  ( NetworkMagic (..),
    makeNetworkMagic,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    matchSize,
  )
import Cardano.Crypto.ProtocolMagic
  ( AProtocolMagic (..),
    RequiresNetworkMagic (..),
    getProtocolMagic,
  )
import Cardano.Prelude hiding ((%))
import Data.Aeson (ToJSON)
import Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

data NetworkMagic
  = NetworkMainOrStage
  | NetworkTestnet {-# UNPACK #-} !Word32
  deriving (Show, Eq, Ord, Generic, NFData, NoThunks)

instance B.Buildable NetworkMagic where
  build NetworkMainOrStage = "NetworkMainOrStage"
  build (NetworkTestnet n) = bprint ("NetworkTestnet (" % build % ")") n

-- Used for debugging purposes only
instance ToJSON NetworkMagic

instance HeapWords NetworkMagic where
  heapWords NetworkMainOrStage = 0
  heapWords (NetworkTestnet _) = 2

instance ToCBOR NetworkMagic where
  toCBOR = \case
    NetworkMainOrStage ->
      encodeListLen 1 <> toCBOR @Word8 0
    NetworkTestnet n ->
      encodeListLen 2 <> toCBOR @Word8 1 <> toCBOR n

instance FromCBOR NetworkMagic where
  fromCBOR = do
    len <- decodeListLen
    tag <- decodeWord8
    case tag of
      0 -> matchSize "NetworkMagic" 1 len $> NetworkMainOrStage
      1 -> matchSize "NetworkMagic" 2 len >> NetworkTestnet <$> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "NetworkMagic" tag

makeNetworkMagic :: AProtocolMagic a -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
  RequiresNoMagic -> NetworkMainOrStage
  RequiresMagic -> NetworkTestnet (getProtocolMagic pm)
