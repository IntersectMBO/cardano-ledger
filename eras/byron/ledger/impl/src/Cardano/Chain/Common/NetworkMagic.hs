{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Common.NetworkMagic (
  NetworkMagic (..),
  makeNetworkMagic,
)
where

import Cardano.Crypto.ProtocolMagic (
  AProtocolMagic (..),
  RequiresNetworkMagic (..),
  getProtocolMagic,
 )
import Cardano.HeapWords (HeapWords (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecoderError (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  cborError,
  decodeListLen,
  decodeWord8,
  encodeListLen,
  fromByronCBOR,
  matchSize,
  toByronCBOR,
 )
import Cardano.Prelude hiding (cborError, (%))
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
  toCBOR = toByronCBOR

instance FromCBOR NetworkMagic where
  fromCBOR = fromByronCBOR

instance EncCBOR NetworkMagic where
  encCBOR = \case
    NetworkMainOrStage ->
      encodeListLen 1 <> encCBOR @Word8 0
    NetworkTestnet n ->
      encodeListLen 2 <> encCBOR @Word8 1 <> encCBOR n

instance DecCBOR NetworkMagic where
  decCBOR = do
    len <- decodeListLen
    tag <- decodeWord8
    case tag of
      0 -> matchSize "NetworkMagic" 1 len $> NetworkMainOrStage
      1 -> matchSize "NetworkMagic" 2 len >> NetworkTestnet <$> decCBOR
      _ -> cborError $ DecoderErrorUnknownTag "NetworkMagic" tag

makeNetworkMagic :: AProtocolMagic a -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
  RequiresNoMagic -> NetworkMainOrStage
  RequiresMagic -> NetworkTestnet (getProtocolMagic pm)
