{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Keys.WitVKey (
  WitVKey (WitVKey),
  witVKeyHash,
) where

import Cardano.Crypto.DSIGN.Class (
  DSIGNAlgorithm (
    rawDeserialiseSigDSIGN,
    rawDeserialiseVerKeyDSIGN,
    rawSerialiseSigDSIGN,
    rawSerialiseVerKeyDSIGN
  ),
  SignedDSIGN (..),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Binary.Crypto (decodeSignedDSIGN)
import Cardano.Ledger.Hashes (
  EraIndependentTxBody,
  HASH,
  Hash,
  KeyHash (..),
  hashKey,
  hashTxBodySignature,
 )
import Cardano.Ledger.Keys.Internal (
  DSIGN,
  KeyRole (..),
  VKey (..),
  asWitness,
 )
import Control.DeepSeq
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base16 as B16
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey kr = WitVKeyInternal
  { wvkKey :: !(VKey kr)
  , wvkSignature :: !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
  , wvkKeyHash :: KeyHash Witness
  -- ^ Hash of the witness vkey. We store this here to avoid repeated hashing
  --   when used in ordering.
  }
  deriving (Generic, Show, Eq)

deriving via
  AllowThunksIn '["wvkKeyHash"] (WitVKey kr)
  instance
    Typeable kr => NoThunks (WitVKey kr)

instance NFData (WitVKey kr) where
  rnf WitVKeyInternal {wvkKeyHash} = wvkKeyHash `seq` ()

instance Typeable kr => Ord (WitVKey kr) where
  compare x y =
    -- It is advised against comparison on keys and signatures directly,
    -- therefore we use hashes of verification keys and signatures for
    -- implementing this Ord instance. Note that we do not need to memoize the
    -- hash of a signature, like it is done with the hash of a key, because Ord
    -- instance is only used for Sets of WitVKeys and it would be a mistake to
    -- have two WitVKeys in a same Set for different transactions. Therefore
    -- comparison on signatures is unlikely to happen and is only needed for
    -- compliance with Ord laws.
    comparing wvkKeyHash x y <> comparing (hashTxBodySignature . wvkSignature) x y

instance ToJSON (WitVKey kr) where
  toJSON (WitVKey (VKey vk) (SignedDSIGN sig)) =
    Aeson.object
      [ "key" .= Text.decodeUtf8 (B16.encode (rawSerialiseVerKeyDSIGN vk))
      , "signature" .= Text.decodeUtf8 (B16.encode (rawSerialiseSigDSIGN sig))
      ]

instance Typeable kr => FromJSON (WitVKey kr) where
  parseJSON = Aeson.withObject "WitVKey" $ \o -> do
    !keyHex <- o .: "key" :: Parser Text
    !sigHex <- o .: "signature" :: Parser Text
    !keyBytes <- either fail pure $ B16.decode (Text.encodeUtf8 keyHex)
    !sigBytes <- either fail pure $ B16.decode (Text.encodeUtf8 sigHex)
    !vk <- maybe (fail "WitVKey: invalid key bytes") pure (rawDeserialiseVerKeyDSIGN keyBytes)
    !sig <- maybe (fail "WitVKey: invalid signature bytes") pure (rawDeserialiseSigDSIGN sigBytes)
    pure $ WitVKey (VKey vk) (SignedDSIGN sig)

instance EncCBOR (WitVKey kr) where
  encCBOR (WitVKey k sig) =
    encodeListLen 2
      <> encCBOR k
      <> encCBOR sig

instance Typeable kr => DecCBOR (WitVKey kr) where
  decCBOR =
    decodeRecordNamed "WitVKey" (const 2) $
      WitVKey <$> decCBOR <*> decodeSignedDSIGN
  {-# INLINE decCBOR #-}

pattern WitVKey ::
  VKey kr ->
  SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody) ->
  WitVKey kr
pattern WitVKey k s <-
  WitVKeyInternal k s _
  where
    WitVKey k s =
      let hash = asWitness $ hashKey k
       in WitVKeyInternal k s hash

{-# COMPLETE WitVKey #-}

-- | Access computed hash. Evaluated lazily
witVKeyHash :: WitVKey kr -> KeyHash Witness
witVKeyHash = wvkKeyHash
