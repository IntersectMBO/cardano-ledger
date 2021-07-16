{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Block.Body
  ( Body,
    pattern Body,
    ABody (..),
    bodyTxs,
    bodyWitnesses,
  )
where

import Cardano.Binary
  ( ByteSpan,
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscPayload (..))
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxPayload (ATxPayload, TxPayload, txpTxs, txpWitnesses)
import Cardano.Chain.UTxO.TxWitness (TxWitness)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Prelude
import Data.Aeson (ToJSON)

-- | 'Body' consists of payloads of all block components
type Body = ABody ()

-- | Constructor for 'Body'
pattern Body :: TxPayload -> SscPayload -> Delegation.Payload -> Update.Payload -> Body
pattern Body tx ssc dlg upd = ABody tx ssc dlg upd

-- | 'Body' consists of payloads of all block components
data ABody a = ABody
  { -- | UTxO payload
    bodyTxPayload :: !(ATxPayload a),
    -- | Ssc payload
    bodySscPayload :: !SscPayload,
    -- | Heavyweight delegation payload (no-ttl certificates)
    bodyDlgPayload :: !(Delegation.APayload a),
    -- | Additional update information for the update system
    bodyUpdatePayload :: !(Update.APayload a)
  }
  deriving (Eq, Show, Generic, Functor, NFData)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABody a)

instance ToCBOR Body where
  toCBOR bc =
    encodeListLen 4
      <> toCBOR (bodyTxPayload bc)
      <> toCBOR (bodySscPayload bc)
      <> toCBOR (bodyDlgPayload bc)
      <> toCBOR (bodyUpdatePayload bc)

instance FromCBOR Body where
  fromCBOR = void <$> fromCBOR @(ABody ByteSpan)

instance FromCBOR (ABody ByteSpan) where
  fromCBOR = do
    enforceSize "Body" 4
    ABody
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

bodyTxs :: Body -> [Tx]
bodyTxs = txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = txpWitnesses . bodyTxPayload
