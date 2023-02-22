{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Block.Body (
  Body,
  pattern Body,
  ABody (..),
  bodyTxs,
  bodyWitnesses,
)
where

import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscPayload (..))
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxPayload (ATxPayload, TxPayload, txpTxs, txpWitnesses)
import Cardano.Chain.UTxO.TxWitness (TxWitness)
import qualified Cardano.Chain.Update.Payload as Update
import Cardano.Ledger.Binary (
  ByteSpan,
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)

-- | 'Body' consists of payloads of all block components
type Body = ABody ()

-- | Constructor for 'Body'
pattern Body :: TxPayload -> SscPayload -> Delegation.Payload -> Update.Payload -> Body
pattern Body tx ssc dlg upd = ABody tx ssc dlg upd

-- | 'Body' consists of payloads of all block components
data ABody a = ABody
  { bodyTxPayload :: !(ATxPayload a)
  -- ^ UTxO payload
  , bodySscPayload :: !SscPayload
  -- ^ Ssc payload
  , bodyDlgPayload :: !(Delegation.APayload a)
  -- ^ Heavyweight delegation payload (no-ttl certificates)
  , bodyUpdatePayload :: !(Update.APayload a)
  -- ^ Additional update information for the update system
  }
  deriving (Eq, Show, Generic, Functor, NFData)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ABody a)

instance ToCBOR Body where
  toCBOR = toByronCBOR

instance FromCBOR Body where
  fromCBOR = fromByronCBOR

instance FromCBOR (ABody ByteSpan) where
  fromCBOR = fromByronCBOR

instance EncCBOR Body where
  encCBOR bc =
    encodeListLen 4
      <> encCBOR (bodyTxPayload bc)
      <> encCBOR (bodySscPayload bc)
      <> encCBOR (bodyDlgPayload bc)
      <> encCBOR (bodyUpdatePayload bc)

instance DecCBOR Body where
  decCBOR = void <$> decCBOR @(ABody ByteSpan)

instance DecCBOR (ABody ByteSpan) where
  decCBOR = do
    enforceSize "Body" 4
    ABody
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR

bodyTxs :: Body -> [Tx]
bodyTxs = txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = txpWitnesses . bodyTxPayload
