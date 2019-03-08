{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Body
  ( Body
  , body
  , ABody(..)
  , bodyTxs
  , bodyWitnesses
  , decodeABody
  )
where

import Cardano.Prelude

import Cardano.Binary.Class
  (Bi(..), ByteSpan, Decoder, encodeListLen, enforceSize)
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscPayload(..))
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Chain.Txp.TxPayload
  (ATxPayload, TxPayload, decodeATxPayload, txpTxs, txpWitnesses)
import Cardano.Chain.Txp.TxWitness (TxWitness)
import qualified Cardano.Chain.Update.Payload as Update

-- | 'Body' consists of payloads of all block components
type Body = ABody ()

-- | Constructor for 'Body'
body :: TxPayload -> SscPayload -> Delegation.Payload -> Update.Payload -> Body
body tx ssc dlg upd = ABody tx ssc dlg upd

-- | 'Body' consists of payloads of all block components
data ABody a = ABody
  { bodyTxPayload     :: !(ATxPayload a)
  -- ^ Txp payload
  , bodySscPayload    :: !SscPayload
  -- ^ Ssc payload
  , bodyDlgPayload    :: !(Delegation.APayload a)
  -- ^ Heavyweight delegation payload (no-ttl certificates)
  , bodyUpdatePayload :: !(Update.APayload a)
  -- ^ Additional update information for the update system
  } deriving (Eq, Show, Generic, Functor, NFData)

instance Bi (ABody ()) where
  encode bc =
    encodeListLen 4
      <> encode (bodyTxPayload bc)
      <> encode (bodySscPayload bc)
      <> encode (bodyDlgPayload bc)
      <> encode (bodyUpdatePayload bc)

  decode = void <$> decodeABody

decodeABody :: Decoder s (ABody ByteSpan)
decodeABody = do
  enforceSize "Body" 4
  ABody
    <$> decodeATxPayload
    <*> decode
    <*> Delegation.decodeAPayload
    <*> Update.decodeAPayload

bodyTxs :: Body -> [Tx]
bodyTxs = txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = txpWitnesses . bodyTxPayload
