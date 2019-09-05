{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Cardano.Chain.Block.Body
  ( Body(..)
  , pattern Body
  , bodyTxs
  , bodyWitnesses
  )
where

import Cardano.Prelude

import Cardano.Binary
  (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize,
  FromCBORAnnotated(..), serializeEncoding', encodePreEncoded, serialize',
  liftByteSpanDecoder, withSlice')
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscPayload(..))
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxPayload (TxPayload, txpTxs, txpWitnesses)
import Cardano.Chain.UTxO.TxWitness (TxWitness)
import qualified Cardano.Chain.Update.Payload as Update

-- | Constructor for 'Body'
pattern Body :: TxPayload -> SscPayload -> Delegation.Payload -> Update.Payload -> Body
pattern Body bodyTxPayload bodySscPayload bodyDlgPayload bodyUpdatePayload <-
  Body'
    bodyTxPayload
    bodySscPayload
    (void -> bodyDlgPayload)
    (void -> bodyUpdatePayload)
    _
  where
  Body tx ssc dlg upd =
    let bytes = serializeEncoding' $ encodeListLen 4
          <> toCBOR tx
          <> encodePreEncoded sscBytes
          <> encodePreEncoded dlgBytes
          <> encodePreEncoded updBytes
        sscBytes = serialize' ssc
        dlgBytes = serialize' dlg
        updBytes = serialize' upd
    -- FIXME: This constructs the members of members of the body with incorrect
    -- bytestring references. We'd need to make the same change we made to body
    -- all the way down to correct this problem.
    in Body' tx ssc (dlgBytes <$ dlg) (updBytes <$ upd) bytes

-- | 'Body' consists of payloads of all block components
data Body = Body'
  { bodyTxPayload     :: !TxPayload
  -- ^ UTxO payload
  , bodySscPayload    :: !SscPayload
  -- ^ Ssc payload
  , bodyDlgPayload    :: !(Delegation.APayload ByteString)
  -- ^ Heavyweight delegation payload (no-ttl certificates)
  , bodyUpdatePayload :: !(Update.APayload ByteString)
  -- ^ Additional update information for the update system
  , bodySerialized    :: ByteString
  } deriving (Eq, Show, Generic, NFData)

instance ToCBOR Body where
  toCBOR = encodePreEncoded . bodySerialized

instance FromCBORAnnotated Body where
  fromCBORAnnotated' = withSlice' $
    Body' <$ lift (enforceSize "Body" 4)
      <*> fromCBORAnnotated'
      <*> lift fromCBOR
      <*> liftByteSpanDecoder fromCBOR
      <*> liftByteSpanDecoder fromCBOR

bodyTxs :: Body -> [Tx]
bodyTxs = txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = txpWitnesses . bodyTxPayload
