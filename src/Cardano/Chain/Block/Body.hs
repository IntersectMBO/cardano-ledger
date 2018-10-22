{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Body
       ( Body (..)
       , bodyTxs
       , bodyWitnesses
       , verifyBody
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import qualified Cardano.Chain.Delegation.Payload as Delegation (Payload,
                     checkPayload)
import           Cardano.Chain.Ssc (SscPayload (..))
import           Cardano.Chain.Txp.Tx (Tx)
import           Cardano.Chain.Txp.TxPayload (TxPayload (..), checkTxPayload)
import           Cardano.Chain.Txp.TxWitness (TxWitness)
import qualified Cardano.Chain.Update.Payload as Update
import           Cardano.Crypto (ProtocolMagic)


-- | 'Body' consists of payloads of all block components
data Body = Body
  { bodyTxPayload     :: !TxPayload
  -- ^ Txp payload
  , bodySscPayload    :: !SscPayload
  -- ^ Ssc payload
  , bodyDlgPayload    :: !Delegation.Payload
  -- ^ Heavyweight delegation payload (no-ttl certificates)
  , bodyUpdatePayload :: !Update.Payload
  -- ^ Additional update information for the update system
  } deriving (Eq, Show, Generic, NFData, Typeable)

instance Bi Body where
  encode bc =
    encodeListLen 4
      <> encode (bodyTxPayload bc)
      <> encode (bodySscPayload bc)
      <> encode (bodyDlgPayload bc)
      <> encode (bodyUpdatePayload bc)

  decode = do
    enforceSize "Body" 4
    Body <$> decode <*> decode <*> decode <*> decode

verifyBody :: MonadError Text m => ProtocolMagic -> Body -> m ()
verifyBody pm mb = do
  checkTxPayload (bodyTxPayload mb)
  Delegation.checkPayload pm (bodyDlgPayload mb)
  Update.checkPayload pm (bodyUpdatePayload mb)

bodyTxs :: Body -> [Tx]
bodyTxs = _txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = _txpWitnesses . bodyTxPayload
