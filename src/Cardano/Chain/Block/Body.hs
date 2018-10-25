{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Body
       ( Body (..)
       , BodyError (..)
       , bodyTxs
       , bodyWitnesses
       , verifyBody
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))
import           Formatting (bprint, build)
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import qualified Cardano.Chain.Delegation.Payload as Delegation (Payload,
                     PayloadError (..), checkPayload)
import           Cardano.Chain.Ssc (SscPayload (..))
import           Cardano.Chain.Txp.Tx (Tx)
import           Cardano.Chain.Txp.TxPayload (TxPayload (..), txpTxs,
                     txpWitnesses)
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
  } deriving (Eq, Show, Generic, NFData)

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

data BodyError
  = BodyDelegationPayloadError Delegation.PayloadError
  | BodyUpdatePayloadError Update.PayloadError

instance B.Buildable BodyError where
  build = \case
    BodyDelegationPayloadError err -> bprint
      ("Delegation.Payload was invalid while checking Body.\n Error: " . build)
      err
    BodyUpdatePayloadError err -> bprint
      ("Update.Payload was invalid while checking Body.\n Error: " . build)
      err

verifyBody :: MonadError BodyError m => ProtocolMagic -> Body -> m ()
verifyBody pm mb = do
  either (throwError . BodyDelegationPayloadError) pure
    $ Delegation.checkPayload pm (bodyDlgPayload mb)
  either (throwError . BodyUpdatePayloadError) pure
    $ Update.checkPayload pm (bodyUpdatePayload mb)

bodyTxs :: Body -> [Tx]
bodyTxs = txpTxs . bodyTxPayload

bodyWitnesses :: Body -> [TxWitness]
bodyWitnesses = txpWitnesses . bodyTxPayload
