{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.UTxO.TxPayload
  ( TxPayload(..)
  , recoverHashedBytes
  , txpTxs
  , txpWitnesses
  )
where

import Cardano.Prelude

import Cardano.Binary (Annotated(..), FromCBORAnnotated(..), ToCBOR(..))
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxAux (TxAux(..))
import Cardano.Chain.UTxO.TxWitness (TxWitness(..))


-- | Payload of UTxO component which is part of the block body
newtype TxPayload = TxPayload
  { unTxPayload :: [TxAux]
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

instance ToCBOR TxPayload where
  toCBOR = toCBOR . unTxPayload

instance FromCBORAnnotated TxPayload where
  fromCBORAnnotated' = TxPayload <$> fromCBORAnnotated'

txpTxs :: TxPayload -> [Tx]
txpTxs = fmap taTx . unTxPayload

txpWitnesses :: TxPayload -> [TxWitness]
txpWitnesses = fmap taWitness . unTxPayload

recoverHashedBytes :: TxPayload -> Annotated [TxWitness] ByteString
recoverHashedBytes (TxPayload auxs) =
  let
    witnesses  = taWitness <$> auxs
    prefix      = "\159" :: ByteString
    -- This is the value of Codec.CBOR.Write.toLazyByteString encodeListLenIndef
    suffix      = "\255" :: ByteString
    -- This is the value of Codec.CBOR.Write.toLazyByteString encodeBreak
    -- They are hard coded here because the hashed bytes included them as an
    -- implementation artifact
    hashedByted = prefix <> mconcat (txWitnessSerialized <$> witnesses) <> suffix
  in Annotated witnesses hashedByted
