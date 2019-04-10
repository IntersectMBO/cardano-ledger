{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Chain.Txp.TxPayload
  ( TxPayload
  , ATxPayload(..)
  , mkTxPayload
  , recoverHashedBytes
  , txpTxs
  , txpWitnesses
  , unTxPayload
  )
where

import Cardano.Prelude

import Cardano.Binary (Annotated(..), ByteSpan, FromCBOR(..), ToCBOR(..))
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Chain.Txp.TxAux (ATxAux, TxAux, aTaWitness, taTx, taWitness)
import Cardano.Chain.Txp.TxWitness (TxWitness)


-- | Payload of Txp component which is part of the block body
type TxPayload = ATxPayload ()

mkTxPayload :: [TxAux] -> TxPayload
mkTxPayload = ATxPayload

newtype ATxPayload a = ATxPayload
  { aUnTxPayload :: [ATxAux a]
  } deriving (Show, Eq, Generic, Functor)
    deriving anyclass NFData

unTxPayload :: ATxPayload a -> [TxAux]
unTxPayload = fmap void . aUnTxPayload

instance ToCBOR TxPayload where
  toCBOR = toCBOR . unTxPayload

instance FromCBOR TxPayload where
  fromCBOR = void <$> fromCBOR @(ATxPayload ByteSpan)

instance FromCBOR (ATxPayload ByteSpan) where
  fromCBOR = ATxPayload <$> fromCBOR

txpTxs :: ATxPayload a -> [Tx]
txpTxs = fmap taTx . unTxPayload

txpWitnesses :: TxPayload -> [TxWitness]
txpWitnesses = fmap taWitness . unTxPayload

recoverHashedBytes :: ATxPayload ByteString -> Annotated [TxWitness] ByteString
recoverHashedBytes (ATxPayload auxs) =
  let
    aWitnesses  = aTaWitness <$> auxs
    prefix      = "\159" :: ByteString
    -- This is the value of Codec.CBOR.Write.toLazyByteString encodeListLenIndef
    suffix      = "\255" :: ByteString
    -- This is the value of Codec.CBOR.Write.toLazyByteString encodeBreak
    -- They are hard coded here because the hashed bytes included them as an
    -- implementation artifact
    hashedByted = prefix <> mconcat (annotation <$> aWitnesses) <> suffix
  in Annotated (unAnnotated <$> aWitnesses) hashedByted
