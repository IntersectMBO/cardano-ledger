{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Txp.TxPayload
  ( TxPayload(..)
  , txpTxs
  , txpWitnesses
  )
where

import Cardano.Prelude

import Cardano.Binary.Class (Bi(..))
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Chain.Txp.TxAux (TxAux(..))
import Cardano.Chain.Txp.TxWitness (TxWitness)


-- | Payload of Txp component which is part of the block body
newtype TxPayload = TxPayload
  { unTxPayload :: [TxAux]
  } deriving (Show, Eq, Generic)
    deriving newtype Bi
    deriving anyclass NFData

txpTxs :: TxPayload -> [Tx]
txpTxs = fmap taTx . unTxPayload

txpWitnesses :: TxPayload -> [TxWitness]
txpWitnesses = fmap taWitness . unTxPayload
