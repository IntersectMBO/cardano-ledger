{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.State.ChainAccount (
  ChainAccountState (..),
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (def))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data ChainAccountState = ChainAccountState
  { casTreasury :: !Coin
  , casReserves :: !Coin
  }
  deriving (Show, Eq, Generic)

instance EncCBOR ChainAccountState where
  encCBOR (ChainAccountState t r) =
    encodeListLen 2 <> encCBOR t <> encCBOR r

instance DecCBOR ChainAccountState where
  decCBOR =
    decodeRecordNamed "ChainAccountState" (const 2) $ ChainAccountState <$> decCBOR <*> decCBOR

instance ToJSON ChainAccountState where
  toJSON = object . toChainAccountStatePairs
  toEncoding = pairs . mconcat . toChainAccountStatePairs

toChainAccountStatePairs :: KeyValue e a => ChainAccountState -> [a]
toChainAccountStatePairs as@(ChainAccountState _ _) =
  let ChainAccountState {casTreasury, casReserves} = as
   in [ "treasury" .= casTreasury
      , "reserves" .= casReserves
      ]

instance NoThunks ChainAccountState

instance NFData ChainAccountState

instance Default ChainAccountState where
  def = ChainAccountState (Coin 0) (Coin 0)
