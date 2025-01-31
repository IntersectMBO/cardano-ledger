{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.State.AccountState (
  AccountState (..),
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (def))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data AccountState = AccountState
  { asTreasury :: !Coin
  , asReserves :: !Coin
  }
  deriving (Show, Eq, Generic)

instance EncCBOR AccountState where
  encCBOR (AccountState t r) =
    encodeListLen 2 <> encCBOR t <> encCBOR r

instance DecCBOR AccountState where
  decCBOR =
    decodeRecordNamed "AccountState" (const 2) $ AccountState <$> decCBOR <*> decCBOR

instance ToJSON AccountState where
  toJSON = object . toAccountStatePairs
  toEncoding = pairs . mconcat . toAccountStatePairs

toAccountStatePairs :: KeyValue e a => AccountState -> [a]
toAccountStatePairs as@(AccountState _ _) =
  let AccountState {asTreasury, asReserves} = as
   in [ "treasury" .= asTreasury
      , "reserves" .= asReserves
      ]

instance NoThunks AccountState

instance NFData AccountState

instance Default AccountState where
  def = AccountState (Coin 0) (Coin 0)
