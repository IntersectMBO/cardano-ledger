{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Ledger.State.ChainAccount (
  ChainAccountState (AccountState, asTreasury, asReserves, ..),
  AccountState,
  casTreasuryL,
  casReservesL,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

type AccountState = ChainAccountState

pattern AccountState :: Coin -> Coin -> AccountState
pattern AccountState {asTreasury, asReserves} = ChainAccountState asTreasury asReserves
{-# DEPRECATED AccountState "In favor of `ChainAccountState`" #-}
{-# DEPRECATED asTreasury "In favor of `casTreasury`" #-}
{-# DEPRECATED asReserves "In favor of `casReserves`" #-}

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

casTreasuryL :: Lens' ChainAccountState Coin
casTreasuryL = lens casTreasury (\ds u -> ds {casTreasury = u})

casReservesL :: Lens' ChainAccountState Coin
casReservesL = lens casReserves (\ds u -> ds {casReserves = u})
