{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- some GHC bug wrongfully complains about CanSetChainAccountState constraint being redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.State.ChainAccount (
  CanGetChainAccountState (..),
  CanSetChainAccountState (..),
  ChainAccountState (AccountState, asTreasury, asReserves, ..),
  AccountState,
  casTreasuryL,
  casReservesL,
  treasuryL,
  reservesL,
  emptyChainAccountState,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

class CanGetChainAccountState t where
  chainAccountStateG :: SimpleGetter (t era) ChainAccountState
  default chainAccountStateG :: CanSetChainAccountState t => SimpleGetter (t era) ChainAccountState
  chainAccountStateG = chainAccountStateL
  {-# INLINE chainAccountStateG #-}

class CanGetChainAccountState t => CanSetChainAccountState t where
  chainAccountStateL :: Lens' (t era) ChainAccountState

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

emptyChainAccountState :: ChainAccountState
emptyChainAccountState = 
  ChainAccountState
    mempty
    mempty

toChainAccountStatePairs :: KeyValue e a => ChainAccountState -> [a]
toChainAccountStatePairs as@(ChainAccountState _ _) =
  let ChainAccountState {casTreasury, casReserves} = as
   in [ "treasury" .= casTreasury
      , "reserves" .= casReserves
      ]

instance NoThunks ChainAccountState

instance NFData ChainAccountState

casTreasuryL :: Lens' ChainAccountState Coin
casTreasuryL = lens casTreasury (\ds u -> ds {casTreasury = u})
{-# INLINE casTreasuryL #-}

casReservesL :: Lens' ChainAccountState Coin
casReservesL = lens casReserves (\ds u -> ds {casReserves = u})
{-# INLINE casReservesL #-}

treasuryL :: CanSetChainAccountState t => Lens' (t era) Coin
treasuryL = chainAccountStateL . lens casTreasury (\ds u -> ds {casTreasury = u})
{-# INLINE treasuryL #-}

reservesL :: CanSetChainAccountState t => Lens' (t era) Coin
reservesL = chainAccountStateL . lens casReserves (\ds u -> ds {casReserves = u})
{-# INLINE reservesL #-}
