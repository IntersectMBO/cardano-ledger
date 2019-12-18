{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Value
    (
      Value(..)
    , Quantity(..)
    -- , CurrencyId(..)
    -- , TokenId(..)
    ) where

import           Cardano.Binary (ToCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Map
import           Cardano.Ledger.Shelley.Crypto
-- import           TxData (ScriptHash(..))

-- | Currency ID
-- type CurrencyId crypto = ScriptHash crypto

-- | Token ID
-- type TokenId crypto = ScriptHash crypto

-- | Quantity
type Quantity = Integer

-- | Value type
type Value currencyId tokenId = Map currencyId (Map tokenId Quantity)
--  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)

defaultValue :: Value
defaultValue = 

-- | Overloaded functions for operations on underlying numeric Quantity in
-- | the Value type (in a scalar way)
