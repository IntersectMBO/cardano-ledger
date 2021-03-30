{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.AvvmBalances
  ( GenesisAvvmBalances(..)
  )
where

import Cardano.Prelude

import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary
  ( FromCBOR(..)
  , ToCBOR(..)
  , enforceSize
  , encodeListLen
  )
import Cardano.Chain.Common (Lovelace)
import Cardano.Crypto.Signing.Redeem (CompactRedeemVerificationKey)


-- | Predefined balances of AVVM (Ada Voucher Vending Machine) entries.
-- People who purchased Ada at a pre-sale were issued a certificate during
-- the pre-sale period. These certificates allow customers to redeem ADA.
newtype GenesisAvvmBalances = GenesisAvvmBalances
  { unGenesisAvvmBalances :: Map CompactRedeemVerificationKey Lovelace
  } deriving (Show, Eq, Semigroup, NoThunks)

instance Monad m => ToJSON m GenesisAvvmBalances where
    toJSON = toJSON . unGenesisAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisAvvmBalances where
    -- | Unfortunately, because @canonical-json@ doesn't utilize operations from
    -- "Data.Map.Strict" but only those from "Data.Map.Lazy" (i.e. 'fromJSON'
    -- will return a 'Map' that is not necessarily strict in its values), we
    -- need to be careful in order to ensure that we're still dealing with a
    -- 'Map' that's strict in both its keys and values.
    --
    -- To remedy this, we use 'forceElemsToWHNF' from "Cardano.Prelude" to
    -- convert the 'Map' to one that is now guaranteed to be strict in both
    -- its keys and values.
    --
    -- n.b. both the strict and lazy 'Map' modules utilize the same 'Map' data
    -- type which is what makes something like this possible.
    fromJSON = fmap (GenesisAvvmBalances . forceElemsToWHNF) . fromJSON

instance ToCBOR GenesisAvvmBalances where
  toCBOR (GenesisAvvmBalances gab)
    = encodeListLen 1
      <> toCBOR @(Map CompactRedeemVerificationKey Lovelace) gab

instance FromCBOR GenesisAvvmBalances where
  fromCBOR = do
    enforceSize "GenesisAvvmBalances" 1
    GenesisAvvmBalances <$> fromCBOR @(Map CompactRedeemVerificationKey Lovelace)
