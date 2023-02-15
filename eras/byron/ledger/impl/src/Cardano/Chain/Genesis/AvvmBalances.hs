{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missed-specialisations #-}

module Cardano.Chain.Genesis.AvvmBalances (
  GenesisAvvmBalances (..),
)
where

import Cardano.Chain.Common (Lovelace)
import Cardano.Crypto.Signing.Redeem (CompactRedeemVerificationKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  encodeListLen,
  enforceSize,
 )
import Cardano.Prelude
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | Predefined balances of AVVM (Ada Voucher Vending Machine) entries.
-- People who purchased Ada at a pre-sale were issued a certificate during
-- the pre-sale period. These certificates allow customers to redeem ADA.
newtype GenesisAvvmBalances = GenesisAvvmBalances
  { unGenesisAvvmBalances :: Map CompactRedeemVerificationKey Lovelace
  }
  deriving (Show, Eq, Semigroup, NoThunks)

instance Monad m => ToJSON m GenesisAvvmBalances where
  toJSON = toJSON . unGenesisAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisAvvmBalances where
  fromJSON = fmap (GenesisAvvmBalances . forceElemsToWHNF) . fromJSON

instance EncCBOR GenesisAvvmBalances where
  encCBOR (GenesisAvvmBalances gab) =
    encodeListLen 1
      <> encCBOR @(Map CompactRedeemVerificationKey Lovelace) gab

instance DecCBOR GenesisAvvmBalances where
  decCBOR = do
    enforceSize "GenesisAvvmBalances" 1
    GenesisAvvmBalances <$> decCBOR @(Map CompactRedeemVerificationKey Lovelace)
