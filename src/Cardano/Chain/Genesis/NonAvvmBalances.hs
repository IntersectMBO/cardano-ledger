{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Genesis.NonAvvmBalances
  ( GenesisNonAvvmBalances(..)
  , convertNonAvvmDataToBalances
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..), liftEither)
import qualified Data.Aeson as Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Map.Strict as M
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary.Class (DecoderError)
import Cardano.Chain.Common
  ( Address
  , Coin
  , CoinError
  , addCoin
  , decodeTextAddress
  , integerToCoin
  , unsafeGetCoin
  )


-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
  { getGenesisNonAvvmBalances :: Map Address Coin
  } deriving (Show, Eq)

instance B.Buildable GenesisNonAvvmBalances where
  build (GenesisNonAvvmBalances m) =
    bprint ("GenesisNonAvvmBalances: " . mapJson) m

deriving instance Semigroup GenesisNonAvvmBalances
deriving instance Monoid GenesisNonAvvmBalances

instance Monad m => ToJSON m GenesisNonAvvmBalances where
  toJSON = toJSON . getGenesisNonAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisNonAvvmBalances where
  fromJSON = fmap GenesisNonAvvmBalances . fromJSON

instance Aeson.ToJSON GenesisNonAvvmBalances where
  toJSON = Aeson.toJSON . convert . getGenesisNonAvvmBalances
   where
    convert :: Map Address Coin -> Map Text Integer
    convert = M.fromList . map f . M.toList
    f :: (Address, Coin) -> (Text, Integer)
    f = bimap (sformat build) (toInteger . unsafeGetCoin)

instance Aeson.FromJSON GenesisNonAvvmBalances where
  parseJSON = toAesonError . convertNonAvvmDataToBalances <=< Aeson.parseJSON

data NonAvvmBalancesError
  = NonAvvmBalancesCoinError CoinError
  | NonAvvmBalancesDecoderError DecoderError

instance B.Buildable NonAvvmBalancesError where
  build = \case
    NonAvvmBalancesCoinError err -> bprint
      ("Failed to construct a coin in NonAvvmBalances.\n Error: " . build)
      err
    NonAvvmBalancesDecoderError err -> bprint
      ("Failed to decode NonAvvmBalances.\n Error: " . build)
      err

-- | Generate genesis address distribution out of avvm parameters. Txdistr of
--   the utxo is all empty. Redelegate it in calling function.
convertNonAvvmDataToBalances
  :: forall m
   . MonadError NonAvvmBalancesError m
  => Map Text Integer
  -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = fmap GenesisNonAvvmBalances $ do
  converted <- traverse convert (M.toList balances)
  mkBalances converted
 where
  mkBalances :: [(Address, Coin)] -> m (Map Address Coin)
  mkBalances =
    liftEither
      -- Pull 'CoinError's out of the 'Map' and lift them to
      -- 'NonAvvmBalancesError's
      . first NonAvvmBalancesCoinError
      . sequence
      -- Make map joining duplicate keys with 'addCoin' lifted from 'Coin ->
      -- Coin -> Either CoinError Coin' to 'Either CoinError Coin -> Either
      -- CoinError Coin -> Either CoinError Coin'
      . M.fromListWith (\c -> join . liftM2 addCoin c)
      -- Lift the 'Coin's to 'Either CoinError Coin's
      . fmap (second Right)

  convert :: (Text, Integer) -> m (Address, Coin)
  convert (txt, i) = do
    addr <- liftEither . first NonAvvmBalancesDecoderError $ decodeTextAddress
      txt
    coin <- liftEither . first NonAvvmBalancesCoinError $ integerToCoin i
    return (addr, coin)
