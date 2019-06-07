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

import Control.Monad.Except (MonadError(..))
import qualified Data.Aeson as Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Map.Strict as M
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary (DecoderError)
import Cardano.Chain.Common
  ( Address
  , Lovelace
  , LovelaceError
  , addLovelace
  , fromCBORTextAddress
  , integerToLovelace
  , unsafeGetLovelace
  )


-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
  { unGenesisNonAvvmBalances :: Map Address Lovelace
  } deriving (Show, Eq)

instance B.Buildable GenesisNonAvvmBalances where
  build (GenesisNonAvvmBalances m) =
    bprint ("GenesisNonAvvmBalances: " . mapJson) m

deriving instance Semigroup GenesisNonAvvmBalances
deriving instance Monoid GenesisNonAvvmBalances

instance Monad m => ToJSON m GenesisNonAvvmBalances where
  toJSON = toJSON . unGenesisNonAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisNonAvvmBalances where
  fromJSON = fmap GenesisNonAvvmBalances . fromJSON

instance Aeson.ToJSON GenesisNonAvvmBalances where
  toJSON = Aeson.toJSON . convert . unGenesisNonAvvmBalances
   where
    convert :: Map Address Lovelace -> Map Text Integer
    convert = M.fromList . map f . M.toList
    f :: (Address, Lovelace) -> (Text, Integer)
    f = bimap (sformat build) (toInteger . unsafeGetLovelace)

instance Aeson.FromJSON GenesisNonAvvmBalances where
  parseJSON = toAesonError . convertNonAvvmDataToBalances <=< Aeson.parseJSON

data NonAvvmBalancesError
  = NonAvvmBalancesLovelaceError LovelaceError
  | NonAvvmBalancesDecoderError DecoderError

instance B.Buildable NonAvvmBalancesError where
  build = \case
    NonAvvmBalancesLovelaceError err -> bprint
      ("Failed to construct a lovelace in NonAvvmBalances.\n Error: " . build)
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
  mkBalances :: [(Address, Lovelace)] -> m (Map Address Lovelace)
  mkBalances =
    -- Pull 'LovelaceError's out of the 'Map' and lift them to
    -- 'NonAvvmBalancesError's
    (`wrapError` NonAvvmBalancesLovelaceError)
    . sequence
    -- Make map joining duplicate keys with 'addLovelace' lifted from 'Lovelace ->
    -- Lovelace -> Either LovelaceError Lovelace' to 'Either LovelaceError Lovelace -> Either
    -- LovelaceError Lovelace -> Either LovelaceError Lovelace'
    . M.fromListWith (\c -> join . liftM2 addLovelace c)
    -- Lift the 'Lovelace's to 'Either LovelaceError Lovelace's
    . fmap (second Right)

  convert :: (Text, Integer) -> m (Address, Lovelace)
  convert (txt, i) = do
    addr <- fromCBORTextAddress txt `wrapError` NonAvvmBalancesDecoderError
    lovelace <-integerToLovelace i `wrapError` NonAvvmBalancesLovelaceError
    return (addr, lovelace)
