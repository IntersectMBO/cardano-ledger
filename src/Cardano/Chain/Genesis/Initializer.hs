{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Genesis.Initializer
       ( GenesisInitializer (..)
       , TestnetBalanceOptions (..)
       , FakeAvvmOptions (..)
       ) where

import           Cardano.Prelude

import           Cardano.Chain.Common
    (CoinPortion)
import           Data.Aeson.Options
    (defaultOptions)
import           Data.Aeson.TH
    (deriveJSON)

-- | This data type contains various options which determine genesis
-- stakes, balanaces, heavy delegation, etc.
data GenesisInitializer = GenesisInitializer
  { giTestBalance       :: !TestnetBalanceOptions
  , giFakeAvvmBalance   :: !FakeAvvmOptions
  , giAvvmBalanceFactor :: !CoinPortion
  -- ^ Avvm balances will be multiplied by this factor.
  , giUseHeavyDlg       :: !Bool
  -- ^ Whether to use heavyweight delegation for bootstrap era
  -- stakeholders.
  , giSeed              :: !Integer
    -- ^ Seed to use to generate secret data. There are two
    -- ways to use it:
    --
    -- 1. Keep it secret and use genesis data generated from it.
    -- 2. Just use it directly and keep it public if you want
    -- to deploy testing cluster.
  } deriving (Eq, Show)


-- | These options determine balances of nodes specific for testnet.
data TestnetBalanceOptions = TestnetBalanceOptions
  { tboPoors          :: !Word
  -- ^ Number of poor nodes (with small balance).
  , tboRichmen        :: !Word
  -- ^ Number of rich nodes (with huge balance).
  , tboTotalBalance   :: !Word64
  -- ^ Total balance owned by these nodes.
  , tboRichmenShare   :: !Double
  -- ^ Portion of stake owned by all richmen together.
  , tboUseHDAddresses :: !Bool
  -- ^ Whether generate plain addresses or with hd payload.
  } deriving (Eq, Show)


-- | These options determines balances of fake AVVM nodes which didn't
-- really go through vending, but pretend they did.
data FakeAvvmOptions = FakeAvvmOptions
  { faoCount      :: !Word
  , faoOneBalance :: !Word64
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''GenesisInitializer
deriveJSON defaultOptions ''TestnetBalanceOptions
deriveJSON defaultOptions ''FakeAvvmOptions
