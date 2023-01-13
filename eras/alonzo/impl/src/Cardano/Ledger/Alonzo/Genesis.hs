{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Alonzo.Genesis (
  AlonzoGenesis (
    AlonzoGenesisWrapper,
    unAlonzoGenesisWrapper,
    AlonzoGenesis,
    agCoinsPerUTxOWord,
    agCostModels,
    agPrices,
    agMaxTxExUnits,
    agMaxBlockExUnits,
    agMaxValSize,
    agCollateralPercentage,
    agMaxCollateralInputs
  ),
  alonzoGenesisAesonPairs,

  -- * Deprecated
  coinsPerUTxOWord,
  costmdls,
  prices,
  maxTxExUnits,
  maxBlockExUnits,
  maxValSize,
  collateralPercentage,
  maxCollateralInputs,
)
where

import Cardano.Ledger.Alonzo.Core (CoinPerWord (..))

import Cardano.Ledger.Alonzo.PParams (UpgradeAlonzoPParams (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (..), Prices (..))
import Cardano.Ledger.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import Cardano.Ledger.Binary.Coders (
  Decode (From, RecD),
  Encode (Rec, To),
  decode,
  encode,
  (!>),
  (<!),
 )
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

-- | All configuration that is necessary to bootstrap AlonzoEra from ShelleyGenesis
newtype AlonzoGenesis = AlonzoGenesisWrapper
  { unAlonzoGenesisWrapper :: UpgradeAlonzoPParams Identity
  }
  deriving stock (Eq, Generic)
  deriving newtype (Show, NoThunks)

pattern AlonzoGenesis ::
  CoinPerWord ->
  CostModels ->
  Prices ->
  ExUnits ->
  ExUnits ->
  Natural ->
  Natural ->
  Natural ->
  AlonzoGenesis
pattern AlonzoGenesis
  { agCoinsPerUTxOWord
  , agCostModels
  , agPrices
  , agMaxTxExUnits
  , agMaxBlockExUnits
  , agMaxValSize
  , agCollateralPercentage
  , agMaxCollateralInputs
  } <-
  ( unAlonzoGenesisWrapper ->
      UpgradeAlonzoPParams
        { uappCoinsPerUTxOWord = agCoinsPerUTxOWord
        , uappCostModels = agCostModels
        , uappPrices = agPrices
        , uappMaxTxExUnits = agMaxTxExUnits
        , uappMaxBlockExUnits = agMaxBlockExUnits
        , uappMaxValSize = agMaxValSize
        , uappCollateralPercentage = agCollateralPercentage
        , uappMaxCollateralInputs = agMaxCollateralInputs
        }
    )
  where
    AlonzoGenesis
      coinsPerUTxOWord_
      costModels_
      prices_
      maxTxExUnits_
      maxBlockExUnits_
      maxValSize_
      collateralPercentage_
      maxCollateralInputs_ =
        AlonzoGenesisWrapper $
          UpgradeAlonzoPParams
            { uappCoinsPerUTxOWord = coinsPerUTxOWord_
            , uappCostModels = costModels_
            , uappPrices = prices_
            , uappMaxTxExUnits = maxTxExUnits_
            , uappMaxBlockExUnits = maxBlockExUnits_
            , uappMaxValSize = maxValSize_
            , uappCollateralPercentage = collateralPercentage_
            , uappMaxCollateralInputs = maxCollateralInputs_
            }

{-# COMPLETE AlonzoGenesis #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance FromCBOR AlonzoGenesis where
  fromCBOR =
    decode $
      RecD AlonzoGenesis
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance ToCBOR AlonzoGenesis where
  toCBOR
    AlonzoGenesis
      { agCoinsPerUTxOWord
      , agCostModels
      , agPrices
      , agMaxTxExUnits
      , agMaxBlockExUnits
      , agMaxValSize
      , agCollateralPercentage
      , agMaxCollateralInputs
      } =
      encode $
        Rec AlonzoGenesis
          !> To agCoinsPerUTxOWord
          !> To agCostModels
          !> To agPrices
          !> To agMaxTxExUnits
          !> To agMaxBlockExUnits
          !> To agMaxValSize
          !> To agCollateralPercentage
          !> To agMaxCollateralInputs

instance FromJSON AlonzoGenesis where
  parseJSON = Aeson.withObject "Alonzo Genesis" $ \o -> do
    agCoinsPerUTxOWord <- o .: "lovelacePerUTxOWord"
    agCostModels <- o .: "costModels"
    agPrices <- o .: "executionPrices"
    agMaxTxExUnits <- o .: "maxTxExUnits"
    agMaxBlockExUnits <- o .: "maxBlockExUnits"
    agMaxValSize <- o .: "maxValueSize"
    agCollateralPercentage <- o .: "collateralPercentage"
    agMaxCollateralInputs <- o .: "maxCollateralInputs"
    return AlonzoGenesis {..}

instance ToJSON AlonzoGenesis where
  toJSON = object . alonzoGenesisAesonPairs

alonzoGenesisAesonPairs :: Aeson.KeyValue a => AlonzoGenesis -> [a]
alonzoGenesisAesonPairs ag =
  [ "lovelacePerUTxOWord" .= coinsPerUTxOWord ag
  , "costModels" .= costmdls ag
  , "executionPrices" .= prices ag
  , "maxTxExUnits" .= maxTxExUnits ag
  , "maxBlockExUnits" .= maxBlockExUnits ag
  , "maxValueSize" .= maxValSize ag
  , "collateralPercentage" .= collateralPercentage ag
  , "maxCollateralInputs" .= maxCollateralInputs ag
  ]

coinsPerUTxOWord :: AlonzoGenesis -> CoinPerWord
coinsPerUTxOWord = agCoinsPerUTxOWord
{-# DEPRECATED coinsPerUTxOWord "Use `agCoinsPerUTxOWord` instead" #-}

costmdls :: AlonzoGenesis -> CostModels
costmdls = agCostModels
{-# DEPRECATED costmdls "Use `agCostModels` instead" #-}

prices :: AlonzoGenesis -> Prices
prices = agPrices
{-# DEPRECATED prices "Use `agPrices` instead" #-}

maxTxExUnits :: AlonzoGenesis -> ExUnits
maxTxExUnits = agMaxTxExUnits
{-# DEPRECATED maxTxExUnits "Use `agMaxTxExUnits` instead" #-}

maxBlockExUnits :: AlonzoGenesis -> ExUnits
maxBlockExUnits = agMaxBlockExUnits
{-# DEPRECATED maxBlockExUnits "Use `agMaxBlockExUnits` instead" #-}

maxValSize :: AlonzoGenesis -> Natural
maxValSize = agMaxValSize
{-# DEPRECATED maxValSize "Use `agMaxValSize` instead" #-}

collateralPercentage :: AlonzoGenesis -> Natural
collateralPercentage = agCollateralPercentage
{-# DEPRECATED collateralPercentage "Use `agCollateralPercentage` instead" #-}

maxCollateralInputs :: AlonzoGenesis -> Natural
maxCollateralInputs = agMaxCollateralInputs
{-# DEPRECATED maxCollateralInputs "Use `agMaxCollateralInputs` instead" #-}
