{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Genesis (
  AlonzoGenesis (
    AlonzoGenesisWrapper,
    unAlonzoGenesisWrapper,
    extraConfig,
    AlonzoGenesis,
    agCoinsPerUTxOWord,
    agCostModels,
    agPrices,
    agMaxTxExUnits,
    agMaxBlockExUnits,
    agMaxValSize,
    agCollateralPercentage,
    agMaxCollateralInputs,
    agExtraConfig
  ),
  AlonzoExtraConfig (..),
) where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (
  CoinPerWord,
  UpgradeAlonzoPParams (..),
 )
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits (..), Prices (..))
import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR,
  EncCBOR,
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (From, RecD),
  Encode (Rec, To),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (EraGenesis (..))
import Cardano.Ledger.Plutus.CostModels (parseCostModels)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

-- | All configuration that is necessary to bootstrap AlonzoEra from ShelleyGenesis
data AlonzoGenesis = AlonzoGenesisWrapper
  { unAlonzoGenesisWrapper :: UpgradeAlonzoPParams Identity
  , extraConfig :: AlonzoExtraConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs AlonzoGenesis

instance NoThunks AlonzoGenesis

instance NFData AlonzoGenesis

newtype AlonzoExtraConfig = AlonzoExtraConfig
  { aecCostModels :: Maybe CostModels
  }
  deriving (Eq)
  deriving newtype (EncCBOR, DecCBOR, NFData, NoThunks, Show)

instance FromJSON AlonzoExtraConfig where
  parseJSON = Aeson.withObject "Extra Config" $ \o ->
    o .:? "costModels" >>= \case
      Nothing -> pure $ AlonzoExtraConfig Nothing
      Just val -> AlonzoExtraConfig . Just <$> parseCostModels True [] val

instance ToJSON AlonzoExtraConfig where
  toJSON (AlonzoExtraConfig cms) = Aeson.object ["costModels" .= cms]

pattern AlonzoGenesis ::
  CoinPerWord ->
  CostModels ->
  Prices ->
  ExUnits ->
  ExUnits ->
  Natural ->
  Natural ->
  Natural ->
  AlonzoExtraConfig ->
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
  , agExtraConfig
  } <-
  AlonzoGenesisWrapper
    { unAlonzoGenesisWrapper =
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
    , extraConfig = agExtraConfig
    }
  where
    AlonzoGenesis
      coinsPerUTxOWord_
      costModels_
      prices_
      maxTxExUnits_
      maxBlockExUnits_
      maxValSize_
      collateralPercentage_
      maxCollateralInputs_
      extraConfig_ =
        AlonzoGenesisWrapper
          ( UpgradeAlonzoPParams
              { uappCoinsPerUTxOWord = coinsPerUTxOWord_
              , uappCostModels = costModels_
              , uappPrices = prices_
              , uappMaxTxExUnits = maxTxExUnits_
              , uappMaxBlockExUnits = maxBlockExUnits_
              , uappMaxValSize = maxValSize_
              , uappCollateralPercentage = collateralPercentage_
              , uappMaxCollateralInputs = maxCollateralInputs_
              }
          )
          extraConfig_

{-# COMPLETE AlonzoGenesis #-}

instance EraGenesis AlonzoEra where
  type Genesis AlonzoEra = AlonzoGenesis

-- | Genesis types are always encoded with the version of era they are defined in.
instance DecCBOR AlonzoGenesis

instance EncCBOR AlonzoGenesis

instance FromCBOR AlonzoGenesis where
  fromCBOR =
    eraDecoder @AlonzoEra $
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
      toEraCBOR @AlonzoEra
        . encode
        $ Rec AlonzoGenesis
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
    agCostModels <- parseCostModels False =<< o .: "costModels"
    agPrices <- o .: "executionPrices"
    agMaxTxExUnits <- o .: "maxTxExUnits"
    agMaxBlockExUnits <- o .: "maxBlockExUnits"
    agMaxValSize <- o .: "maxValueSize"
    agCollateralPercentage <- o .: "collateralPercentage"
    agMaxCollateralInputs <- o .: "maxCollateralInputs"
    agExtraConfig <- o .: "extraConfig"
    return AlonzoGenesis {..}

instance ToKeyValuePairs AlonzoGenesis where
  toKeyValuePairs ag =
    [ "lovelacePerUTxOWord" .= agCoinsPerUTxOWord ag
    , "costModels" .= agCostModels ag
    , "executionPrices" .= agPrices ag
    , "maxTxExUnits" .= agMaxTxExUnits ag
    , "maxBlockExUnits" .= agMaxBlockExUnits ag
    , "maxValueSize" .= agMaxValSize ag
    , "collateralPercentage" .= agCollateralPercentage ag
    , "maxCollateralInputs" .= agMaxCollateralInputs ag
    , "extraConfig" .= agExtraConfig ag
    ]
