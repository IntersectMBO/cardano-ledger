{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  toAlonzoGenesisPairs,
) where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (CoinPerWord, UpgradeAlonzoPParams (..))
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
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
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
  deriving (ToJSON) via KeyValuePairs AlonzoGenesis

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
    ]

toAlonzoGenesisPairs :: Aeson.KeyValue e a => AlonzoGenesis -> [a]
toAlonzoGenesisPairs = toKeyValuePairs
{-# DEPRECATED toAlonzoGenesisPairs "In favor of `toKeyValuePairs`" #-}
