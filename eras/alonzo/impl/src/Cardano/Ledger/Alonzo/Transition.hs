{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Transition (
  TransitionConfig (..),
  toAlonzoTransitionConfigPairs,
) where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis, toAlonzoGenesisPairs)
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Core
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Transition (TransitionConfig (MaryTransitionConfig))
import Cardano.Ledger.Shelley.Transition
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  Value (..),
  object,
  pairs,
  withObject,
  (.:),
 )
import qualified Data.Aeson as Aeson (Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition AlonzoEra where
  data TransitionConfig AlonzoEra = AlonzoTransitionConfig
    { atcAlonzoGenesis :: !AlonzoGenesis
    , atcMaryTransitionConfig :: !(TransitionConfig MaryEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = AlonzoTransitionConfig

  injectIntoTestState = registerInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens atcMaryTransitionConfig (\atc pc -> atc {atcMaryTransitionConfig = pc})

  tcTranslationContextL =
    lens atcAlonzoGenesis (\atc ag -> atc {atcAlonzoGenesis = ag})

instance NoThunks (TransitionConfig AlonzoEra)

instance ToJSON (TransitionConfig AlonzoEra) where
  toJSON = object . toAlonzoTransitionConfigPairs
  toEncoding = pairs . mconcat . toAlonzoTransitionConfigPairs

toAlonzoTransitionConfigPairs :: KeyValue e a => TransitionConfig AlonzoEra -> [a]
toAlonzoTransitionConfigPairs alonzoConfig =
  toShelleyTransitionConfigPairs shelleyConfig
    ++ ["alonzo" .= object (toAlonzoGenesisPairs (alonzoConfig ^. tcTranslationContextL))]
  where
    maryConfig = alonzoConfig ^. tcPreviousEraConfigL
    allegraConfig = maryConfig ^. tcPreviousEraConfigL
    shelleyConfig = allegraConfig ^. tcPreviousEraConfigL

instance FromJSON (TransitionConfig AlonzoEra) where
  parseJSON = parseTransitionConfigJSON
