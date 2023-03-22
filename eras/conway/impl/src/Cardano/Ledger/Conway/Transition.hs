{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Transition (TransitionConfig (..), toConwayTransitionConfigPairs) where

import Cardano.Ledger.Alonzo.Transition (toAlonzoTransitionConfigPairs)
import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Transition (TransitionConfig (BabbageTransitionConfig))
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Genesis (ConwayGenesis, toConwayGenesisPairs)
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Crypto
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
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTransition (ConwayEra c) where
  data TransitionConfig (ConwayEra c) = ConwayTransitionConfig
    { atcConwayGenesis :: !(ConwayGenesis c)
    , atcBabbageTransitionConfig :: TransitionConfig (BabbageEra c)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = ConwayTransitionConfig

  tcPreviousEraConfigL =
    lens atcBabbageTransitionConfig (\atc pc -> atc {atcBabbageTransitionConfig = pc})

  tcTranslationContextL =
    lens atcConwayGenesis (\atc ag -> atc {atcConwayGenesis = ag})

instance Crypto c => NoThunks (TransitionConfig (ConwayEra c))

instance Crypto c => ToJSON (TransitionConfig (ConwayEra c)) where
  toJSON = object . toConwayTransitionConfigPairs
  toEncoding = pairs . mconcat . toConwayTransitionConfigPairs

toConwayTransitionConfigPairs :: (KeyValue a, Crypto c) => TransitionConfig (ConwayEra c) -> [a]
toConwayTransitionConfigPairs conwayConfig =
  toAlonzoTransitionConfigPairs alonzoConfig
    ++ ["conway" .= object (toConwayGenesisPairs (conwayConfig ^. tcTranslationContextL))]
  where
    babbageConfig = conwayConfig ^. tcPreviousEraConfigL
    alonzoConfig = babbageConfig ^. tcPreviousEraConfigL

instance Crypto c => FromJSON (TransitionConfig (ConwayEra c)) where
  parseJSON = withObject "ConwayTransitionConfig" $ \o -> do
    pc <- parseJSON (Object o)
    ag <- o .: "conway"
    pure $ mkTransitionConfig pc ag
