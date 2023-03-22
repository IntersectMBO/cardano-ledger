{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Transition (
  TransitionConfig (..),
  toAlonzoTransitionConfigPairs,
) where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis, toAlonzoGenesisPairs)
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Crypto
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
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTransition (AlonzoEra c) where
  data TransitionConfig (AlonzoEra c) = AlonzoTransitionConfig
    { atcAlonzoGenesis :: !AlonzoGenesis
    , atcMaryTransitionConfig :: TransitionConfig (MaryEra c)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = AlonzoTransitionConfig

  tcPreviousEraConfigL =
    lens atcMaryTransitionConfig (\atc pc -> atc {atcMaryTransitionConfig = pc})

  tcTranslationContextL =
    lens atcAlonzoGenesis (\atc ag -> atc {atcAlonzoGenesis = ag})

instance Crypto c => NoThunks (TransitionConfig (AlonzoEra c))

instance Crypto c => ToJSON (TransitionConfig (AlonzoEra c)) where
  toJSON = object . toAlonzoTransitionConfigPairs
  toEncoding = pairs . mconcat . toAlonzoTransitionConfigPairs

toAlonzoTransitionConfigPairs :: (KeyValue a, Crypto c) => TransitionConfig (AlonzoEra c) -> [a]
toAlonzoTransitionConfigPairs alonzoConfig =
  toShelleyTransitionConfigPairs shelleyConfig
    ++ ["alonzo" .= object (toAlonzoGenesisPairs (alonzoConfig ^. tcTranslationContextL))]
  where
    maryConfig = alonzoConfig ^. tcPreviousEraConfigL
    allegraConfig = maryConfig ^. tcPreviousEraConfigL
    shelleyConfig = allegraConfig ^. tcPreviousEraConfigL

instance Crypto c => FromJSON (TransitionConfig (AlonzoEra c)) where
  parseJSON = withObject "AlonzoTransitionConfig" $ \o -> do
    pc <- parseJSON (Object o)
    ag <- o .: "alonzo"
    pure $ mkTransitionConfig pc ag
