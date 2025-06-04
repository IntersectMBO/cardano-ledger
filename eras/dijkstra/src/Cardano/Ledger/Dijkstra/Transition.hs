{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Transition (
  TransitionConfig (..),
  toDijkstraTransitionConfigPairs,
) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Transition (
  ConwayEraTransition,
  registerDRepsThenDelegs,
  toConwayTransitionConfigPairs,
 )
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Genesis
import Cardano.Ledger.Dijkstra.Translation ()
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

instance EraTransition DijkstraEra where
  data TransitionConfig DijkstraEra = DijkstraTransitionConfig
    { ctcDijkstraGenesis :: !DijkstraGenesis
    , ctcConwayTransitionConfig :: !(TransitionConfig ConwayEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = DijkstraTransitionConfig

  injectIntoTestState cfg =
    registerDRepsThenDelegs cfg
      . registerInitialFundsThenStaking cfg

  tcPreviousEraConfigL =
    lens ctcConwayTransitionConfig (\ctc pc -> ctc {ctcConwayTransitionConfig = pc})

  tcTranslationContextL =
    lens ctcDijkstraGenesis (\ctc ag -> ctc {ctcDijkstraGenesis = ag})

instance ConwayEraTransition DijkstraEra

instance NoThunks (TransitionConfig DijkstraEra)

instance ToJSON (TransitionConfig DijkstraEra) where
  toJSON = object . toDijkstraTransitionConfigPairs
  toEncoding = pairs . mconcat . toDijkstraTransitionConfigPairs

toDijkstraTransitionConfigPairs :: KeyValue e a => TransitionConfig DijkstraEra -> [a]
toDijkstraTransitionConfigPairs dijkstraConfig =
  toConwayTransitionConfigPairs conwayConfig
    ++ ["dijkstra" .= object (toDijkstraGenesisPairs (dijkstraConfig ^. tcTranslationContextL))]
  where
    conwayConfig = dijkstraConfig ^. tcPreviousEraConfigL

instance FromJSON (TransitionConfig DijkstraEra) where
  parseJSON = withObject "DijkstraTransitionConfig" $ \o -> do
    pc <- parseJSON (Object o)
    ag <- o .: "dijkstra"
    pure $ mkTransitionConfig pc ag
