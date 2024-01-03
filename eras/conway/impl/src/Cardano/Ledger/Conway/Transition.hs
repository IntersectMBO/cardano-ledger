{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Transition (
  ConwayEraTransition (..),
  TransitionConfig (..),
  toConwayTransitionConfigPairs,
  registerDRepDelegs,
  registerInitialDReps,
) where

import Cardano.Ledger.Alonzo.Transition (toAlonzoTransitionConfigPairs)
import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Transition (TransitionConfig (BabbageTransitionConfig))
import Cardano.Ledger.Conway.Core (Era (..))
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..), toConwayGenesisPairs)
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.DRep (DRep, DRepState)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  certDStateL,
  certVStateL,
  dsUnifiedL,
  esLStateL,
  lsCertStateL,
  nesEsL,
  vsDRepsL,
 )
import Cardano.Ledger.Shelley.Transition
import qualified Cardano.Ledger.UMap as UMap
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
import Data.ListMap (ListMap)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class EraTransition era => ConwayEraTransition era where
  tcDRepDelegsL ::
    Lens'
      (TransitionConfig era)
      (ListMap (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)))

  tcInitialDRepsL ::
    Lens'
      (TransitionConfig era)
      (ListMap (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))

  tcConwayGenesisL :: Lens' (TransitionConfig era) (ConwayGenesis (EraCrypto era))

instance Crypto c => EraTransition (ConwayEra c) where
  data TransitionConfig (ConwayEra c) = ConwayTransitionConfig
    { ctcConwayGenesis :: !(ConwayGenesis c)
    , ctcBabbageTransitionConfig :: !(TransitionConfig (BabbageEra c))
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = ConwayTransitionConfig

  tcPreviousEraConfigL =
    lens ctcBabbageTransitionConfig (\ctc pc -> ctc {ctcBabbageTransitionConfig = pc})

  tcTranslationContextL =
    lens ctcConwayGenesis (\ctc ag -> ctc {ctcConwayGenesis = ag})

instance Crypto c => ConwayEraTransition (ConwayEra c) where
  tcConwayGenesisL = lens ctcConwayGenesis (\g x -> g {ctcConwayGenesis = x})

  tcDRepDelegsL =
    protectMainnetLens "DRepDelegs" null $
      tcConwayGenesisL . lens cgDRepDelegs (\g x -> g {cgDRepDelegs = x})

  tcInitialDRepsL =
    protectMainnetLens "InitialDReps" null $
      tcConwayGenesisL . lens cgInitialDReps (\g x -> g {cgInitialDReps = x})

instance Crypto c => NoThunks (TransitionConfig (ConwayEra c))

instance Crypto c => ToJSON (TransitionConfig (ConwayEra c)) where
  toJSON = object . toConwayTransitionConfigPairs
  toEncoding = pairs . mconcat . toConwayTransitionConfigPairs

toConwayTransitionConfigPairs :: (KeyValue e a, Crypto c) => TransitionConfig (ConwayEra c) -> [a]
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

registerInitialDReps ::
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerInitialDReps cfg =
  nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL .~ drepsMap
  where
    drepsMap = ListMap.toMap $ cfg ^. tcInitialDRepsL

registerDRepDelegs ::
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerDRepDelegs cfg =
  nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL %~ drepDelegs
  where
    drepDelegs umap =
      UMap.unUView $
        Map.foldrWithKey'
          UMap.insert'
          (UMap.DRepUView umap)
          (ListMap.toMap $ cfg ^. tcDRepDelegsL)
