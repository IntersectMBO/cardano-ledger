{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Transition (
  ConwayEraTransition (..),
  TransitionConfig (..),
  toConwayTransitionConfigPairs,
  registerDRepsThenDelegs,
) where

import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Transition (TransitionConfig (BabbageTransitionConfig))
import Cardano.Ledger.BaseTypes (toKeyValuePairs)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Rules.Deleg (processDelegation)
import Cardano.Ledger.Conway.State (ConwayEraCertState (..), DRepState, vsDRepsL)
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.TxCert (Delegatee)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.Transition
import Data.Aeson (KeyValue (..))
import Data.ListMap (ListMap)
import qualified Data.ListMap as ListMap
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class (EraTransition era, ConwayEraCertState era) => ConwayEraTransition era where
  tcConwayGenesisL :: Lens' (TransitionConfig era) ConwayGenesis
  default tcConwayGenesisL ::
    ConwayEraTransition (PreviousEra era) =>
    Lens' (TransitionConfig era) ConwayGenesis
  tcConwayGenesisL = tcPreviousEraConfigL . tcConwayGenesisL

registerDRepsThenDelegs ::
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerDRepsThenDelegs cfg =
  -- NOTE: The order of registration does not matter.
  registerDelegs cfg . registerInitialDReps cfg

instance EraTransition ConwayEra where
  data TransitionConfig ConwayEra = ConwayTransitionConfig
    { ctcConwayGenesis :: !ConwayGenesis
    , ctcBabbageTransitionConfig :: !(TransitionConfig BabbageEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = ConwayTransitionConfig

  injectIntoTestState cfg =
    registerDRepsThenDelegs cfg
      . registerInitialFundsThenStaking cfg

  tcPreviousEraConfigL =
    lens ctcBabbageTransitionConfig (\ctc pc -> ctc {ctcBabbageTransitionConfig = pc})

  tcTranslationContextL =
    lens ctcConwayGenesis (\ctc ag -> ctc {ctcConwayGenesis = ag})

instance ConwayEraTransition ConwayEra where
  tcConwayGenesisL = lens ctcConwayGenesis (\g x -> g {ctcConwayGenesis = x})

tcDelegsL ::
  ConwayEraTransition era => Lens' (TransitionConfig era) (ListMap (Credential 'Staking) Delegatee)
tcDelegsL =
  protectMainnetLens "ConwayDelegs" null $
    tcConwayGenesisL . lens cgDelegs (\g x -> g {cgDelegs = x})

tcInitialDRepsL ::
  ConwayEraTransition era => Lens' (TransitionConfig era) (ListMap (Credential 'DRepRole) DRepState)
tcInitialDRepsL =
  protectMainnetLens "InitialDReps" null $
    tcConwayGenesisL . lens cgInitialDReps (\g x -> g {cgInitialDReps = x})

instance NoThunks (TransitionConfig ConwayEra)

toConwayTransitionConfigPairs :: KeyValue e a => TransitionConfig ConwayEra -> [a]
toConwayTransitionConfigPairs = toKeyValuePairs
{-# DEPRECATED toConwayTransitionConfigPairs "In favor of `toKeyValuePairs`" #-}

registerInitialDReps ::
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerInitialDReps cfg =
  nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL .~ drepsMap
  where
    drepsMap = ListMap.toMap $ cfg ^. tcInitialDRepsL

registerDelegs ::
  forall era.
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerDelegs cfg =
  nesEsL . esLStateL . lsCertStateL
    %~ \certState -> ListMap.foldrWithKey (uncurry processDelegation) certState (cfg ^. tcDelegsL)
