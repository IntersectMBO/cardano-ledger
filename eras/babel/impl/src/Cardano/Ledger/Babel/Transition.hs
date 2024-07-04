{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Transition (
  BabelEraTransition (..),
  TransitionConfig (..),
  toBabelTransitionConfigPairs,
) where

import Cardano.Ledger.Babbage.Transition (TransitionConfig (BabbageTransitionConfig))
import Cardano.Ledger.Babel.Core (Era (..))
import Cardano.Ledger.Babel.Era
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..), toBabelGenesisPairs)
import Cardano.Ledger.Babel.Translation ()
import Cardano.Ledger.Babel.TxCert (Delegatee)
import Cardano.Ledger.Conway.Transition (toConwayTransitionConfigPairs)
import Cardano.Ledger.Conway.TxCert (getStakePoolDelegatee, getVoteDelegatee)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.DRep (DRepState)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  HasLedgerState (..),
  NewEpochState,
  certDStateL,
  certVStateL,
  dsUnifiedL,
  esLStateL,
  nesEsL,
  vsDRepsL,
 )
import Cardano.Ledger.Shelley.Transition
import Cardano.Ledger.UMap (UMElem (..), umElemsL)
import Control.Applicative (Alternative (..))
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
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class EraTransition era => BabelEraTransition era where
  tcDelegsL ::
    Lens'
      (TransitionConfig era)
      (ListMap (Credential 'Staking (EraCrypto era)) (Delegatee (EraCrypto era)))

  tcInitialDRepsL ::
    Lens'
      (TransitionConfig era)
      (ListMap (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))

  tcBabelGenesisL :: Lens' (TransitionConfig era) (BabelGenesis (EraCrypto era))

registerDRepsThenDelegs ::
  (BabelEraTransition era, HasLedgerState era) =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerDRepsThenDelegs cfg =
  -- NOTE: The order of registration does not matter.
  registerDelegs cfg . registerInitialDReps cfg

instance Crypto c => EraTransition (BabelEra c) where
  data TransitionConfig (BabelEra c) = BabelTransitionConfig
    { ctcBabelGenesis :: !(BabelGenesis c)
    , ctcBabbageTransitionConfig :: !(TransitionConfig (PreviousEra (BabelEra c)))
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = BabelTransitionConfig

  injectIntoTestState cfg =
    registerDRepsThenDelegs cfg
      . registerInitialFundsThenStaking cfg

  tcPreviousEraConfigL =
    lens ctcBabbageTransitionConfig (\ctc pc -> ctc {ctcBabbageTransitionConfig = pc})

  tcTranslationContextL =
    lens ctcBabelGenesis (\ctc ag -> ctc {ctcBabelGenesis = ag})

instance Crypto c => BabelEraTransition (BabelEra c) where
  tcBabelGenesisL = lens ctcBabelGenesis (\g x -> g {ctcBabelGenesis = x})

  tcDelegsL =
    protectMainnetLens "BabelDelegs" null $
      tcBabelGenesisL . lens cgDelegs (\g x -> g {cgDelegs = x})

  tcInitialDRepsL =
    protectMainnetLens "InitialDReps" null $
      tcBabelGenesisL . lens cgInitialDReps (\g x -> g {cgInitialDReps = x})

instance Crypto c => NoThunks (TransitionConfig (BabelEra c))

instance Crypto c => ToJSON (TransitionConfig (BabelEra c)) where
  toJSON = object . toBabelTransitionConfigPairs
  toEncoding = pairs . mconcat . toBabelTransitionConfigPairs

toBabelTransitionConfigPairs :: (KeyValue e a, Crypto c) => TransitionConfig (BabelEra c) -> [a]
toBabelTransitionConfigPairs babelConfig =
  toConwayTransitionConfigPairs conwayConfig
    ++ ["Babel" .= object (toBabelGenesisPairs (babelConfig ^. tcTranslationContextL))]
  where
    conwayConfig = babelConfig ^. tcPreviousEraConfigL

instance Crypto c => FromJSON (TransitionConfig (BabelEra c)) where
  parseJSON = withObject "BabelTransitionConfig" $ \o -> do
    pc <- parseJSON (Object o)
    ag <- o .: "Babel"
    pure $ mkTransitionConfig pc ag

registerInitialDReps ::
  (BabelEraTransition era, HasLedgerState era) =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerInitialDReps cfg =
  nesEsL . esLStateL . hlsCertStateL . certVStateL . vsDRepsL .~ drepsMap
  where
    drepsMap = ListMap.toMap $ cfg ^. tcInitialDRepsL

registerDelegs ::
  forall era.
  (BabelEraTransition era, HasLedgerState era) =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerDelegs cfg =
  nesEsL
    . esLStateL
    . hlsCertStateL
    . certDStateL
    . dsUnifiedL
    . umElemsL
    %~ \m -> ListMap.foldrWithKey (\(k, v) -> Map.insertWith joinUMElems k $ delegateeToUMElem v) m delegs
  where
    delegs = cfg ^. tcDelegsL
    delegateeToUMElem d =
      UMElem
        SNothing
        mempty
        (maybeToStrictMaybe $ getStakePoolDelegatee d)
        (maybeToStrictMaybe $ getVoteDelegatee d)
    joinUMElems
      (UMElem _ _ newStakePool newDRep)
      (UMElem rdp ptrs oldStakePool oldDRrep) =
        UMElem
          rdp
          ptrs
          (oldStakePool <|> newStakePool)
          (oldDRrep <|> newDRep)
