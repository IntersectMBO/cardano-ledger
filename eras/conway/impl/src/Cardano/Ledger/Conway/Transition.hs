{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Transition (
  ConwayEraTransition (..),
  TransitionConfig (..),
  toConwayTransitionConfigPairs,
) where

import Cardano.Ledger.Alonzo.Transition (toAlonzoTransitionConfigPairs)
import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Transition (TransitionConfig (BabbageTransitionConfig))
import Cardano.Ledger.Coin (compactCoinOrError)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..), toConwayGenesisPairs)
import Cardano.Ledger.Conway.Rules.Deleg (processDelegation)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesisStaking (..))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
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
import Data.ListMap (ListMap)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import GHC.Generics
import GHC.Stack
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class (EraTransition era, ConwayEraCertState era) => ConwayEraTransition era where
  tcDelegsL :: Lens' (TransitionConfig era) (ListMap (Credential 'Staking) Delegatee)

  tcInitialDRepsL :: Lens' (TransitionConfig era) (ListMap (Credential 'DRepRole) DRepState)

  tcConwayGenesisL :: Lens' (TransitionConfig era) ConwayGenesis

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

  injectIntoTestState = conwayRegisterInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens ctcBabbageTransitionConfig (\ctc pc -> ctc {ctcBabbageTransitionConfig = pc})

  tcTranslationContextL =
    lens ctcConwayGenesis (\ctc ag -> ctc {ctcConwayGenesis = ag})

instance ConwayEraTransition ConwayEra where
  tcConwayGenesisL = lens ctcConwayGenesis (\g x -> g {ctcConwayGenesis = x})

  tcDelegsL =
    protectMainnetLens "ConwayDelegs" null $
      tcConwayGenesisL . lens cgDelegs (\g x -> g {cgDelegs = x})

  tcInitialDRepsL =
    protectMainnetLens "InitialDReps" null $
      tcConwayGenesisL . lens cgInitialDReps (\g x -> g {cgInitialDReps = x})

instance NoThunks (TransitionConfig ConwayEra)

instance ToJSON (TransitionConfig ConwayEra) where
  toJSON = object . toConwayTransitionConfigPairs
  toEncoding = pairs . mconcat . toConwayTransitionConfigPairs

toConwayTransitionConfigPairs :: KeyValue e a => TransitionConfig ConwayEra -> [a]
toConwayTransitionConfigPairs conwayConfig =
  toAlonzoTransitionConfigPairs alonzoConfig
    ++ ["conway" .= object (toConwayGenesisPairs (conwayConfig ^. tcTranslationContextL))]
  where
    babbageConfig = conwayConfig ^. tcPreviousEraConfigL
    alonzoConfig = babbageConfig ^. tcPreviousEraConfigL

instance FromJSON (TransitionConfig ConwayEra) where
  parseJSON = withObject "ConwayTransitionConfig" $ \o -> do
    pc <- parseJSON (Object o)
    ag <- o .: "conway"
    pure $ mkTransitionConfig pc ag

conwayRegisterInitialFundsThenStaking ::
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
conwayRegisterInitialFundsThenStaking cfg =
  -- We must first register the initial funds, because the stake
  -- information depends on it.
  resetStakeDistribution
    . registerDRepsThenDelegs cfg
    . conwayRegisterInitialAccounts (cfg ^. tcInitialStakingL)
    . registerInitialStakePools (cfg ^. tcInitialStakingL)
    . registerInitialFunds cfg

-- | Register all staking credentials and apply delegations. Make sure StakePools that are bing
-- delegated to are already registered, which can be done with `registerInitialStakePools`.
conwayRegisterInitialAccounts ::
  forall era.
  (HasCallStack, EraTransition era, ConwayEraAccounts era) =>
  ShelleyGenesisStaking ->
  NewEpochState era ->
  NewEpochState era
conwayRegisterInitialAccounts ShelleyGenesisStaking {sgsStake} nes =
  nes
    & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL %~ \initAccounts ->
      foldr registerAndDelegate initAccounts $ ListMap.toList sgsStake
  where
    stakePools = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL
    deposit = compactCoinOrError $ nes ^. nesEsL . curPParamsEpochStateL . ppKeyDepositL
    registerAndDelegate (stakeKeyHash, stakePool) !accounts
      | stakePool `Map.member` stakePools =
          registerConwayAccount (KeyHashObj stakeKeyHash) deposit (Just (DelegStake stakePool)) accounts
      | otherwise =
          error $
            "Invariant of a delegation of "
              ++ show stakeKeyHash
              ++ " to an unregistered stake pool "
              ++ show stakePool
              ++ " is being violated."

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
