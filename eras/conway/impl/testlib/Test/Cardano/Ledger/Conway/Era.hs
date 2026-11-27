{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Era (
  module Test.Cardano.Ledger.Babbage.Era,
  ConwayEraTest (..),
  mkConwayTestAccountState,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..), EraPlutusTxInfo)
import Cardano.Ledger.BaseTypes (Inject, StrictMaybe (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (DRepGroup (..), PPGroups (..), StakePoolGroup (..), THKD (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxInfo (ConwayContextError)
import Cardano.Ledger.Plutus (CostModels, Language (..), emptyCostModels)
import Data.Coerce
import Data.Functor.Identity (Identity)
import Generic.Random (
  GArbitrary,
  GUniformWeight,
  SetGens,
  UnsizedOpts,
  genericArbitraryUG,
  (:+) (..),
 )
import Lens.Micro
import Paths_cardano_ledger_conway (getDataFileName)
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Common (Gen)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Examples (
  exampleConwayOnwardsEraPParams,
  exampleConwayOnwardsEraPParamsUpdate,
  exampleConwayTx,
 )
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( BabbageEraTest era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  , ConwayEraGov era
  , ConwayEraAccounts era
  , EraPlutusTxInfo PlutusV3 era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  ConwayEraTest era
  where
  -- | Generate `PParams` like `genericArbitraryU`, but with empty cost models.
  --
  -- Building a `CostModel` is expensive, because Plutus builds an evaluation context for it, both
  -- when generating and when decoding one. The governance state roundtrip tests use this for
  -- `PParams` in values whose type has a roundtrip test of its own, and for half of the
  -- `PParams` that a type decodes itself. See "Cost models in the governance state roundtrips" in
  -- "Test.Cardano.Ledger.Conway.Binary.RoundTrip".
  --
  -- The cost models field is generated empty from the start. Generating `PParams` with
  -- `arbitrary` and then setting `ppCostModelsL` would still build the cost models, because the
  -- fields of `PParams` are strict.
  --
  -- The default generates every other field with `arbitrary`, like the `Arbitrary` instances of
  -- the Conway and Dijkstra `PParams` do. An era whose `Arbitrary` instance has a special
  -- generator for some field should override this.
  genPParamsNoCostModels :: Gen (PParams era)
  default genPParamsNoCostModels ::
    ( GArbitrary (SetGens (Gen CostModelsField :+ ()) UnsizedOpts) (PParamsHKD Identity era)
    , GUniformWeight (PParamsHKD Identity era)
    ) =>
    Gen (PParams era)
  genPParamsNoCostModels =
    PParams <$> genericArbitraryUG (pure @Gen @CostModelsField (THKD emptyCostModels) :+ ())

  -- | Like `genPParamsNoCostModels`, but for `PParamsUpdate`: the cost models field is `SNothing`.
  -- The governance state roundtrip tests use this for the parameter changes in nested proposals.
  genPParamsUpdateNoCostModels :: Gen (PParamsUpdate era)
  default genPParamsUpdateNoCostModels ::
    ( GArbitrary
        (SetGens (Gen (CostModelsFieldHKD StrictMaybe) :+ ()) UnsizedOpts)
        (PParamsHKD StrictMaybe era)
    , GUniformWeight (PParamsHKD StrictMaybe era)
    ) =>
    Gen (PParamsUpdate era)
  genPParamsUpdateNoCostModels =
    PParamsUpdate
      <$> genericArbitraryUG (pure @Gen @(CostModelsFieldHKD StrictMaybe) (THKD SNothing) :+ ())

-- | The type of the cost models field in `PParams`
type CostModelsField = CostModelsFieldHKD Identity

-- | The type of the cost models field in `PParams` and `PParamsUpdate`
type CostModelsFieldHKD f = THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f CostModels

instance EraTest ConwayEra where
  type
    EraRulesWithFailures ConwayEra =
      '[ "BBODY"
       , "CERT"
       , "CERTS"
       , "DELEG"
       , "GOVCERT"
       , "GOV"
       , "LEDGER"
       , "LEDGERS"
       , -- , "MEMPOOL" -- TODO: Enable, once we are in Dijkstra era.
         "POOL"
       , "UTXO"
       , "UTXOS"
       , "UTXOW"
       ]

  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV3]

  mkTestAccountState _mPtr = mkConwayTestAccountState

  accountsFromAccountsMap = coerce

  mkEraFullPath = getDataFileName

  exampleTx = exampleConwayTx

  examplePParams = exampleConwayOnwardsEraPParams

  examplePParamsUpdate = exampleConwayOnwardsEraPParamsUpdate

instance ShelleyEraTest ConwayEra

instance AllegraEraTest ConwayEra

instance MaryEraTest ConwayEra

instance AlonzoEraTest ConwayEra

instance BabbageEraTest ConwayEra

instance ConwayEraTest ConwayEra

-- | similar to mkShelleyTestAccountState, but it ignores the mPtr, and doesn't
--   need to test that mDRep is SNothing, since this is the Conway Era, where DReps can be allocated.
mkConwayTestAccountState ::
  ConwayEraAccounts era =>
  CompactForm Coin ->
  Maybe (KeyHash StakePool) ->
  Maybe DRep ->
  AccountState era
mkConwayTestAccountState deposit mStakePool mDRep =
  mkConwayAccountState deposit
    & stakePoolDelegationAccountStateL .~ mStakePool
    & dRepDelegationAccountStateL .~ mDRep
