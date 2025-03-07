{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Conway.Experiment.LedgerTypes.Tests where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.CertState (ShelleyCertState)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.State (SnapShots (..), UTxO (..))
import Constrained.Experiment.API
import Data.Kind (Type)
import Data.Map (Map)
import Data.Typeable
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Cert (
  testConwayCert,
  testGenesisCert,
  testShelleyCert,
 )
import Test.Cardano.Ledger.Constrained.Conway.Experiment.LedgerTypes.Specs
import Test.Cardano.Ledger.Constrained.Conway.Experiment.LedgerTypes.WellFormed
import Test.Cardano.Ledger.Constrained.Conway.Experiment.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Experiment.ParametricSpec (irewardSpec)
import Test.Cardano.Ledger.Constrained.Conway.Experiment.WitnessUniverse
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (prettyA))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Gen,
  Property,
  counterexample,
  property,
  withMaxSuccess,
 )

-- ====================================================================================
-- Some Specifications are constrained by types (say 'x') that do not appear in the type being
-- specified. We use the strategy of passing (Term x) as inputs to those specifcations.
-- For example, the AccountState must have sufficient capacity to support the InstantaneousRewards
-- So we pass a (Term AccountState) as input to 'instantaneousRewardsSpec' which then
-- constrains both the AccountState Term and the 'instantaneousRewardsSpec' so that they are consistent.
-- In order to create tests, we need specifications that are fully applied, so we write combinators
-- to lift (Term a -> Spec b) functions to (Specification a -> Gen(Specification b))
-- The idea is to combine several Specifications to get a Gen(composed specifations)
-- For example (dstateSpec @Shelley !$! accountStateSpec !*! poolMapSpec)
-- is a (Gen (Specification (DState Shelley)))
-- If a Specification takes an actual PParams (not a (Term PParams)), like
-- lederstateSpec, we can combine it like this using the Functor <$>, rather than our !$!
-- (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
-- ====================================================================================

-- GenFromSpec fixed at
genConwayFn :: (HasCallStack, HasSpec a) => Specification a -> Gen a
genConwayFn = genFromSpec

-- Analagous to <$> except the function to be applied takes a (Term a -> t) instead of (a -> t)
infixr 6 !$!
(!$!) ::
  forall t a.
  HasSpec a =>
  (Term a -> t) -> Specification a -> Gen t
(!$!) bf specA = do a <- genFromSpec @a specA; pure (bf (lit a))

-- Analagous to <*> except the function to be applied takes a Gen (Term a -> t) instead of F (a -> t)
infixl 4 !*!
(!*!) ::
  forall t a.
  HasSpec a =>
  Gen (Term a -> t) -> Specification a -> Gen t
(!*!) gentf specA = do a <- genFromSpec @a specA; f <- gentf; pure (f (lit a))

poolMapSpec ::
  Specification (Map (KeyHash 'StakePool) PoolParams)
poolMapSpec = hasSize (rangeSize 8 8)

delegationsSpec ::
  Specification
    (Map (Credential 'Staking) (KeyHash 'StakePool))
delegationsSpec = (hasSize (rangeSize 8 12))

-- ====================================================================
-- HSpec tests
-- ===================================================================

soundSpec ::
  forall t. (HasSpec t, PrettyA t) => Gen (Specification t) -> Gen Property
soundSpec specGen = do
  spect <- specGen
  x <- genConwayFn @t spect
  pure $
    property $
      counterexample (show ("Does not meet spec\n" <> prettyA x)) (conformsToSpec x spect)

soundSpecWith ::
  forall t.
  (HasSpec t, PrettyA t) =>
  Int -> Gen (Specification t) -> SpecWith (Arg Property)
soundSpecWith n specx = it (show (typeRep (Proxy @t))) $ withMaxSuccess n $ property $ (soundSpec @t specx)

-- | A bunch of soundness tests on different LederTypes, all in the same Era.
--   The idea is to run this suite on every era.
specSuite ::
  forall (era :: Type).
  ( EraSpecLedger era
  , PrettyA (GovState era)
  , CertState era ~ ShelleyCertState era
  ) =>
  Int -> Spec
specSuite n = do
  let universe = genWitUniv @era 200

  soundSpecWith @(PState era) (5 * n) (pstateSpec @era <$> universe !*! epochNoSpec)

  soundSpecWith @(DState era)
    (5 * n)
    $ do
      univ <- genWitUniv @era 50
      (dstateSpec @era univ !$! accountStateSpec !*! poolMapSpec)

  soundSpecWith @(VState era)
    (10 * n)
    $ do
      univ <- genWitUniv @era 25
      ( vstateSpec @era univ
          !$! epochNoSpec
          !*! (goodDrep @era univ)
        )

  soundSpecWith @(CertState era)
    (5 * n)
    $ do
      univ <- genWitUniv @era 50
      (certStateSpec @era univ {- (lit drepRoleCredSet) -} !$! accountStateSpec !*! epochNoSpec)

  soundSpecWith @(UTxO era) (5 * n) (utxoSpecWit @era <$> universe !*! delegationsSpec)

  soundSpecWith @(UTxOState era) (2 * n) (utxoStateGen @era)

  soundSpecWith @(GovState era)
    (2 * n)
    (do x <- genFromSpec pparamsSpec; pure $ govStateSpec @era x)

  soundSpecWith @(LedgerState era)
    (2 * n)
    ( ledgerStateSpec
        <$> genConwayFn pparamsSpec
        <*> genWitUniv @era 100 !*! accountStateSpec !*! epochNoSpec
    )
  soundSpecWith @(EpochState era)
    (2 * n)
    (epochStateSpec @era <$> genConwayFn pparamsSpec <*> genWitUniv @era 50 !*! epochNoSpec)
  soundSpecWith @(NewEpochState era)
    (2 * n)
    (newEpochStateSpec @era <$> genConwayFn pparamsSpec <*> genWitUniv @era 50)

spec :: Spec
spec = do
  prop "Classify GenesisCert" (testGenesisCert @ShelleyEra)
  prop "Classify ShelleyCert" (testShelleyCert @BabbageEra)
  prop "Classify ConwayCert" testConwayCert
  describe "Soundness of WellFormed types from the Cardano Ledger: " $ do
    soundSpecWith @(ProtVer, ProtVer) 100 (pure protVersCanfollow)
    soundSpecWith @InstantaneousRewards
      20
      (irewardSpec @ShelleyEra (eraWitUniv @ShelleyEra 50) !$! accountStateSpec)
    soundSpecWith @SnapShots
      10
      (snapShotsSpec <$> ((lit . getMarkSnapShot) <$> (wff @(LedgerState ConwayEra) @ConwayEra)))
  specSuite @ShelleyEra 10
  specSuite @AllegraEra 10
  specSuite @MaryEra 10
  specSuite @AlonzoEra 10
  specSuite @BabbageEra 10
  specSuite @ConwayEra 10

utxoStateGen ::
  forall era.
  ( EraSpecLedger era
  , CertState era ~ ShelleyCertState era
  ) =>
  Gen (Specification (UTxOState era))
utxoStateGen =
  utxoStateSpec @era
    <$> genConwayFn @(PParams era) pparamsSpec
    <*> genWitUniv @era 25
    <*> (lit <$> wff @(ShelleyCertState era) @era) -- TODO: revisit once we have `ConwayCertState`
