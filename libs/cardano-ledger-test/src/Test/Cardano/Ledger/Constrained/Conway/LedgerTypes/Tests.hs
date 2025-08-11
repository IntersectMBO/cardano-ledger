{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Tests where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Constrained.API
import Data.Kind (Type)
import Data.Map (Map)
import Data.TreeDiff
import Data.Typeable
import Test.Cardano.Ledger.Constrained.Conway.Cert (
  testConwayCert,
  testShelleyCert,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances.Basic (prettyE)
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.Cardano.Ledger.Conway.Era
import Test.Hspec hiding (context)
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

delegationsSpec ::
  Specification
    (Map (Credential 'Staking) (KeyHash 'StakePool))
delegationsSpec = (hasSize (rangeSize 8 12))

poolRegSpec ::
  forall era. Era era => WitUniv era -> Specification (Map (KeyHash 'StakePool) PoolParams)
poolRegSpec univ = constrained $ \poolRegMap ->
  [ witness univ (dom_ poolRegMap)
  , witness univ (rng_ poolRegMap)
  , satisfies poolRegMap (hasSize (rangeSize 8 12))
  ]

-- ====================================================================
-- HSpec tests
-- ===================================================================

soundSpec ::
  forall t. (HasSpec t, ToExpr t) => Gen (Specification t) -> Gen Property
soundSpec specGen = do
  spect <- specGen
  x <- genConwayFn @t spect
  pure $
    property $
      counterexample (show ("Does not meet spec\n" <> prettyE x)) (conformsToSpec x spect)

soundSpecWith ::
  forall t.
  (HasSpec t, ToExpr t) =>
  Int -> Gen (Specification t) -> SpecWith (Arg Property)
soundSpecWith n specx = it (show (typeRep (Proxy @t))) $ withMaxSuccess n $ property $ (soundSpec @t specx)

-- | A bunch of soundness tests on different LederTypes, all in the same Era.
--   The idea is to run this suite on every era.
specSuite ::
  forall (era :: Type).
  ( era ~ ConwayEra
  , ShelleyEraTest era
  ) =>
  Int -> Spec
specSuite n = do
  let universe = genWitUniv @era 200

  soundSpecWith @(PState era) (5 * n) $ do
    univ <- genWitUniv @era 200
    pStateSpec @era univ !$! epochNoSpec

  soundSpecWith @(DState era) (5 * n) $ do
    univ <- genWitUniv @era 200
    context <- genCertContext @era univ
    poolreg <- genFromSpec (poolRegSpec univ)
    pure (conwayDStateSpec @era univ context (lit $ mkStakePoolState <$> poolreg))

  soundSpecWith @(VState era) (10 * n) $ do
    univ <- genWitUniv @era 200
    whodelegates <- genFromSpec (goodDrep @era univ)
    epoch <- genFromSpec epochNoSpec
    pure (vStateSpec @era univ (lit epoch) whodelegates)

  soundSpecWith @(CertState era) (5 * n) $ do
    univ <- genWitUniv @era 200
    context <- genCertContext @era univ
    epn <- genFromSpec epochNoSpec
    pure (conwayCertStateSpec univ context (lit epn))

  soundSpecWith @(UTxO era) (5 * n) (utxoSpecWit @era <$> universe !*! delegationsSpec)

  soundSpecWith @(UTxOState era) (2 * n) utxoStateGen

  soundSpecWith @(GovState era) (2 * n) $ do
    pp <- genFromSpec pparamsSpec
    pure (conwayGovStateSpec pp (testGovEnv pp))

  soundSpecWith @(LedgerState era) (2 * n) $ do
    pp <- genConwayFn pparamsSpec
    univ <- genWitUniv @era 200
    context <- genCertContext @era univ
    epn <- genFromSpec epochNoSpec
    pure (ledgerStateSpec pp univ context (lit epn))

  soundSpecWith @(EpochState era) (2 * n) $ do
    pp <- genConwayFn pparamsSpec
    univ <- genWitUniv @era 200
    context <- genCertContext @era univ
    epn <- genFromSpec epochNoSpec
    pure (epochStateSpec @era pp univ context (lit epn))

  soundSpecWith @(NewEpochState era) (2 * n) $ do
    pp <- genConwayFn pparamsSpec
    univ <- genWitUniv @era 200
    context <- genCertContext @era univ
    pure (newEpochStateSpec @era pp univ context)

spec :: Spec
spec = do
  prop "Classify ShelleyCert" (testShelleyCert @BabbageEra)
  prop "Classify ConwayCert" testConwayCert
  describe "Soundness of WellFormed types from the Cardano Ledger: " $ do
    soundSpecWith @(ProtVer, ProtVer) 100 (pure protVersCanfollow)

    soundSpecWith @SnapShots 10 $ do
      pp <- genConwayFn pparamsSpec
      ls <- lsX pp
      pure (snapShotsSpec (lit (getMarkSnapShot ls)))

  specSuite @ConwayEra 10

utxoStateGen :: Gen (Specification (UTxOState ConwayEra))
utxoStateGen =
  utxoStateSpec @ConwayEra
    <$> genConwayFn @(PParams ConwayEra) pparamsSpec
    <*> genWitUniv @ConwayEra 25
    <*> (lit <$> csX @ConwayEra)
