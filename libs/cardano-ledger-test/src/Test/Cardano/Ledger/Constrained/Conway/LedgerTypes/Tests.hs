{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Tests where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash ()
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Typeable
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Cert (
  testConwayCert,
  testGenesisCert,
  testShelleyCert,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
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
-- specified. We use the strategy of passing (Term fn x) as inputs to those specifcations.
-- For example, the AccountState must have sufficient capacity to support the InstantaneousRewards
-- So we pass a (Term fn AccountState) as input to 'instantaneousRewardsSpec' which then
-- constrains both the AccountState Term and the 'instantaneousRewardsSpec' so that they are consistent.
-- In order to create tests, we need specifications that are fully applied, so we write combinators
-- to lift (Term a -> Spec b) functions to (Specification a -> Gen(Specification b))
-- The idea is to combine several Specifications to get a Gen(composed specifations)
-- For example (dstateSpec @Shelley !$! accountStateSpec !*! poolMapSpec)
-- is a (Gen (Specification ConwayFn (DState Shelley)))
-- If a Specification takes an actual PParams (not a (Term fn PParams)), like
-- lederstateSpec, we can combine it like this using the Functor <$>, rather than our !$!
-- (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
-- ====================================================================================

-- GenFromSpec fixed at ConwayFn
genConwayFn :: (HasCallStack, HasSpec ConwayFn a) => Specification ConwayFn a -> Gen a
genConwayFn = genFromSpec @ConwayFn

-- Analagous to <$> except the function to be applied takes a (Term fn a -> t) instead of (a -> t)
infixr 6 !$!
(!$!) ::
  forall fn t a.
  HasSpec fn a =>
  (Term fn a -> t) -> Specification fn a -> Gen t
(!$!) bf specA = do a <- genFromSpec @fn @a specA; pure (bf (lit a))

-- Analagous to <*> except the function to be applied takes a Gen (Term fn a -> t) instead of F (a -> t)
infixl 4 !*!
(!*!) ::
  forall fn t a.
  HasSpec fn a =>
  Gen (Term fn a -> t) -> Specification fn a -> Gen t
(!*!) gentf specA = do a <- genFromSpec @fn @a specA; f <- gentf; pure (f (lit a))

poolMapSpec ::
  Specification ConwayFn (Map (KeyHash 'StakePool) PoolParams)
poolMapSpec = hasSize (rangeSize 8 8)

delegationsSpec ::
  Specification
    ConwayFn
    (Map (Credential 'Staking) (KeyHash 'StakePool))
delegationsSpec = (hasSize (rangeSize 8 12))

-- ====================================================================
-- HSpec tests
-- ===================================================================

soundSpec ::
  forall t. (HasSpec ConwayFn t, PrettyA t) => Gen (Specification ConwayFn t) -> Gen Property
soundSpec specGen = do
  spect <- specGen
  x <- genConwayFn @t spect
  pure $
    property $
      counterexample (show ("Does not meet spec\n" <> prettyA x)) (conformsToSpec x spect)

soundSpecWith ::
  forall t.
  (HasSpec ConwayFn t, PrettyA t) =>
  Int -> Gen (Specification ConwayFn t) -> SpecWith (Arg Property)
soundSpecWith n specx = it (show (typeRep (Proxy @t))) $ withMaxSuccess n $ property $ (soundSpec @t specx)

-- | A bunch of soundness tests on different LederTypes, all in the same Era.
--   The idea is to run this suite on every era.
specSuite ::
  forall (era :: Type).
  ( EraSpecLedger era ConwayFn
  , PrettyA (GovState era)
  ) =>
  Int -> Spec
specSuite n = do
  soundSpecWith @(PState era) (5 * n) (pstateSpec !$! epochNoSpec)
  soundSpecWith @(DState era) (5 * n) (dstateSpec @era !$! accountStateSpec !*! poolMapSpec)

  soundSpecWith @(VState era)
    (10 * n)
    ( vstateSpec @_ @era
        !$! epochNoSpec
        !*! ( TrueSpec @ConwayFn
                @( Map
                    (Credential 'DRepRole)
                    (Set.Set (Credential 'Staking))
                 )
            )
    )

  soundSpecWith @(CertState era)
    (5 * n)
    $ certStateSpec !$! TrueSpec !*! accountStateSpec !*! epochNoSpec
  soundSpecWith @(UTxO era) (5 * n) (utxoSpec !$! delegationsSpec)
  soundSpecWith @(GovState era)
    (2 * n)
    (do x <- genFromSpec (pparamsSpec @ConwayFn); pure $ govStateSpec @era x)
  soundSpecWith @(UTxOState era) (2 * n) (utxoStateGen @era)
  soundSpecWith @(LedgerState era)
    (2 * n)
    (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
  soundSpecWith @(EpochState era) (2 * n) (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)
  soundSpecWith @(NewEpochState era) (2 * n) (newEpochStateSpec <$> genConwayFn pparamsSpec)

spec :: Spec
spec = do
  prop "Classify GenesisCert" (testGenesisCert @ShelleyEra)
  prop "Classify ShelleyCert" (testShelleyCert @BabbageEra)
  prop "Classify ConwayCert" testConwayCert
  describe "Soundness of WellFormed types from the Cardano Ledger: " $ do
    soundSpecWith @(ProtVer, ProtVer) 100 (pure protVersCanfollow)
    soundSpecWith @InstantaneousRewards
      20
      (irewardSpec @ShelleyEra !$! accountStateSpec)
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
  forall era. EraSpecLedger era ConwayFn => Gen (Specification ConwayFn (UTxOState era))
utxoStateGen =
  utxoStateSpec @era
    <$> genConwayFn @(PParams era) pparamsSpec
    <*> (lit <$> wff @(CertState era) @era)
