{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Tests where

import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Api
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..), calculatePoolDistr)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash ()
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  IncrementalStake (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  updateStakeDistribution,
 )
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.UMap (CompactForm (..))
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Constrained.Base (Pred (..), checkPredPure, fromList_, hasSize, rangeSize)
import Constrained.Env (singletonEnv)
import Control.Monad.Writer.Lazy
import Data.Default.Class (Default (def))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Generic.PrettyCore (
  PrettyA (prettyA),
  pcIRewards,
  pcSnapShotL,
  ppRecord,
 )
import Test.Hspec
import Test.QuickCheck (
  Gen,
  Property,
  arbitrary,
  counterexample,
  generate,
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
  (BaseUniverse fn, HasSpec fn a) =>
  (Term fn a -> t) -> Specification fn a -> Gen t
(!$!) bf specA = do a <- genFromSpec @fn @a specA; pure (bf (lit a))

-- Analagous to <*> except the function to be applied takes a Gen (Term fn a -> t) instead of F (a -> t)
infixl 4 !*!
(!*!) ::
  forall fn t a.
  (BaseUniverse fn, HasSpec fn a) =>
  Gen (Term fn a -> t) -> Specification fn a -> Gen t
(!*!) gentf specA = do a <- genFromSpec @fn @a specA; f <- gentf; pure (f (lit a))

poolMapSpec ::
  Specification ConwayFn (Map (KeyHash 'StakePool StandardCrypto) (PoolParams StandardCrypto))
poolMapSpec = hasSize (rangeSize 8 8)

delegationsSpec ::
  Specification
    ConwayFn
    (Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto))
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

utxoStateGen :: forall era. LedgerEra era ConwayFn => Gen (Specification ConwayFn (UTxOState era))
utxoStateGen =
  utxoStateSpec @era
    <$> genConwayFn @(PParams era) pparamsSpec
    <*> (lit <$> wff @(CertState era) @era)

-- | A bunch of soundness tests on different LederTypes, all in the same Era.
--   The idea is to run this suite on every era.
specSuite ::
  forall (era :: Type).
  ( EraTxOut era
  , EraCrypto era ~ StandardCrypto
  , LedgerEra era ConwayFn
  , IsNormalType (TxOut era)
  , PrettyA (GovState era)
  ) =>
  Int -> Spec
specSuite n = do
  soundSpecWith @(PState era) (5 * n) (pstateSpec !$! epochNoSpec)
  soundSpecWith @(DState era) (5 * n) (dstateSpec @era !$! accountStateSpec !*! poolMapSpec)
  soundSpecWith @(VState era) (10 * n) (vstateSpec @_ @era !$! epochNoSpec)
  soundSpecWith @(CertState era) (5 * n) (certStateSpec !$! accountStateSpec !*! epochNoSpec)
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
  describe "Soundness of WellFormed types from the Cardano Ledger: " $ do
    soundSpecWith @(ProtVer, ProtVer) 100 (pure protVersCanfollow)
    soundSpecWith @(InstantaneousRewards StandardCrypto)
      20
      (instantaneousRewardsSpec !$! accountStateSpec)
    soundSpecWith @(SnapShots StandardCrypto)
      10
      (snapShotsSpec <$> ((lit . getMarkSnapShot) <$> (wff @(LedgerState Conway) @Conway)))
  specSuite @Shelley 10
  specSuite @Allegra 10
  specSuite @Mary 10
  specSuite @Alonzo 10
  specSuite @Babbage 10
  specSuite @Conway 10

test :: forall t era. (WellFormed t era, PrettyA t) => IO ()
test = do
  ls <- generate $ wff @t @era
  putStrLn (show (typeRep (Proxy @t)) ++ " " ++ show (length (show (prettyA ls))))

testAll :: IO ()
testAll = do
  test @(PParams Shelley) @Shelley
  test @AccountState @Shelley
  test @(PState Shelley) @Shelley
  test @(DState Shelley) @Shelley
  test @(VState Shelley) @Shelley
  test @(CertState Shelley) @Shelley
  test @(UTxOState Shelley) @Shelley
  test @(LedgerState Shelley) @Shelley
  test @(EpochState Shelley) @Shelley
  test @(NewEpochState Shelley) @Shelley
  test @(ShelleyGovState Shelley) @Shelley
  test @(DRepState StandardCrypto) @Shelley
  test @(UTxO Shelley) @Shelley
  test @(SnapShot StandardCrypto) @Shelley
  test @(SnapShots StandardCrypto) @Shelley

  putStrLn ("\nAllega")
  test @(PParams Allegra) @Allegra
  test @AccountState @Allegra
  test @(PState Allegra) @Allegra
  test @(DState Allegra) @Allegra
  test @(VState Allegra) @Allegra
  test @(CertState Allegra) @Allegra
  test @(UTxOState Allegra) @Allegra
  test @(LedgerState Allegra) @Allegra
  test @(EpochState Allegra) @Allegra
  test @(NewEpochState Allegra) @Allegra
  test @(ShelleyGovState Allegra) @Allegra
  test @(DRepState StandardCrypto) @Allegra
  test @(UTxO Allegra) @Allegra
  test @(SnapShot StandardCrypto) @Allegra
  test @(SnapShots StandardCrypto) @Allegra

  putStrLn ("\nMary")
  test @(PParams Mary) @Mary
  test @AccountState @Mary
  test @(PState Mary) @Mary
  test @(DState Mary) @Mary
  test @(VState Mary) @Mary
  test @(CertState Mary) @Mary
  test @(UTxOState Mary) @Mary
  test @(LedgerState Mary) @Mary
  test @(EpochState Mary) @Mary
  test @(NewEpochState Mary) @Mary
  test @(ShelleyGovState Mary) @Mary
  test @(DRepState StandardCrypto) @Mary
  test @(UTxO Mary) @Mary
  test @(SnapShot StandardCrypto) @Mary
  test @(SnapShots StandardCrypto) @Mary

  putStrLn ("\nAlonzo")
  test @(PParams Alonzo) @Alonzo
  test @AccountState @Alonzo
  test @(PState Alonzo) @Alonzo
  test @(DState Alonzo) @Alonzo
  test @(VState Alonzo) @Alonzo
  test @(CertState Alonzo) @Alonzo
  test @(UTxOState Alonzo) @Alonzo
  test @(LedgerState Alonzo) @Alonzo
  test @(EpochState Alonzo) @Alonzo
  test @(NewEpochState Alonzo) @Alonzo
  test @(ShelleyGovState Alonzo) @Alonzo
  test @(DRepState StandardCrypto) @Alonzo
  test @(UTxO Alonzo) @Alonzo
  test @(SnapShot StandardCrypto) @Alonzo
  test @(SnapShots StandardCrypto) @Alonzo

  putStrLn ("\nBabbage")
  test @(PParams Babbage) @Babbage
  test @AccountState @Babbage
  test @(PState Babbage) @Babbage
  test @(DState Babbage) @Babbage
  test @(VState Babbage) @Babbage
  test @(CertState Babbage) @Babbage
  test @(UTxOState Babbage) @Babbage
  test @(LedgerState Babbage) @Babbage
  test @(EpochState Babbage) @Babbage
  test @(NewEpochState Babbage) @Babbage
  test @(ShelleyGovState Babbage) @Babbage
  test @(DRepState StandardCrypto) @Babbage
  test @(UTxO Babbage) @Babbage
  test @(SnapShot StandardCrypto) @Babbage
  test @(SnapShots StandardCrypto) @Babbage

  putStrLn ("\nConway")
  test @(PParams Conway) @Conway
  test @AccountState @Conway
  test @(PState Conway) @Conway
  test @(DState Conway) @Conway
  test @(VState Conway) @Conway
  test @(CertState Conway) @Conway
  test @(UTxOState Conway) @Conway
  test @(LedgerState Conway) @Conway
  test @(EpochState Conway) @Conway
  test @(NewEpochState Conway) @Conway
  test @(ConwayGovState Conway) @Conway
  test @(DRepState StandardCrypto) @Conway
  test @(UTxO Conway) @Conway
  test @(SnapShots StandardCrypto) @Conway
  test @(SnapShot StandardCrypto) @Conway

  test @(GovEnv Conway) @Conway
  test @(ConwayGovState Conway) @Conway
