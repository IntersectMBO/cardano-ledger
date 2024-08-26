-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Tests where

import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed

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
-- So we pass a (Term fn AccountState) as input to 'instantaneousRewardsSpec'
-- In order to create tests, we need specifications that are fully applied.
-- The idea is to combine several Specifications to get a Gen(composed specifations)
-- For example (dstateSpec @Shelley !$! accountStateSpec !*! poolMapSpec)
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

-- =============================================================
-- helper functions for examples and tests

{-
generateSpec :: forall a. HasSpec ConwayFn a => Specification ConwayFn a -> IO a
generateSpec specx = generate (genConwayFn specx)

specToGen :: forall t. HasSpec ConwayFn t => Specification ConwayFn t -> Gen t
specToGen = genFromSpec

genSpec :: HasSpec ConwayFn a => Specification ConwayFn a -> IO a
genSpec = generateSpec

ioTest :: forall t. (HasSpec ConwayFn t, PrettyA t) => Specification ConwayFn t -> IO Bool
ioTest specx = do
  t <- generateSpec @t specx
  putStrLn (show (prettyA t))
  pure (conformsToSpec t specx)
-}

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

soundSpecIt ::
  forall t.
  (HasSpec ConwayFn t, PrettyA t) => Int -> Gen (Specification ConwayFn t) -> SpecWith (Arg Property)
soundSpecIt n specx = it (show (typeRep (Proxy @t))) $ withMaxSuccess n $ property $ (soundSpec @t specx)

utxoStateGen :: forall era. LedgerEra era ConwayFn => Gen (Specification ConwayFn (UTxOState era))
utxoStateGen =
  utxoStateSpec @era
    <$> genConwayFn @(PParams era) pparamsSpec
    <*> (lit <$> wff @(CertState era) @era)

spec :: Spec
spec = do
  describe "Soundnes of WellFormed types from the Cardano Ledger: " $ do
    soundSpecIt @(ProtVer, ProtVer) 100 (pure protVersCanfollow)
    soundSpecIt @(InstantaneousRewards StandardCrypto)
      20
      (instantaneousRewardsSpec !$! accountStateSpec)

    soundSpecIt @(PState Shelley) 100 (pstateSpec !$! epochNoSpec)
    soundSpecIt @(PState Allegra) 100 (pstateSpec !$! epochNoSpec)
    soundSpecIt @(PState Mary) 100 (pstateSpec !$! epochNoSpec)
    soundSpecIt @(PState Alonzo) 100 (pstateSpec !$! epochNoSpec)
    soundSpecIt @(PState Babbage) 100 (pstateSpec !$! epochNoSpec)
    soundSpecIt @(PState Conway) 20 (pstateSpec !$! epochNoSpec)

    soundSpecIt @(DState Shelley) 50 (dstateSpec @Shelley !$! accountStateSpec !*! poolMapSpec)
    soundSpecIt @(DState Allegra) 50 (dstateSpec @Allegra !$! accountStateSpec !*! poolMapSpec)
    soundSpecIt @(DState Mary) 50 (dstateSpec @Mary !$! accountStateSpec !*! poolMapSpec)
    soundSpecIt @(DState Alonzo) 50 (dstateSpec @Alonzo !$! accountStateSpec !*! poolMapSpec)
    soundSpecIt @(DState Babbage) 50 (dstateSpec @Babbage !$! accountStateSpec !*! poolMapSpec)
    soundSpecIt @(DState Conway) 20 (dstateSpec @Conway !$! accountStateSpec !*! poolMapSpec)

    soundSpecIt @(VState Shelley) 100 (vstateSpec @_ @Shelley !$! epochNoSpec)
    soundSpecIt @(VState Allegra) 100 (vstateSpec @_ @Allegra !$! epochNoSpec)
    soundSpecIt @(VState Mary) 100 (vstateSpec @_ @Mary !$! epochNoSpec)
    soundSpecIt @(VState Alonzo) 100 (vstateSpec @_ @Alonzo !$! epochNoSpec)
    soundSpecIt @(VState Babbage) 100 (vstateSpec @_ @Babbage !$! epochNoSpec)
    soundSpecIt @(VState Conway) 100 (vstateSpec @_ @Conway !$! epochNoSpec)

    soundSpecIt @(CertState Shelley) 50 (certStateSpec !$! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(CertState Allegra) 50 (certStateSpec !$! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(CertState Mary) 50 (certStateSpec !$! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(CertState Alonzo) 50 (certStateSpec !$! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(CertState Babbage) 50 (certStateSpec !$! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(CertState Conway) 20 (certStateSpec !$! accountStateSpec !*! epochNoSpec)

    soundSpecIt @(UTxO Shelley) 50 (utxoSpec !$! delegationsSpec)
    soundSpecIt @(UTxO Allegra) 50 (utxoSpec !$! delegationsSpec)
    soundSpecIt @(UTxO Mary) 50 (utxoSpec !$! delegationsSpec)
    soundSpecIt @(UTxO Alonzo) 50 (utxoSpec !$! delegationsSpec)
    soundSpecIt @(UTxO Babbage) 50 (utxoSpec !$! delegationsSpec)
    soundSpecIt @(UTxO Conway) 50 (utxoSpec !$! delegationsSpec)

    soundSpecIt @(GovState Shelley)
      20
      (do x <- genFromSpec (pparamsSpec @ConwayFn); pure $ govStateSpec @Shelley x)
    soundSpecIt @(GovState Allegra)
      20
      (do x <- genFromSpec (pparamsSpec @ConwayFn @Allegra); pure $ govStateSpec @Allegra x)
    soundSpecIt @(GovState Mary)
      20
      (do x <- genFromSpec (pparamsSpec @ConwayFn); pure $ govStateSpec @Mary x)
    soundSpecIt @(GovState Alonzo)
      20
      (do x <- genFromSpec (pparamsSpec @ConwayFn); pure $ govStateSpec @Alonzo x)
    soundSpecIt @(GovState Babbage)
      20
      (do x <- genFromSpec (pparamsSpec @ConwayFn); pure $ govStateSpec @Babbage x)
    soundSpecIt @(GovState Conway)
      20
      (do x <- genFromSpec (pparamsSpec @ConwayFn); pure $ govStateSpec @Conway x)

    soundSpecIt @(UTxOState Shelley) 50 (utxoStateGen @Shelley)
    soundSpecIt @(UTxOState Allegra) 50 (utxoStateGen @Allegra)
    soundSpecIt @(UTxOState Mary) 50 (utxoStateGen @Mary)
    soundSpecIt @(UTxOState Alonzo) 50 (utxoStateGen @Alonzo)
    soundSpecIt @(UTxOState Babbage) 50 (utxoStateGen @Babbage)
    soundSpecIt @(UTxOState Conway) 20 (utxoStateGen @Conway)

    soundSpecIt @(LedgerState Shelley)
      50
      (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(LedgerState Allegra)
      50
      (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(LedgerState Mary)
      50
      (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(LedgerState Alonzo)
      50
      (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(LedgerState Babbage)
      50
      (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)
    soundSpecIt @(LedgerState Conway)
      20
      (ledgerStateSpec <$> genConwayFn pparamsSpec !*! accountStateSpec !*! epochNoSpec)

    soundSpecIt @(EpochState Shelley) 20 (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)
    soundSpecIt @(EpochState Allegra) 20 (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)
    soundSpecIt @(EpochState Mary) 20 (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)
    soundSpecIt @(EpochState Alonzo) 20 (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)
    soundSpecIt @(EpochState Babbage) 20 (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)
    soundSpecIt @(EpochState Conway) 20 (epochStateSpec <$> genConwayFn pparamsSpec !*! epochNoSpec)

    soundSpecIt @(NewEpochState Shelley) 20 (newEpochStateSpec <$> genConwayFn pparamsSpec)
    soundSpecIt @(NewEpochState Allegra) 20 (newEpochStateSpec <$> genConwayFn pparamsSpec)
    soundSpecIt @(NewEpochState Mary) 20 (newEpochStateSpec <$> genConwayFn pparamsSpec)
    soundSpecIt @(NewEpochState Alonzo) 20 (newEpochStateSpec <$> genConwayFn pparamsSpec)
    soundSpecIt @(NewEpochState Babbage) 20 (newEpochStateSpec <$> genConwayFn pparamsSpec)
    soundSpecIt @(NewEpochState Conway) 20 (newEpochStateSpec <$> genConwayFn pparamsSpec)

    soundSpecIt @(SnapShots StandardCrypto)
      20
      (snapShotsSpec <$> ((lit . getMarkSnapShot) <$> (wff @(LedgerState Conway) @Conway)))

{-

monadConformsToSpec :: forall fn a. HasSpec fn a => a -> Specification fn a -> Writer [String] Bool
monadConformsToSpec _ TrueSpec = pure True
monadConformsToSpec a (MemberSpec as) =
  if elem a as
    then pure True
    else tell [show a ++ " not an element of " ++ show as] >> pure False
monadConformsToSpec a (TypeSpec s cant) = do
  ans <- monadConformsTo @fn a s
  if notElem a cant && ans
    then pure True
    else tell [show a ++ " is an element of the cant set" ++ show cant] >> pure False
monadConformsToSpec a (SuspendedSpec v ps) =
  if checkPredPure (singletonEnv v a) ps
    then pure True
    else tell ["Suspended Spec " ++ show (SuspendedSpec v ps)] >> pure False
monadConformsToSpec _ (ErrorSpec es) = tell (NE.toList es) >> pure False
-}

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
  test @(SnapShot StandardCrypto) @Conway
  test @(SnapShots StandardCrypto) @Conway

  test @(GovEnv Conway) @Conway
  test @(ConwayGovState Conway) @Conway
