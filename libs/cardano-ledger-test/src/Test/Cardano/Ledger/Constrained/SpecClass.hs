{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Constrained.SpecClass where

import Cardano.Ledger.Coin (Coin, DeltaCoin)
import Cardano.Ledger.Era (Era)
import Data.Kind
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Word (Word64)
import Test.Cardano.Ledger.Common (Arbitrary (arbitrary), Gen)
import Test.Cardano.Ledger.Constrained.Classes (Adds)
import Test.Cardano.Ledger.Constrained.Monad (LiftT)
import Test.Cardano.Ledger.Constrained.Size (Size (..), genFromSize, runSize)
import Test.Cardano.Ledger.Constrained.Spec (
  ElemSpec,
  ListSpec,
  MapSpec,
  PairSpec,
  RelSpec,
  RngSpec,
  SetSpec,
  SomeLens (SomeLens),
  TT,
  genElemSpec,
  genFromElemSpec,
  genFromListSpec,
  genFromMapSpec,
  genFromPairSpec,
  genFromRelSpec,
  genFromRngSpec,
  genFromSetSpec,
  genListSpec,
  genMapSpec,
  genPairSpec,
  genRelSpec,
  genRngSpec,
  genSetSpec,
  genSize,
  intDeltaCoinL,
  runElemSpec,
  runListSpec,
  runMapSpec,
  runPairSpec,
  runRelSpec,
  runRngSpec,
  runSetSpec,
  sizeForElemSpec,
  sizeForListSpec,
  sizeForMapSpec,
  sizeForPairSpec,
  sizeForRel,
  sizeForRng,
  sizeForSetSpec,
  word64CoinL,
 )
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- import Lens.Micro (Lens')

{-
class (Monoid spec, LiftT spec) => Specification spec t | spec -> t where
  type Count :: Type -> Type
  -- type Count Size = ()
  --      Count RelSpec = Int
  --      Count RngSpec = Int
  --      Count MapSpec = Int
  --      Count SetSpec = Int
  --      Count ElemSpec = Size
  --      Count ListSpec = Size
  --      Count AddsSpec = () -- This is not ready yet. TODO
  type Generators spec :: Type
  -- type Generators Size = ()
  -- type Generators (MapSpec era dom rng) = (Gen dom, Gen rng)
  -- type Generators (RelSpec era dom) = Gen dom
  -- ...
  runS :: t -> spec -> Bool
  genS :: [String] -> Count spec -> Generators spec -> Gen spec
  sizeForS :: spec -> Size
  genFromS :: [String] -> Int -> Generators spec -> spec -> Gen t

Notes:

genFromSize     ::                                                                 Size              -> Gen Int
genFromRelSpec  :: forall era t. Ord t => [String] -> Gen t            -> Int   -> RelSpec era t     -> Gen (Set t)
genFromRngSpec  :: forall era r.          [String] -> Gen r            -> Int   -> RngSpec era r     -> Gen [r]
genFromMapSpec  ::              String -> [String] -> Gen dom -> Gen w          -> MapSpec era dom w -> Gen (Map dom w)
genFromSetSpec  :: forall era a.          [String] -> Gen a                     -> SetSpec era a     -> Gen (Set a)
genFromElemSpec ::                        [String] -> Gen r            -> Int   -> ElemSpec era r    -> Gen [r]
genFromListSpec ::                        [String] -> Gen r                     -> ListSpec era r    -> Gen [r]
genFromAddsSpec ::                        [String]                              -> AddsSpec c        -> Gen Int

genConsistentSize
genConsistentRelSpec ::                          [String] -> Gen dom -> RelSpec era dom -> Gen (RelSpec era dom)
genConsistentRngSpec :: ( Adds w , Sums w c ) => Int -> Gen w -> Rep era w -> Rep era c -> Gen (RngSpec era w, RngSpec era w)
TODO
genConsisent*Spec for all the other Specs
-}

class Arbitrary t => HasRep t where
  hasRep :: Rep era t

instance HasRep Word64 where
  hasRep = Word64R
instance HasRep Int where
  hasRep = IntR
instance HasRep Coin where
  hasRep = CoinR
instance HasRep DeltaCoin where
  hasRep = DeltaCoinR

class (Monoid spec, LiftT spec) => Specification spec t | spec -> t where
  type Count spec :: Type
  type Generators spec :: Type
  type Reps spec :: Type
  type Lenses spec :: Type
  runS :: t -> spec -> Bool
  genS :: [String] -> Count spec -> Generators spec -> Reps spec -> Lenses spec -> Gen spec

  -- genConsistentS :: [String] -> Count spec -> Generators spec -> Reps spec -> spec -> Gen spec
  -- genConsistentS :: [String] -> Count spec -> Generators spec -> Reps spec -> gen (spec, spec)
  sizeForS :: spec -> Size
  genFromS :: [String] -> Int -> Generators spec -> spec -> Gen t

instance Specification Size Int where
  type Count Size = ()
  type Generators Size = ()
  type Reps Size = ()
  type Lenses Size = ()
  runS = runSize
  genS _ _ _ _ _ = genSize
  sizeForS = id
  genFromS _ _ _ = genFromSize

instance
  (Era era, Ord dom {- HasRep dom, HasRep rng, HasRep c, -}, Ord rng, Adds rng) =>
  Specification (MapSpec era dom rng) (Map dom rng)
  where
  type Count (MapSpec era dom rng) = Int
  type Generators (MapSpec era dom rng) = (Gen dom, Gen rng)
  type Reps (MapSpec era dom rng) = (Rep era dom, Rep era rng)
  type Lenses (MapSpec era dom rng) = SomeLens era rng
  runS = runMapSpec
  genS _ count (genD, _) (domRep, rngRep) sl = genMapSpec genD domRep rngRep sl count
  sizeForS = sizeForMapSpec
  genFromS msgs _count (genD, genR) spec = genFromMapSpec "genFromMapSpec" msgs genD genR spec

instance
  (Era era, Ord dom, Eq rng, HasRep dom, HasRep rng) =>
  Specification (PairSpec era dom rng) (Map dom rng)
  where
  type Count (PairSpec era dom rng) = Int
  type Generators (PairSpec era dom rng) = ()
  type Reps (PairSpec era dom rng) = ()
  type Lenses (PairSpec era dom rng) = ()
  runS = runPairSpec
  genS _msgs _count _g _ _ = genPairSpec hasRep hasRep
  sizeForS = sizeForPairSpec
  genFromS msgs _count () spec = genFromPairSpec msgs spec

instance
  (Era era, Ord dom, HasRep dom) =>
  Specification (RelSpec era dom) (Set dom)
  where
  type Count (RelSpec era dom) = Int
  type Generators (RelSpec era dom) = Gen dom
  type Reps (RelSpec era dom) = ()
  type Lenses (RelSpec era dom) = ()
  runS = runRelSpec
  genS msgs count g _ _ = genRelSpec msgs g hasRep count
  sizeForS = sizeForRel
  genFromS msgs count g spec = genFromRelSpec msgs g count spec

instance
  (Era era, Adds rng, Ord rng) =>
  Specification (RngSpec era rng) [rng]
  where
  type Count (RngSpec era rng) = Int
  type Generators (RngSpec era rng) = Gen rng
  type Reps (RngSpec era rng) = Rep era rng
  type Lenses (RngSpec era rng) = SomeLens era rng
  runS = runRngSpec
  genS _msgs count g r sl = genRngSpec g r sl count
  sizeForS = sizeForRng
  genFromS msgs count g spec = genFromRngSpec msgs g count spec

instance
  (Era era, Ord a, HasRep a) =>
  Specification (SetSpec era a) (Set a)
  where
  type Count (SetSpec era a) = Int
  type Generators (SetSpec era a) = Gen a
  type Reps (SetSpec era a) = ()
  type Lenses (SetSpec era a) = ()
  runS = runSetSpec
  genS msgs count g _ _ = genSetSpec msgs g hasRep count
  sizeForS = sizeForSetSpec
  genFromS msgs _count g spec = genFromSetSpec msgs g spec

instance
  (Era era, HasRep a, Adds a) =>
  Specification (ElemSpec era a) [a]
  where
  type Count (ElemSpec era a) = Size
  type Generators (ElemSpec era a) = Gen a
  type Reps (ElemSpec era a) = ()
  type Lenses (ElemSpec era a) = SomeLens era a
  runS = runElemSpec
  genS _msgs count _g _ sl = genElemSpec hasRep sl count
  sizeForS = sizeForElemSpec
  genFromS msgs count g spec = genFromElemSpec msgs g count spec

instance
  (Era era, HasRep a, Adds a) =>
  Specification (ListSpec era a) [a]
  where
  type Count (ListSpec era a) = Size
  type Generators (ListSpec era a) = Gen a
  type Reps (ListSpec era a) = ()
  type Lenses (ListSpec era a) = SomeLens era a
  runS = runListSpec
  genS _msgs count _g _ sl = genListSpec hasRep sl count
  sizeForS = sizeForListSpec
  genFromS msgs _count g spec = genFromListSpec msgs g spec

testSound :: forall spec t. (Specification spec t) => Lenses spec -> Reps spec -> Count spec -> Generators spec -> Gen Bool
testSound l r c g = do
  spec <- genS @spec ["testSound"] c g r l
  ans <- genFromS @spec ["testSound"] 10 g spec
  pure $ runS ans spec

main :: IO ()
main =
  defaultMain $
    testGroup
      "Generic Specification class tests"
      [ testGroup
          "Generic testSound"
          [ testProperty "Size" $ testSound @Size @Int ()
          , testProperty "MapSpec" $
              testSound @(MapSpec TT Int Word64) (SomeLens word64CoinL) (IntR, Word64R) 10 (arbitrary, arbitrary)
          , testProperty "PairSpec" $ testSound @(PairSpec TT Int Word64) () () 10
          , testProperty "RelSpec" $ testSound @(RelSpec TT Int) () () 10 arbitrary
          , testProperty "RngSpec" $
              testSound @(RngSpec TT Int) (SomeLens intDeltaCoinL) IntR 10 arbitrary
          , testProperty "ListSpec" $
              testSound @(ListSpec TT Int) (SomeLens intDeltaCoinL) () SzAny arbitrary
          , testProperty "ElemSpec" $
              testSound @(ElemSpec TT Int) (SomeLens intDeltaCoinL) () SzAny arbitrary
          ]
      ]
