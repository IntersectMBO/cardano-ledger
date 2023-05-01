{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Constrained.SpecClass where

import Cardano.Ledger.Coin (Coin, DeltaCoin)
import Cardano.Ledger.Era (Era)
import Data.Kind
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Word (Word64)
import Test.Cardano.Ledger.Common (Arbitrary (arbitrary), Gen)
import Test.Cardano.Ledger.Constrained.Classes (Adds, Sums)
import Test.Cardano.Ledger.Constrained.Monad (LiftT)
import Test.Cardano.Ledger.Constrained.Size (Size (..), genFromSize, runSize)
import Test.Cardano.Ledger.Constrained.Spec (
  ElemSpec,
  ListSpec,
  MapSpec,
  RelSpec,
  RngSpec,
  SetSpec,
  TT,
  genElemSpec,
  genFromElemSpec,
  genFromListSpec,
  genFromMapSpec,
  genFromRelSpec,
  genFromRngSpec,
  genFromSetSpec,
  genListSpec,
  genMapSpec,
  genRelSpec,
  genRngSpec,
  genSetSpec,
  genSize,
  runElemSpec,
  runListSpec,
  runMapSpec,
  runRelSpec,
  runRngSpec,
  runSetSpec,
  sizeForElemSpec,
  sizeForListSpec,
  sizeForMapSpec,
  sizeForRel,
  sizeForRng,
  sizeForSetSpec,
 )
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

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
  runS :: t -> spec -> Bool
  genS :: [String] -> Count spec -> Generators spec -> Gen spec
  sizeForS :: spec -> Size
  genFromS :: [String] -> Int -> Generators spec -> spec -> Gen t

instance Specification Size Int where
  type Count Size = ()
  type Generators Size = ()
  runS = runSize
  genS _ _ _ = genSize
  sizeForS = id
  genFromS _ _ _ = genFromSize

instance
  (Era era, Ord dom, HasRep dom, HasRep rng, HasRep c, Sums rng c, Adds rng) =>
  Specification (MapSpec era dom rng) (Map dom rng)
  where
  type Count (MapSpec era dom rng) = Int
  type Generators (MapSpec era dom rng) = (Gen dom, Gen rng)
  runS = runMapSpec
  genS _ count (genD, _) = genMapSpec genD hasRep hasRep hasRep count
  sizeForS = sizeForMapSpec
  genFromS msgs _count (genD, genR) spec = genFromMapSpec "genFromMapSpec" msgs genD genR spec

instance
  (Era era, Ord dom, HasRep dom) =>
  Specification (RelSpec era dom) (Set dom)
  where
  type Count (RelSpec era dom) = Int
  type Generators (RelSpec era dom) = Gen dom
  runS = runRelSpec
  genS msgs count g = genRelSpec msgs g hasRep count
  sizeForS = sizeForRel
  genFromS msgs count g spec = genFromRelSpec msgs g count spec

instance
  (Era era, HasRep rng, HasRep c, Sums rng c, Adds rng) =>
  Specification (RngSpec era rng) [rng]
  where
  type Count (RngSpec era rng) = Int
  type Generators (RngSpec era rng) = Gen rng
  runS = runRngSpec
  genS _msgs count g = genRngSpec g hasRep hasRep count
  sizeForS = sizeForRng
  genFromS msgs count g spec = genFromRngSpec msgs g count spec

instance
  (Era era, Ord a, HasRep a) =>
  Specification (SetSpec era a) (Set a)
  where
  type Count (SetSpec era a) = Int
  type Generators (SetSpec era a) = Gen a
  runS = runSetSpec
  genS msgs count g = genSetSpec msgs g hasRep count
  sizeForS = sizeForSetSpec
  genFromS msgs _count g spec = genFromSetSpec msgs g spec

instance
  (Era era, HasRep a, HasRep b, Sums a b, Adds a) =>
  Specification (ElemSpec era a) [a]
  where
  type Count (ElemSpec era a) = Size
  type Generators (ElemSpec era a) = Gen a
  runS = runElemSpec
  genS _msgs count _g = genElemSpec hasRep hasRep count
  sizeForS = sizeForElemSpec
  genFromS msgs count g spec = genFromElemSpec msgs g count spec

instance
  (Era era, HasRep a, HasRep b, Sums a b, Adds a) =>
  Specification (ListSpec era a) [a]
  where
  type Count (ListSpec era a) = Size
  type Generators (ListSpec era a) = Gen a
  runS = runListSpec
  genS _msgs count _g = genListSpec hasRep hasRep count
  sizeForS = sizeForListSpec
  genFromS msgs _count g spec = genFromListSpec msgs g spec

testSound :: forall spec t. Specification spec t => Count spec -> Generators spec -> Gen Bool
testSound c g = do
  spec <- genS @spec ["testSound"] c g
  ans <- genFromS @spec ["testSound"] 10 g spec
  pure $ runS ans spec

main :: IO ()
main =
  defaultMain $
    testGroup
      "Generic Specification class tests"
      [ testGroup
          "Generic testSound"
          [ testProperty "Size" $ testSound @Size @Int
          , testProperty "MapSpec" $ testSound @(MapSpec TT Int Word64) 10 (arbitrary, arbitrary)
          , testProperty "RelSpec" $ testSound @(RelSpec TT Int) 10 arbitrary
          , testProperty "RngSpec" $ testSound @(RngSpec TT Int) 10 arbitrary
          , testProperty "ListSpec" $ testSound @(ListSpec TT Int) SzAny arbitrary
          , testProperty "ElemSpec" $ testSound @(ElemSpec TT Int) SzAny arbitrary
          ]
      ]
