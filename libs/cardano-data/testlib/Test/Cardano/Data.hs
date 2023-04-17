{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data (
  expectValidMap,
  genNonEmptyMap,
  propZero,
  propExtend,
  plusUnary,
  plusBinary,
  genMonoidRngD,
  genBinaryRngD,
) where

import Control.Monad
import Data.Incremental
import qualified Data.Map.Internal.Debug as Map
import qualified Data.Map.Strict as Map hiding (showTree)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

expectValidMap :: HasCallStack => (Ord k, Show k, Show a) => Map.Map k a -> Expectation
expectValidMap m =
  unless (Map.valid m) $
    expectationFailure $
      unlines
        [ "Interal strucutre of a map is invalid:"
        , "Keys are ordered: " ++ show (Map.ordered m)
        , "Tree is balanced: " ++ show (Map.balanced m)
        , "Sizes are valid: " ++ show (Map.validsize m)
        , Map.showTree m
        ]

genNonEmptyMap :: Ord k => Gen k -> Gen v -> Gen (Map.Map k v)
genNonEmptyMap genKey genVal = Map.fromList <$> listOf1 ((,) <$> genKey <*> genVal)

-- ======================================================================
-- Reusable components for the Incremental Lambda Calculus (ILC)
-- ======================================================================

-- =================================
-- Generic, reusable, Property tests

propZero :: forall t. (Eq t, Show t, ILC t) => Gen t -> Spec
propZero gent = prop "propZero" $ do
  x <- gent
  pure $ applyDiff @t x (zero @t) `shouldBe` x

type ILCProp t = (ILC t, Show t, Eq t, Show (Diff t))

propExtend :: forall t. (ILCProp t) => Gen t -> Gen (Diff t) -> Spec
propExtend gent genDiff = prop "propExtend" $ do
  x <- gent
  dx1 <- genDiff
  dx2 <- genDiff
  let ext = extend @t dx2 dx1
      appdif = applyDiff @t x dx1
  pure
    ( counterexample
        ( unlines
            [ "x= " ++ show x
            , "dx1= " ++ show dx1
            , "dx2= " ++ show dx2
            , "extend dx2 dx1= " ++ show ext
            , "applyDiff x dx1= " ++ show appdif
            , "lhs (applyDiff x (extend dx2 dx1))= " ++ show (applyDiff x ext)
            , "rhs (applyDiff (applyDiff x dx1) dx2)= " ++ show (applyDiff appdif dx2)
            ]
        )
        (applyDiff x (extend @t dx2 dx1) `shouldBe` applyDiff (applyDiff @t x dx1) dx2)
    )

-- | Test that f' is really the derivative of the unary function f.
plusUnary ::
  forall a b.
  (ILCProp a, ILCProp b) =>
  (a -> b) ->
  (a -> Diff a -> Diff b) ->
  a ->
  Diff a ->
  Property
plusUnary f f' a da =
  counterexample
    ( unlines
        [ "a = " ++ show a
        , "da = " ++ show da
        , "f a = " ++ show (f a)
        , "f' a da = " ++ show (f' a da)
        , "applyDiff (f a) (f' a da)) = " ++ show (applyDiff (f a) (f' a da))
        , "applyDiff a da = " ++ show (applyDiff a da)
        , "f (applyDiff a da) = " ++ show (f (applyDiff a da))
        ]
    )
    (f (applyDiff a da) `shouldBe` applyDiff (f a) (f' a da))

-- | Test that f' is really the derivative of the binary function f.
plusBinary ::
  forall a b c.
  (ILCProp a, ILCProp b, ILCProp c) =>
  (a -> b -> c) ->
  (a -> Diff a -> b -> Diff b -> Diff c) ->
  Gen a ->
  Gen (Diff a) ->
  Gen b ->
  Gen (Diff b) ->
  Gen Property
plusBinary f f' ga gda gb gdb = do
  m <- ga
  dm <- gda
  n <- gb
  dn <- gdb
  pure $
    counterexample
      ( unlines
          [ "m = " ++ show m
          , "dm = " ++ show dm
          , "n = " ++ show n
          , "dn = " ++ show dn
          , "f m n = " ++ show (f m n)
          , "f' m dm n dn = " ++ show (f' m dm n dn)
          , "applyDiff m dm = " ++ show (applyDiff m dm)
          , "applyDiff n dn = " ++ show (applyDiff n dn)
          , ""
          , "f (applyDiff m dm) (applyDiff n dn) = " ++ show (f (applyDiff m dm) (applyDiff n dn))
          , "applyDiff (f m n) (f' m dm n dn) = " ++ show (applyDiff (f m n) (f' m dm n dn))
          ]
      )
      (f (applyDiff m dm) (applyDiff n dn) `shouldBe` applyDiff (f m n) (f' m dm n dn))

-- ====================
-- reusable ILC Generators

instance Arbitrary t => Arbitrary (MonoidRngD t) where
  arbitrary = genMonoidRngD arbitrary

instance Arbitrary t => Arbitrary (BinaryRngD t) where
  arbitrary = genBinaryRngD arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Diff (Map.Map k v)) where
  arbitrary = Dn <$> arbitrary

instance (Ord k, Eq v, Monoid v, Arbitrary k, Arbitrary v) => Arbitrary (MonoidMap k v) where
  arbitrary = MM . Map.filter (/= mempty) <$> arbitrary

instance (Ord k, Arbitrary (Diff v), Arbitrary k, Arbitrary v) => (Arbitrary (Diff (MonoidMap k v))) where
  arbitrary = Dm <$> arbitrary

genMonoidRngD :: Gen t -> Gen (MonoidRngD t)
genMonoidRngD g = oneof [pure DeleteM, WriteM <$> g, CombM <$> g]

genBinaryRngD :: Gen t -> Gen (BinaryRngD t)
genBinaryRngD g = oneof [pure DeleteD, WriteD <$> g]

genTotal :: Gen t -> Gen (Total t)
genTotal gent = Total <$> gent

genDiffTotal :: Gen t -> Gen (Diff (Total t))
genDiffTotal gent = frequency [(1, pure Zero), (6, Total' <$> gent)]

instance Arbitrary t => Arbitrary (Total t) where
  arbitrary = genTotal arbitrary

instance Arbitrary t => Arbitrary (Diff (Total t)) where
  arbitrary = genDiffTotal arbitrary
