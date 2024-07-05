{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Spec.Pairs where

import Constrained.Base
import Constrained.Core
import Constrained.List
import Constrained.Univ
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck

-- HasSpec ----------------------------------------------------------------

cartesian ::
  (HasSpec fn a, HasSpec fn b) =>
  Specification fn a ->
  Specification fn b ->
  Specification fn (Prod a b)
cartesian (MemberSpec []) _ = MemberSpec []
cartesian _ (MemberSpec []) = MemberSpec []
cartesian (ErrorSpec es) _ = ErrorSpec es
cartesian _ (ErrorSpec es) = ErrorSpec es
cartesian s s' = typeSpec $ Cartesian s s'

data PairSpec fn a b = Cartesian (Specification fn a) (Specification fn b)

instance (Arbitrary (Specification fn a), Arbitrary (Specification fn b)) => Arbitrary (PairSpec fn a b) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  shrink (Cartesian a b) = uncurry Cartesian <$> shrink (a, b)

instance (HasSpec fn a, HasSpec fn b) => HasSpec fn (Prod a b) where
  type TypeSpec fn (Prod a b) = PairSpec fn a b

  type Prerequisites fn (Prod a b) = (HasSpec fn a, HasSpec fn b)

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = cartesian (a <> a') (b <> b')

  conformsTo (Prod a b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  genFromTypeSpec (Cartesian sa sb) = Prod <$> genFromSpecT sa <*> genFromSpecT sb

  shrinkWithTypeSpec (Cartesian sa sb) (Prod a b) =
    [Prod a' b | a' <- shrinkWithSpec sa a]
      ++ [Prod a b' | b' <- shrinkWithSpec sb b]

  toPreds x (Cartesian sf ss) =
    satisfies (app fstFn x) sf
      <> satisfies (app sndFn x) ss

  cardinalTypeSpec (Cartesian x y) = multSpecInt (cardinality x) (cardinality y)

deriving instance (HasSpec fn a, HasSpec fn b) => Show (PairSpec fn a b)

-- Functions for working on pairs -----------------------------------------

instance BaseUniverse fn => Functions (PairFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    Fst ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: PairFn fn '[Prod a b] a)
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Prod a b) ->
              cartesian spec TrueSpec
    Snd ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: PairFn fn '[Prod a b] b)
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Prod a b) ->
              cartesian TrueSpec spec
    _
      | SuspendedSpec v ps <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (v :-> ps)
    Pair
      | HOLE :? Value b :> Nil <- ctx ->
          let sameSnd ps = [a | Prod a b' <- ps, b == b']
           in case spec of
                TypeSpec (Cartesian sa sb) cant
                  | b `conformsToSpec` sb -> sa <> foldMap notEqualSpec (sameSnd cant)
                  -- TODO: better error message
                  | otherwise ->
                      ErrorSpec (NE.fromList ["propagateSpecFun (Pair a b) has conformance failure on b", show sb])
                MemberSpec es -> MemberSpec (sameSnd es)
      | Value a :! NilCtx HOLE <- ctx ->
          let sameFst ps = [b | Prod a' b <- ps, a == a']
           in case spec of
                TypeSpec (Cartesian sa sb) cant
                  | a `conformsToSpec` sa -> sb <> foldMap notEqualSpec (sameFst cant)
                  -- TODO: better error message
                  | otherwise ->
                      ErrorSpec (NE.fromList ["propagateSpecFun (Pair a b) has conformance failure on a", show sa])
                MemberSpec es -> MemberSpec (sameFst es)

  rewriteRules Fst ((pairView -> Just (x, _)) :> Nil) = Just x
  rewriteRules Snd ((pairView -> Just (_, y)) :> Nil) = Just y
  rewriteRules _ _ = Nothing

  mapTypeSpec f ts = case f of
    Fst | Cartesian s _ <- ts -> s
    Snd | Cartesian _ s <- ts -> s

pairView ::
  forall fn a b. Member (PairFn fn) fn => Term fn (Prod a b) -> Maybe (Term fn a, Term fn b)
pairView (App (extractFn @(PairFn fn) @fn -> Just Pair) (x :> y :> Nil)) = Just (x, y)
pairView _ = Nothing
