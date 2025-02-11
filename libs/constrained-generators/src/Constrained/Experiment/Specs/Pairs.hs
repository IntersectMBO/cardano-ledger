{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Experiment.Specs.Pairs where

import Constrained.Core (Evidence (..))
import Constrained.Experiment.Base
import Constrained.Experiment.Conformance
import Constrained.Experiment.Generic
import Constrained.Experiment.NumSpec (cardinality)
import Constrained.Experiment.TheKnot
import Constrained.Experiment.Witness
import Constrained.List
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Typeable ((:~:) (Refl))
import Prettyprinter
import Test.QuickCheck hiding (Witness, witness)

-- HasSpec ----------------------------------------------------------------

cartesian ::
  forall a b.
  (HasSpec a, HasSpec b) =>
  Specification a ->
  Specification b ->
  Specification (Prod a b)
cartesian (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
cartesian (ErrorSpec es) _ = ErrorSpec (NE.cons "cartesian left" es)
cartesian _ (ErrorSpec es) = ErrorSpec (NE.cons "cartesian right" es)
cartesian s s' = typeSpec $ Cartesian s s'

data PairSpec a b = Cartesian (Specification a) (Specification b)

instance (Arbitrary (Specification a), Arbitrary (Specification b)) => Arbitrary (PairSpec a b) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  shrink (Cartesian a b) = uncurry Cartesian <$> shrink (a, b)

instance (HasSpec a, HasSpec b) => HasSpec (Prod a b) where
  type TypeSpec (Prod a b) = PairSpec a b

  type Prerequisites (Prod a b) = (HasSpec a, HasSpec b)

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = cartesian (a <> a') (b <> b')

  conformsTo (Prod a b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  genFromTypeSpec (Cartesian sa sb) = Prod <$> genFromSpecT sa <*> genFromSpecT sb

  shrinkWithTypeSpec (Cartesian sa sb) (Prod a b) =
    [Prod a' b | a' <- shrinkWithSpec sa a]
      ++ [Prod a b' | b' <- shrinkWithSpec sb b]

  toPreds x (Cartesian sf ss) =
    satisfies (fst_ x) sf
      <> satisfies (snd_ x) ss

  cardinalTypeSpec (Cartesian x y) = (cardinality x) + (cardinality y)

  typeSpecHasError (Cartesian x y) =
    case (isErrorLike x, isErrorLike y) of
      (False, False) -> Nothing
      (True, False) -> Just $ errorLikeMessage x
      (False, True) -> Just $ errorLikeMessage y
      (True, True) -> Just $ (errorLikeMessage x <> errorLikeMessage y)

  alternateShow (Cartesian left right@(TypeSpec r [])) =
    case alternateShow @b r of
      (BinaryShow "Cartesian" ps) -> BinaryShow "Cartesian" ("," <+> viaShow left : ps)
      (BinaryShow "SumSpec" ps) -> BinaryShow "Cartesian" ("," <+> viaShow left : ["SumSpec" /> vsep ps])
      _ -> BinaryShow "Cartesian" ["," <+> viaShow left, "," <+> viaShow right]
  alternateShow (Cartesian left right) = BinaryShow "Cartesian" ["," <+> viaShow left, "," <+> viaShow right]

instance (HasSpec a, HasSpec b) => Show (PairSpec a b) where
  show pair@(Cartesian l r) = case alternateShow @(Prod a b) pair of
    (BinaryShow "Cartesian" ps) -> show $ parens ("Cartesian" /> vsep ps)
    _ -> "(Cartesian " ++ "(" ++ show l ++ ") (" ++ show r ++ "))"

-- ==================================================================================
-- FunSym instances for PairW, FstW, SndW

pairView :: forall a b. (HasSpec a, HasSpec b) => Term (Prod a b) -> Maybe (Term a, Term b)
pairView (App (sameFunSym $ PairW @a @b -> Just (_, Refl, Refl, Refl, Refl, Refl)) (x :> y :> Nil)) = Just (x, y)
pairView _ = Nothing

-- ========= FstW

instance (HasSpec a, HasSpec b) => FunSym () "fst_" BaseW '[Prod a b] a where
  simplepropagate (Context Evidence FstW (HOLE :<> End)) (spec :: Specification a) = Right $ (cartesian @a @b spec TrueSpec)
  simplepropagate ctx _ = Left (NE.fromList ["FstW[fst_]", "Unreachable context, wrong number of args", show ctx])

  rewriteRules FstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules _ _ _ = Nothing

  mapTypeSpec f ts = case f of
    FstW | Cartesian s _ <- ts -> s

fst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
fst_ = appTerm FstW

-- ========= SndW

instance (HasSpec a, HasSpec b) => FunSym () "snd_" BaseW '[Prod a b] b where
  simplepropagate (Context _ SndW (HOLE :<> End)) spec = Right $ (cartesian @a @b TrueSpec spec)
  simplepropagate ctx _ = Left (NE.fromList ["SndW[fst_]", "Unreachable context, wrong number of args", show ctx])

  rewriteRules SndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec f ts = case f of
    SndW | Cartesian _ s <- ts -> s

snd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
snd_ = appTerm SndW

-- ========= PairW

instance (HasSpec a, HasSpec b) => FunSym () "pair_" BaseW '[a, b] (Prod a b) where
  simplepropagate ctx@(Context _ PairW (a :|> HOLE :<> End)) spec =
    let sameFst ps = [b | Prod a' b <- ps, a == a']
     in case spec of
          TypeSpec (Cartesian sa sb) cant
            | a `conformsToSpec` sa -> Right $ sb <> foldMap notEqualSpec (sameFst cant)
            | otherwise ->
                Left $ (NE.fromList ["propagateSpecFun (pair_ a HOLE) has conformance failure on a", show sa])
          MemberSpec es -> case (nub (sameFst (NE.toList es))) of
            (w : ws) -> Right $ MemberSpec (w :| ws)
            [] ->
              Left $
                pure
                  ("propagateSpecFun (pair_ a HOLE) on (MemberSpec " ++ show es ++ " where a does not appear in as.")
          _ -> Left (NE.fromList ["PairW[pair_]", "Unreachable context, wrong number of args", show ctx])
  simplepropagate ctx@(Context _ PairW (HOLE :<> b :<| End)) spec =
    let sameSnd ps = [a | Prod a b' <- ps, b == b']
     in case spec of
          TypeSpec (Cartesian sa sb) cant
            | b `conformsToSpec` sb -> Right $ sa <> foldMap notEqualSpec (sameSnd cant)
            | otherwise ->
                Left $ (NE.fromList ["propagateSpecFun (pair_ HOLE b) has conformance failure on b", show sb])
          MemberSpec es -> case (nub (sameSnd (NE.toList es))) of
            (w : ws) -> Right $ MemberSpec (w :| ws)
            [] ->
              Left $
                pure
                  ("propagateSpecFun (pair_ HOLE b) on (MemberSpec " ++ show es ++ " where a does not appear in as.")
          _ -> Left (NE.fromList ["PairW[pair_]", "Unreachable context, wrong number of args", show ctx])
  simplepropagate ctx _ = Left (NE.fromList ["PairW[pair_]", "Unreachable context, wrong number of args", show ctx])

pair_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (Prod a b)
pair_ = appTerm PairW
