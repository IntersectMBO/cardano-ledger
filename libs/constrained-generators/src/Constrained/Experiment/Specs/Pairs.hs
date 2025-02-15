{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
import Constrained.List
import Data.Kind (Constraint, Type)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Typeable ((:~:) (Refl))
import GHC.TypeLits hiding (Text)
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

data ProdW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  PairW :: forall a b. ProdW () "pair_" '[a, b] (Prod a b)
  FstW :: forall a b. ProdW () "fst_" '[Prod a b] a
  SndW :: forall a b. ProdW () "snd_" '[Prod a b] b

deriving instance Eq (ProdW c s dom rng)

instance Show (ProdW c s dom rng) where
  show PairW = "pair_"
  show FstW = "fst_"
  show SndW = "snd_"

pairSem :: ProdW c sym dom rng -> FunTy dom rng
pairSem PairW = Prod
pairSem FstW = prodFst
pairSem SndW = prodSnd

instance Witness ProdW where
  semantics = pairSem

-- ========= FstW

instance (HasSpec a, HasSpec b) => FunSym () "fst_" ProdW '[Prod a b] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence FstW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App FstW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence FstW (HOLE :<> End)) (TypeSpec ts cant) =
    cartesian @a @b (TypeSpec ts cant) TrueSpec
  propagate (Context Evidence FstW (HOLE :<> End)) (MemberSpec es) =
    cartesian @a @b (MemberSpec es) TrueSpec
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for FstW with wrong number of arguments. " ++ show ctx)

  rewriteRules FstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules _ _ _ = Nothing

  mapTypeSpec f ts = case f of
    FstW | Cartesian s _ <- ts -> s

fst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
fst_ = appTerm FstW

-- ========= SndW

instance (HasSpec a, HasSpec b) => FunSym () "snd_" ProdW '[Prod a b] b where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence SndW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App SndW (v' :> Nil)) (v :-> ps)
  propagate (Context Evidence SndW (HOLE :<> End)) (TypeSpec ts cant) =
    cartesian @a @b TrueSpec (TypeSpec ts cant)
  propagate (Context Evidence SndW (HOLE :<> End)) (MemberSpec es) =
    cartesian @a @b TrueSpec (MemberSpec es)
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for SndW with wrong number of arguments. " ++ show ctx)

  rewriteRules SndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec f ts = case f of
    SndW | Cartesian _ s <- ts -> s

snd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
snd_ = appTerm SndW

-- ========= PairW
sameFst :: Eq a1 => a1 -> [Prod a1 a2] -> [a2]
sameFst a ps = [b | Prod a' b <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [Prod a2 a1] -> [a2]
sameSnd b ps = [a | Prod a b' <- ps, b == b']

instance (HasSpec a, HasSpec b) => FunSym () "pair_" ProdW '[a, b] (Prod a b) where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence PairW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App PairW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context Evidence PairW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App PairW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context Evidence PairW (a :|> HOLE :<> End)) (TypeSpec (Cartesian sa sb) cant)
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec (NE.fromList ["propagateSpecFun (pair_ a HOLE) has conformance failure on a", show sa])
  propagate (Context Evidence PairW (HOLE :<> b :<| End)) (TypeSpec (Cartesian sa sb) cant)
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec (NE.fromList ["propagateSpecFun (pair_ HOLE b) has conformance failure on b", show sb])
  propagate (Context Evidence PairW (a :|> HOLE :<> End)) (MemberSpec es) =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          pure
            ("propagateSpecFun (pair_ HOLE b) on (MemberSpec " ++ show es ++ " where b does not appear in bs.")
  propagate (Context Evidence PairW (HOLE :<> b :<| End)) (MemberSpec es) =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          pure
            ("propagateSpecFun (pair_ HOLE b) on (MemberSpec " ++ show es ++ " where b does not appear in bs.")
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for PairW with wrong number of arguments. " ++ show ctx)

pair_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (Prod a b)
pair_ = appTerm PairW
