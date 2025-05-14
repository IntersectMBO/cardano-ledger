{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- HasSpec instances for known types such as (a,b,c), (a,b,c,d) i.e. tuples.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Minimal.Tuple where

import Constrained.GenT
import Constrained.List hiding (ListCtx)
import Data.Kind
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import Test.Minimal.Base
import Test.Minimal.Model
import Test.Minimal.Syntax

-- import Data.Coerce
-- import Data.Typeable

-- =======================================================
-- Experiment to see if we can build tuples, using only the binary tuple
-- as the base case.  The only compilcated part is the `combineSpec` which
-- uses `guardTypeSpec` to merge the simple sub cases on smaller tuples.

instance (HasSpec a, HasSpec b, HasSpec c) => HasSpec (a, b, c) where
  type TypeSpec (a, b, c) = (Spec a, Spec (b, c))

  anySpec = (mempty @(Spec a), mempty @(Spec (b, c)))

  combineSpec (a, b) (a', b') = guardTypeSpec (a <> a', b <> b')

  conformsTo (a, b, c) (sa, sbc) = conformsToSpec a sa && conformsToSpec (b, c) sbc

  guardTypeSpec (a, bc) =
    handleErrors
      a
      bc
      ( \x y -> case (x :: Spec a, y :: Spec (b, c)) of
          (MemberSpec xs, MemberSpec ys) ->
            -- Given two MemberSpec, build one MemberSpec, by joining all combinations
            MemberSpec
              ( NE.fromList
                  [ (a', b', c')
                  | a' <- NE.toList xs
                  , (b', c') <- NE.toList ys
                  ]
              )
          (specA, specBC) -> constrained $ \p -> And [satisfies (head3_ p) specA, satisfies (tail3_ p) specBC]
      )

  genFromTypeSpec (a, bc) = f <$> genFromSpecT a <*> genFromSpecT bc
    where
      f a' (b', c') = (a', b', c')

  toPreds x (a, bc) = satisfies (head3_ x) a <> satisfies (tail3_ x) bc

head3_ :: All HasSpec '[a, b, c] => Term (a, b, c) -> Term a
head3_ x = App Head3W (x :> Nil)

tail3_ :: All HasSpec '[a, b, c] => Term (a, b, c) -> Term (b, c)
tail3_ x = App Tail3W (x :> Nil)

-- =======================================================

instance (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => HasSpec (a, b, c, d) where
  type TypeSpec (a, b, c, d) = (Spec a, Spec (b, c, d))

  anySpec = (mempty @(Spec a), mempty @(Spec (b, c, d)))

  combineSpec (a, bcd) (a', bcd') = guardTypeSpec (a <> a', bcd <> bcd')

  conformsTo (a, b, c, d) (sA, sBCD) = conformsToSpec a sA && conformsToSpec (b, c, d) sBCD

  guardTypeSpec (a, bcd) =
    handleErrors
      a
      bcd
      ( \x y -> case (x, y) of
          (MemberSpec xs, MemberSpec ys) ->
            MemberSpec
              ( NE.fromList
                  [ (s, b, c, d)
                  | s <- NE.toList xs
                  , (b, c, d) <- NE.toList ys
                  ]
              )
          (specA, specBCD) -> constrained $ \ps -> And [satisfies (head4_ ps) specA, satisfies (tail4_ ps) specBCD]
      )

  genFromTypeSpec (a, bcd) = f <$> genFromSpecT a <*> genFromSpecT bcd
    where
      f a' (b, c, d) = (a', b, c, d)

  toPreds x (a, bcd) = satisfies (head4_ x) a <> satisfies (tail4_ x) bcd

head4_ :: All HasSpec '[a, b, c, d] => Term (a, b, c, d) -> Term a
head4_ x = App Head4W (x :> Nil)

tail4_ :: All HasSpec '[a, b, c, d] => Term (a, b, c, d) -> Term (b, c, d)
tail4_ x = App Tail4W (x :> Nil)

-- ======================================================================
-- We need some function symbols, to break Bigger tuples into sub-tuples

data TupleSym (ds :: [Type]) r where
  Head3W :: All HasSpec '[a, b, c] => TupleSym '[(a, b, c)] a
  Tail3W :: All HasSpec '[a, b, c] => TupleSym '[(a, b, c)] (b, c)
  Head4W :: All HasSpec '[a, b, c, d] => TupleSym '[(a, b, c, d)] a
  Tail4W :: All HasSpec '[a, b, c, d] => TupleSym '[(a, b, c, d)] (b, c, d)

deriving instance Eq (TupleSym ds r)

instance Show (TupleSym ds r) where show = name

instance Syntax TupleSym where
  inFix _ = False
  name Head3W = "head3_"
  name Tail3W = "tail3_"
  name Head4W = "head4_"
  name Tail4W = "tail4_"

instance Semantics TupleSym where
  semantics Head3W = \(a, _b, _c) -> a
  semantics Tail3W = \(_a, b, c) -> (b, c)
  semantics Head4W = \(a, _b, _c, _d) -> a
  semantics Tail4W = \(_a, b, c, d) -> (b, c, d)

instance Logic TupleSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate symW (Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App symW (v' :> Nil)) (v :-> ps)
  propagate Head3W (Unary HOLE) specA = anyTail3 specA
  propagate Tail3W (Unary HOLE) specBC = anyHead3 specBC
  propagate Head4W (Unary HOLE) specA = anyTail4 specA
  propagate Tail4W (Unary HOLE) specBCD = anyHead4 specBCD

anyHead3 :: forall a b c. (HasSpec a, HasSpec b, HasSpec c) => Spec (b, c) -> Spec (a, b, c)
anyHead3 specBC = typeSpec @(a, b, c) (mempty @(Spec a), specBC)

anyTail3 :: forall a b c. (HasSpec a, HasSpec b, HasSpec c) => Spec a -> Spec (a, b, c)
anyTail3 specA = typeSpec (specA, mempty @(Spec (b, c)))

anyHead4 ::
  forall a b c d. (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => Spec (b, c, d) -> Spec (a, b, c, d)
anyHead4 specBCD = typeSpec (mempty @(Spec a), specBCD)

anyTail4 ::
  forall a b c d. (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => Spec a -> Spec (a, b, c, d)
anyTail4 specA = typeSpec (specA, mempty @(Spec (b, c, d)))

-- ======================================================================
-- The Match class, with function `match` makes using all tuples uniform
-- For any n-tuple, supply `match` with an n-ary function to bring into
-- scope 'n' variables with type Term, which can be used to make Pred
-- Note how the binary case is the inductive step, and the others just
-- call the `match` with one less item in the tuple.

class Match t (ts :: [Type]) | t -> ts where
  match :: All HasSpec ts => Term t -> FunTy (MapList Term ts) Pred -> Pred

-- Base case where binary tuple.
instance Match (a, b) '[a, b] where
  match ab f =
    Let
      (fst_ ab)
      ( bind $ \ft ->
          Let (snd_ ab) (bind $ \st -> f ft st)
      )

-- Inductive case for ternary tuple, calls 'match' for binary tuple.
instance Match (a, b, c) '[a, b, c] where
  match abc f =
    Let
      (head3_ abc)
      ( bind $ \a ->
          Let
            (tail3_ abc)
            (bind $ \bc -> match @(b, c) bc (f a))
      )

-- Inductive case for quadary tuple, calls 'match' for ternary tuple.
instance Match (a, b, c, d) '[a, b, c, d] where
  match abcd f =
    Let
      (head4_ abcd)
      ( bind $ \a ->
          Let
            (tail4_ abcd)
            (bind $ \bcd -> match @(b, c, d) bcd (f a))
      )

-- =========================================================
-- Here are some examples, Notice how the arity of the function
-- passed to `match` changes as the width of the tuples changes.

spec2 :: Spec (Integer, Integer)
spec2 = constrained $ \x ->
  match x $ \a b -> Assert $ a <=. b

spec3 :: Spec (Integer, Integer, Integer)
spec3 = constrained $ \v4 ->
  match v4 $ \v3 v1 v0 -> And [Assert $ v3 <=. v1, Assert $ v1 <=. v0]

spec4 :: Spec (Integer, Integer, Integer, Integer)
spec4 = constrained $ \x ->
  match x $ \a b c d -> And [Assert $ a <=. b, Assert $ b <=. c, Assert $ c <=. d]

-- ========================================================

{-
class TypeList ts where
  uncurryList :: FunTy (MapList f ts) r -> List f ts -> r
  uncurryList_ :: (forall a. f a -> a)
                  -> FunTy ts r -> List f ts -> r
  curryList :: (List f ts -> r) -> FunTy (MapList f ts) r
  curryList_ :: (forall a. a -> f a)
                -> (List f ts -> r) -> FunTy ts r
  unfoldList :: (forall a (as :: [*]). List f as -> f a) -> List f ts
-}

-- | Fold over a (List Term ts) with 'getTermsize' which consumes a Term component for each element of the list
ex1 :: Maybe Int
ex1 = uncurryList getTermsize1 args1
  where
    args1 :: List Term '[Int, Bool, String]
    args1 = Lit 5 :> Lit True :> Lit "abc" :> Nil
    getTermsize1 :: Term Int -> Term Bool -> Term String -> Maybe Int
    getTermsize1 (Lit n) (Lit b) (Lit s) = Just $ if b then n else length s
    getTermsize1 _ _ _ = Nothing

-- Fold over a list with two parts 'unLit' and 'getSize'
ex2 :: Int
ex2 = uncurryList_ unLit getsize2 args2
  where
    unLit :: forall a. Term a -> a
    unLit (Lit n) = n
    unLit _ = error "unLit on non literal"
    getsize2 :: Int -> Bool -> String -> Int
    getsize2 n b s = if b then n else length s
    args2 :: List Term '[Int, Bool, String]
    args2 = Lit 5 :> Lit True :> Lit "abc" :> Nil

-- Construct a function from a List and function on that list.
ex3 :: Term a -> Term b -> Term c -> String
ex3 = curryList crush
  where
    crush :: (List Term '[a, b, c] -> String)
    crush (a :> b :> c :> Nil) = show a ++ show b ++ show c

-- Construct a function over from a
ex4 :: Int -> Bool -> String -> Int
ex4 = curryList_ one totalLength
  where
    totalLength :: List [] '[Int, Bool, String] -> Int
    totalLength (n :> b :> s :> Nil) = length n + length b + length s
    one :: a -> [a]
    one x = [x]

ex5 :: Spec (Set Integer)
ex5 = constrained $ \s -> Assert (size_ s ==. Lit 104)

-- ==========================================================================

data Sum3 a b c where
  Inj3_1 :: a -> Sum3 a b c
  Inj3_2 :: b -> Sum3 a b c
  Inj3_3 :: c -> Sum3 a b c

deriving instance (Show a, Show b, Show c) => Show (Sum3 a b c)

deriving instance (Eq a, Eq b, Eq c) => Eq (Sum3 a b c)

foo_ :: Term (Sum3 a b c) -> Term a
foo_ = undefined

bar_ = undefined

bar_ :: Term (Sum3 a b c) -> Term (Either b c)
guardSum3 ::
  forall a b c. (HasSpec a, HasSpec b, HasSpec c) => Spec a -> Spec (Either b c) -> Spec (Sum3 a b c)
guardSum3 (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
guardSum3 (ErrorSpec es) _ = ErrorSpec (NE.cons "Sum3 error on left" es)
guardSum3 _ (ErrorSpec es) = ErrorSpec (NE.cons "Sum3 error on right" es)
guardSum3 s s' = typeSpec $ SumSpec s s'

instance (HasSpec a, HasSpec b, HasSpec c) => HasSpec (Sum3 a b c) where
  type TypeSpec (Sum3 a b c) = SumSpec (Spec a) (Spec (Either b c))

  anySpec = SumSpec TrueSpec TrueSpec

  combineSpec (SumSpec x y) (SumSpec a b) = guardSum3 (x <> a) (y <> b)

  conformsTo (Inj3_1 a) (SumSpec as _) = conformsToSpec a as
  conformsTo (Inj3_2 b) (SumSpec _ es) = conformsToSpec (Left b) es
  conformsTo (Inj3_3 c) (SumSpec _ es) = conformsToSpec (Right c) es

  toPreds x (SumSpec a es) = satisfies (foo_ x) a <> satisfies (bar_ x) es

  genFromTypeSpec (SumSpec (simplifySpec -> sa) (simplifySpec -> sb))
    | emptyA, emptyB = genError "genFromTypeSpec @SumSpec: empty"
    | emptyA = Inj3_1 <$> genFromSpecT sa
    | emptyB = select <$> genFromSpecT sb
    | otherwise = oneofT [Inj3_1 <$> genFromSpecT sa, select <$> genFromSpecT sb]
    where
      emptyA = isErrorLike sa
      emptyB = isErrorLike sb
      select :: Either b c -> Sum3 a b c
      select (Left x) = Inj3_2 x
      select (Right x) = Inj3_3 x

-- ====================================================

coerce_ :: Term (E3 a b c) -> Term (Either a (Either b c))
coerce_ = undefined

newtype E3 a b c = E3 (Either a (Either b c))
  deriving (Eq, Show) via Either a (Either b c)

{-
instance (HasSpec a, HasSpec b, HasSpec c) => HasSpec (E3 a b c) where
   type TypeSpec (E3 a b c) = TypeSpec (Either a (Either b c))

   anySpec = SumSpec TrueSpec TrueSpec

   combineSpec (SumSpec x y) (SumSpec a b) = typeSpec (SumSpec (x <> a) (y <> b))

   conformsTo (E3 (Left x)) (SumSpec a b) = conformsToSpec x a
   conformsTo (E3 (Right x)) (SumSpec a b) = conformsToSpec x b

   toPreds x (SumSpec a b) = Case (coerce_ x)
                                  (bind $ \y -> satisfies y a)
                                  (bind $ \y -> satisfies y b)

flipp :: (Eq b, Typeable b, Show b, HasSpec b, Coercible a b) => Term a -> Term b
flipp (Lit x) = Lit (coerce x)
flipp (V x) = V (coerce x)
-- flipp (App c xs) = App (coerce c) xs
-}
