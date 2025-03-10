{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

-- | Code for the Foldy class, the FunW witness (compose_,id_,flip_) and
--   HasSpec instance for List. These things are all mutually recursive.
module Constrained.Spec.ListFoldy where

import Constrained.Core (Evidence (..), NonEmpty ((:|)), Var (..), eqVar, unionWithMaybe)
import Constrained.Base
import Constrained.Conformance (conformsToSpec, satisfies)
import Constrained.NumSpec
import Constrained.Spec.Num ()
import Constrained.Spec.Size (Sized (..), genFromSizeSpec, sizeOf_)
import Constrained.SumList (Cost (..), Solution (No, Yes), pickAll)
import Constrained.Syntax (forAll, genHint, unsafeExists)
import Constrained.TheKnot (
  caseBoolSpec,
  genFromSpecT,
  mapSpec,
  shrinkWithSpec,
  simplifySpec,
  (==.),
 )
import Constrained.GenT
import Constrained.List
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Int
import Data.Kind
import Data.List (isPrefixOf, isSuffixOf, nub, (\\))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import Data.Word
import GHC.Stack
import GHC.TypeLits
import Prettyprinter hiding (cat)
import System.Random (Random)
import Test.QuickCheck (Arbitrary (..), oneof, shrinkList, shuffle)

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- So that's why the Foldy class is in the NumberSpec module.
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseW
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See FlipW amd ComposeW
-- ==============================================================

-- We need Arbitrary Specification to do this
instance {-# OVERLAPPABLE #-} (Arbitrary (Specification a {- Arbitrary (TypeSpec a), -}), Foldy a) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec (Fun IdW) <$> arbitrary, pure NoFold]
  shrink NoFold = []
  -- shrink (FoldSpec (sameFunSym (IdW @a) -> Just(idW,Refl,Refl,Refl,Refl,Refl)) spec) = FoldSpec (Fun Evidence idW) <$> shrink spec
  -- shrink (FoldSpec fun spec) = FoldSpec (fun :: Fun '[a] b) <$> shrink (spec :: Specification b)
  shrink FoldSpec {} = [NoFold]

data FunW (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  IdW :: forall a. FunW "id_" '[a] a
  ComposeW ::
    forall b s1 s2 t1 t2 a r.
    ( Logic s1 t1 '[b] r
    , Logic s2 t2 '[a] b
    , HasSpec b
    ) =>
    t1 s1 '[b] r -> t2 s2 '[a] b -> FunW "composeFn" '[a] r
  FlipW ::
    forall sym t a b r.
    Logic sym t '[a, b] r =>
    t sym '[a, b] r -> FunW "flip_" '[b, a] r

funSem :: FunW sym dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\a -> semantics f (semantics g a))
funSem (FlipW (f :: g s d r)) = flip (semantics f)

instance Semantics FunW where
  semantics = funSem

instance Syntax FunW

instance KnownSymbol s => Show (FunW s dom rng) where
  show IdW = "id_"
  show (FlipW f) = "(flip_ " ++ show f ++ ")"
  show (ComposeW x y) = "(compose_ " ++ show x ++ " " ++ show y ++ ")"

instance Eq (FunW s dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'

compareWit ::
  forall s1 t1 bs1 r1 s2 t2 bs2 r2.
  (Logic s1 t1 bs1 r1, Logic s2 t2 bs2 r2) =>
  t1 s1 bs1 r1 -> t2 s2 bs2 r2 -> Bool
compareWit x y = case (eqT @t1 @t2, eqT @s1 @s2, eqT @bs1 @bs2, eqT @r1 @r2) of
  (Just Refl, Just Refl, Just Refl, Just Refl) -> x == y
  _ -> False

-- ===================================
-- Logic instances for IdW, FlipW and ComposeW
-- Also their Haskell implementations id_ flip_ composeFn

instance HasSpec a => Logic "id_" FunW '[a] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context IdW (HOLE :<> End)) spec = spec
  propagate ctxt _ = ErrorSpec (NE.fromList ["IdW (id_)", "Unreachable context, too many args", show ctxt])

  mapTypeSpec IdW ts = typeSpec ts
  rewriteRules IdW (x :> Nil) Evidence = Just x

id_ :: forall a. HasSpec a => Term a -> Term a
id_ = appTerm IdW

instance
  (forall sym t. Logic sym t '[a, b] r, All Typeable [a, b, r]) =>
  Logic "flip_" FunW '[b, a] r
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context (FlipW f) (HOLE :<> v :<| End)) spec = propagate (Context f (v :|> HOLE :<> End)) spec
  propagate (Context (FlipW f) (v :|> HOLE :<> End)) spec = propagate (Context f (HOLE :<> v :<| End)) spec
  propagate ctxt@(Context _ _) _ = ErrorSpec (NE.fromList ["FlipW (flip_)", "Unreachable context, too many args", show ctxt])

  -- Note we need Evidence to apply App to f
  rewriteRules (FlipW f) (a@Lit {} :> b :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW f) (a :> b@Lit {} :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW {}) _ Evidence = Nothing

flip_ ::
  forall (t :: FSType) (sym :: Symbol) a b r.
  (HasSpec b, HasSpec a, HasSpec r, forall sym1 t1. Logic sym1 t1 '[a, b] r) =>
  t sym '[a, b] r -> Term b -> Term a -> Term r
flip_ x = appTerm (FlipW x)

instance
  ( All Typeable [a, r]
  , HasSpec r
  ) =>
  Logic "composeFn" FunW '[a] r
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context (ComposeW (f :: t1' s1' '[b'] r') (g :: t2' s2' '[a'] b'')) (HOLE :<> End)) spec =
    propagate @s2' @t2' @'[a'] @b' (Context g (HOLE :<> End)) $
      propagate @s1' @t1' @'[b'] @r' (Context f (HOLE :<> End)) spec
  propagate ctxt@(Context _ _) _ = ErrorSpec (NE.fromList ["ComposeW (composeFn)", "Unreachable context, too many args", show ctxt])

  mapTypeSpec (ComposeW g h) ts = mapSpec g . mapSpec h $ typeSpec ts

  -- Note we need the Evidence to apply App to f, and to apply App to g
  rewriteRules (ComposeW f g) (x :> Nil) Evidence = Just $ App f (App g (x :> Nil) :> Nil)

compose_ ::
  forall b s1 s2 t1 t2 a r.
  ( AppRequires s1 t1 '[b] r
  , AppRequires s2 t2 '[a] b
  ) =>
  t1 s1 '[b] r -> t2 s2 '[a] b -> Term a -> Term r
compose_ f g = appTerm $ ComposeW f g -- @b @c1 @c2 @s1 @s2 @t1 @t2 @a @r f g

-- =============================================================

composeFn :: (HasSpec b, HasSpec a, HasSpec c) => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
composeFn (Fun f) (Fun g) = (Fun (ComposeW f g))

-- flipFn :: forall a b r. (All HasSpec '[b, a], HasSpec r) => Fun '[a,b] r -> Fun '[b,a] r
-- flipFn (Fun (f :: t c s '[a',b'] r')) = Fun (FlipW (f :: t c s '[a',b'] r'))
-- flipFn (Fun f) = Fun (FlipW f)

idFn :: HasSpec a => Fun '[a] a
idFn = Fun IdW

-- =======================================================
-- All the Foldy class instances are intimately tied to
-- Numbers. But that is not required, but this is a
-- convenient place to put the code.
-- =======================================================

class (HasSpec a, NumLike a, Logic "addFn" IntW '[a, a] a) => Foldy a where
  genList ::
    MonadGenError m => Specification a -> Specification a -> GenT m [a]
  theAddFn :: IntW "addFn" '[a, a] a
  theAddFn = AddW
  theZero :: a
  theZero = 0
  genSizedList ::
    MonadGenError m =>
    Specification Integer -> Specification a -> Specification a -> GenT m [a]
  noNegativeValues :: Bool

instance Foldy Integer where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int8 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int16 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int32 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int64 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Natural where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word8 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word16 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word32 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word64 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

genInverse ::
  ( MonadGenError m
  , HasSpec a
  , HasSpec b
  ) =>
  Fun '[a] b ->
  Specification a ->
  b ->
  GenT m a
genInverse (Fun f) argS x =
  let argSpec' = argS <> propagate (Context f (HOLE :<> End)) (equalSpec x)
   in explain
        ( NE.fromList
            [ "genInverse"
            , "  f = " ++ show f
            , show $ "  argS =" <+> pretty argS
            , "  x = " ++ show x
            , show $ "  argSpec' =" <+> pretty argSpec'
            ]
        )
        $ genFromSpecT argSpec'

genFromFold ::
  forall m a b.
  ( MonadGenError m
  , Foldy b
  , HasSpec a
  ) =>
  [a] ->
  Specification Integer ->
  Specification a ->
  Fun '[a] b ->
  Specification b ->
  GenT m [a]
genFromFold must (simplifySpec -> size) elemS fun@(Fun fn) foldS
  | isErrorLike size =
      fatalError (NE.cons "genFromFold has ErrorLike sizeSpec" (errorLikeMessage size))
  | isErrorLike elemS =
      fatalError (NE.cons "genFromFold has ErrorLike elemSpec" (errorLikeMessage elemS))
  | isErrorLike foldS =
      fatalError (NE.cons "genFromFold has ErrorLike totalSpec" (errorLikeMessage foldS))
  | otherwise = ( explain
                    ( NE.fromList
                        [ "while calling genFromFold"
                        , "  must  = " ++ show must
                        , "  size  = " ++ show size
                        , "  elemS = " ++ show elemS
                        , "  fun   = " ++ show fun
                        , "  foldS = " ++ show foldS
                        ]
                    )
                )
      $ do
        let elemS' :: Specification b
            elemS' = mapSpec fn elemS
            mustVal = adds (map (semantics fn) must)
            foldS' :: Specification b
            foldS' = propagate (Context theAddFn (HOLE :<> mustVal :<| End)) foldS
            sizeSpec' :: Specification Integer
            sizeSpec' = propagate (Context AddW (HOLE :<> (sizeOf must) :<| End)) size
        when (isErrorLike sizeSpec') $ genError1 "Inconsistent size spec"
        results0 <- case sizeSpec' of
          TrueSpec -> genList (simplifySpec elemS') (simplifySpec foldS')
          _ -> genSizedList sizeSpec' (simplifySpec elemS') (simplifySpec foldS')
        results <-
          explain
            ( NE.fromList
                [ "genInverse"
                , "  fun = " ++ show fun
                , "  results0 = " ++ show results0
                , show $ "  elemS' =" <+> pretty elemS'
                ]
            )
            $ mapM (genInverse fun elemS) results0
        pureGen $ shuffle $ must ++ results

adds :: Foldy a => [a] -> a
adds = foldr (semantics theAddFn) theZero

addFun :: NumLike n => Fun '[n, n] n
addFun = Fun AddW

data FoldSpec a where
  NoFold :: FoldSpec a
  FoldSpec ::
    forall b a.
    ( HasSpec a
    , HasSpec b
    , Foldy b
    ) =>
    Fun '[a] b -> Specification b -> FoldSpec a

preMapFoldSpec :: HasSpec a => Fun '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s

combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (Fun f) s) (FoldSpec (Fun g) s') =
  case sameFunSym f g of
    Just (_h, Refl, Refl, Refl, Refl) -> pure $ FoldSpec (Fun f) (s <> s')
    Nothing -> Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show g]

conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec (Fun f) s) = adds (map (semantics f) xs) `conformsToSpec` s

{- This must appear in sumProd where the Generic Instances for Prod and PairSpec are in scope
instance
  (HasSpec a, HasSpec b, Arbitrary (FoldSpec a), Arbitrary (FoldSpec b)) =>
  Arbitrary (FoldSpec (a, b))
  where
  arbitrary =
    oneof
      [ preMapFoldSpec (composeFn (Fun ProdFstW) (Fun ToGenericW)) <$> arbitrary
      , preMapFoldSpec (composeFn (Fun ProdSndW) (Fun ToGenericW)) <$> arbitrary
      , pure NoFold
      ]
  shrink NoFold = []
  shrink FoldSpec {} = [NoFold]
-}

-- =============================================================================
-- Lists and Foldy are mutually recursive
-- Here are HasSpec instance for List and the Logic instances for List operators

data ListW (s :: Symbol) (args :: [Type]) (res :: Type) where
  FoldMapW :: forall a b. (Foldy b, HasSpec a) => Fun '[a] b -> ListW "foldMap_" '[[a]] b
  SingletonListW :: ListW "singeltonList_" '[a] [a]
  AppendW :: (Typeable a, Show a) => ListW "append_" '[[a], [a]] [a]

-- ====================== Semantics for ListW

instance Semantics ListW where
  semantics = listSem

instance Syntax ListW where
  prettyWit AppendW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> short n <+> prettyPrec 10 y
  prettyWit AppendW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> prettyPrec 10 y <+> short n
  prettyWit _ _ _ = Nothing

listSem :: ListW s dom rng -> FunTy dom rng
listSem (FoldMapW (Fun f)) = adds . map (semantics f)
listSem SingletonListW = (: [])
listSem AppendW = (++)

instance Show (ListW s d r) where
  show AppendW = "append_"
  show SingletonListW = "singletonList_"
  show (FoldMapW n) = "(FoldMapW  " ++ show n ++ ")"

deriving instance (Eq (ListW s d r))

-- ============= Logicbol for FoldMapW

instance (Typeable a, Foldy b, HasSpec b) => Logic "foldMap_" ListW '[[a]] b where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context (FoldMapW f) (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App (FoldMapW f) (v' :> Nil)) (v :-> ps)
  propagate (Context (FoldMapW f) (HOLE :<> End)) (TypeSpec ts cant) =
    typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f (TypeSpec ts cant))
  propagate (Context (FoldMapW f) (HOLE :<> End)) (MemberSpec es) =
    typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f (MemberSpec es))
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for FoldMapW with wrong number of arguments. " ++ show ctx)

  mapTypeSpec (FoldMapW g) ts =
    constrained $ \x ->
      unsafeExists $ \x' ->
        Assert (x ==. appFun (foldMapFn g) x') <> toPreds x' ts

foldMap_ :: forall a b. (Sized [a], Foldy b, HasSpec a) => (Term a -> Term b) -> Term [a] -> Term b
foldMap_ f = appFun $ foldMapFn $ toFn $ f (V v)
  where
    v = Var (-1) "v" :: Var a
    -- Turn `f (V v) = fn (gn (hn v))` into `composeFn fn (composeFn gn hn)`
    -- Note: composeFn :: HasSpec b => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
    toFn :: forall x. HasCallStack => Term x -> Fun '[a] x
    toFn (App fn (V v' :> Nil)) | Just Refl <- eqVar v v' = Fun fn
    toFn (App fn (t :> Nil)) = composeFn (Fun fn) (toFn t)
    toFn (V v') | Just Refl <- eqVar v v' = idFn
    toFn _ = error "foldMap_ has not been given a function of the form \\ x -> f (g ... (h x))"

sum_ ::
  Foldy a =>
  Term [a] ->
  Term a
sum_ = foldMap_ id

foldMapFn :: forall a b. (HasSpec a, Foldy b) => Fun '[a] b -> Fun '[[a]] b
foldMapFn f = Fun (FoldMapW f)

-- | Used in the HasSpec [a] instance
toPredsFoldSpec :: (HasSpec a, HasSpec [a]) => Term [a] -> FoldSpec a -> Pred
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec funAB sspec) =
  satisfies (appFun (foldMapFn funAB) x) sspec

-- ============ Logicbol for ElemW

instance HasSpec a => Logic "elem_" BaseW '[a, [a]] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context ElemW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ElemW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context ElemW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ElemW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context ElemW (HOLE :<> es :<| End)) spec =
    caseBoolSpec spec $ \case
      True -> memberSpecList (nub es) (pure "propagate on (elem_ x []), The empty list, [], has no solution")
      False -> notMemberSpec es
  propagate (Context ElemW (e :|> HOLE :<> End)) spec =
    caseBoolSpec spec $ \case
      True -> typeSpec (ListSpec Nothing [e] mempty mempty NoFold)
      False -> typeSpec (ListSpec Nothing mempty mempty (notEqualSpec e) NoFold)
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for ElemW with wrong number of arguments. " ++ show ctx)

  rewriteRules ElemW (_ :> Lit [] :> Nil) Evidence = Just $ Lit False
  rewriteRules ElemW (t :> Lit [a] :> Nil) Evidence = Just $ t ==. (Lit a)
  rewriteRules _ _ _ = Nothing

  saturate ElemW (FromGeneric (Product @n @m x y) :> Lit zs :> Nil)
    | Just Refl <- eqT @a @(m, n) = case zs of
        (w : ws) -> [ElemPred True x (fmap fst (w :| ws))]
        [] -> [FalsePred (pure $ "empty list, zs , in elem_ " ++ show (x, y) ++ " zs")]
  saturate ElemW (x :> Lit (y : ys) :> Nil) = [satisfies x (MemberSpec (y :| ys))]
  saturate _ _ = []

infix 4 `elem_`
elem_ :: (Sized [a], HasSpec a) => Term a -> Term [a] -> Term Bool
elem_ = appTerm ElemW

elemFn :: HasSpec a => Fun '[a, [a]] Bool
elemFn = Fun ElemW

-- ============= Logicbol for SingletonListW

instance HasSpec a => Logic "singeltonList_" ListW '[a] [a] where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context SingletonListW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App SingletonListW (v' :> Nil)) (v :-> ps)
  propagate (Context SingletonListW (HOLE :<> End)) (TypeSpec (ListSpec _ m sz e f) cant)
    | length m > 1 =
        ErrorSpec $
          NE.fromList
            [ "Too many required elements for SingletonListW : "
            , "  " ++ show m
            ]
    | not $ 1 `conformsToSpec` sz =
        ErrorSpec $ pure $ "Size spec requires too many elements for SingletonListW : " ++ show sz
    | bad@(_ : _) <- filter (not . (`conformsToSpec` e)) m =
        ErrorSpec $
          NE.fromList
            [ "The following elements of the must spec do not conforms to the elem spec:"
            , show bad
            ]
    -- There is precisely one required element in the final list, so the argument to singletonList_ has to
    -- be that element and we have to respect the cant and fold specs
    | [a] <- m = equalSpec a <> notMemberSpec [z | [z] <- cant] <> reverseFoldSpec f
    -- We have to respect the elem-spec, the can't spec, and the fold spec.
    | otherwise = e <> notMemberSpec [a | [a] <- cant] <> reverseFoldSpec f
  propagate (Context SingletonListW (HOLE :<> End)) (MemberSpec xss) =
    case [a | [a] <- NE.toList xss] of
      [] ->
        ErrorSpec $ (pure "PropagateSpec SingletonListW  with MemberSpec which has no lists of length 1")
      (x : xs) -> MemberSpec (x :| xs)
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for SingletonListW with wrong number of arguments. " ++ show ctx)

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec SingletonListW ts = typeSpec (ListSpec Nothing [] (equalSpec 1) (typeSpec ts) NoFold)

reverseFoldSpec :: FoldSpec a -> Specification a
reverseFoldSpec NoFold = TrueSpec
-- The single element list has to sum to something that obeys spec, i.e. `conformsToSpec (f a) spec`
reverseFoldSpec (FoldSpec (Fun fn) spec) = propagate (Context fn (HOLE :<> End)) spec

singletonList_ :: (Sized [a], HasSpec a) => Term a -> Term [a]
singletonList_ = appTerm SingletonListW

singletonListFn :: forall a. HasSpec a => Fun '[a] [a]
singletonListFn = Fun SingletonListW

-- ============== Logicbol for AppendW

instance (Sized [a], HasSpec a) => Logic "append_" ListW '[[a], [a]] [a] where
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context AppendW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App AppendW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context AppendW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App AppendW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context AppendW ctx) (TypeSpec (ts@ListSpec {listSpecElem = e}) cant)
    | (HOLE :<> (ys :: [a]) :<| End) <- ctx
    , Evidence <- prerequisites @[a]
    , all (`conformsToSpec` e) ys =
        TypeSpec (alreadyHave ys ts) (suffixedBy ys cant)
    | ((ys :: [a]) :|> HOLE :<> End) <- ctx
    , Evidence <- prerequisites @[a]
    , all (`conformsToSpec` e) ys =
        TypeSpec (alreadyHave ys ts) (prefixedBy ys cant)
    | otherwise = ErrorSpec $ pure "The spec given to propagate for AppendW is inconsistent!"
  propagate (Context AppendW ctx) (MemberSpec xss)
    | (HOLE :<> (ys :: [a]) :<| End) <- ctx
    , Evidence <- prerequisites @[a] =
        -- Only keep the prefixes of the elements of xss that can
        -- give you the correct resulting list
        case suffixedBy ys (NE.toList xss) of
          [] ->
            ErrorSpec
              ( NE.fromList
                  [ "propagateSpecFun (append HOLE ys) with (MemberSpec xss)"
                  , "there are no elements in xss with suffix ys"
                  ]
              )
          (x : xs) -> MemberSpec (x :| xs)
    | ((ys :: [a]) :|> HOLE :<> End) <- ctx
    , Evidence <- prerequisites @[a] =
        -- Only keep the suffixes of the elements of xss that can
        -- give you the correct resulting list
        case prefixedBy ys (NE.toList xss) of
          [] ->
            ErrorSpec
              ( NE.fromList
                  [ "propagateSpecFun (append ys HOLE) with (MemberSpec xss)"
                  , "there are no elements in xss with prefix ys"
                  ]
              )
          (x : xs) -> MemberSpec (x :| xs)
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for AppendW with wrong number of arguments. " ++ show ctx)

prefixedBy :: Eq a => [a] -> [[a]] -> [[a]]
prefixedBy ys xss = [drop (length ys) xs | xs <- xss, ys `isPrefixOf` xs]

suffixedBy :: Eq a => [a] -> [[a]] -> [[a]]
suffixedBy ys xss = [take (length xs - length ys) xs | xs <- xss, ys `isSuffixOf` xs]

alreadyHave {- (Eq a, Typeable a, Show a) -} :: Eq a => [a] -> ListSpec a -> ListSpec a
alreadyHave ys (ListSpec h m sz e f) =
  ListSpec
    -- Reduce the hint
    (fmap (subtract (sizeOf ys)) h)
    -- The things in `ys` have already been added to the list, no need to
    -- require them too
    (m \\ ys)
    -- Reduce the required size
    (constrained $ \x -> (x + Lit (sizeOf ys)) `satisfies` sz)
    -- Nothing changes about what's a correct element
    e
    -- we have fewer things to sum now
    (alreadyHaveFold ys f)

alreadyHaveFold {- (Typeable a, Show a) => -} :: [a] -> FoldSpec a -> FoldSpec a
alreadyHaveFold _ NoFold = NoFold
alreadyHaveFold ys (FoldSpec fn spec) =
  FoldSpec
    fn
    (constrained $ \s -> appTerm theAddFn s (foldMap_ (appFun fn) (Lit ys)) `satisfies` spec)

appendFn :: forall a. (Sized [a], HasSpec a) => Fun '[[a], [a]] [a]
appendFn = Fun AppendW

append_ :: (Sized [a], HasSpec a) => Term [a] -> Term [a] -> Term [a]
append_ = appTerm AppendW

-- ================================================
-- Sized instance for Lists

instance Sized [a] where
  sizeOf = toInteger . length
  liftSizeSpec spec cant = typeSpec (ListSpec Nothing mempty (TypeSpec spec cant) TrueSpec NoFold)
  liftMemberSpec xs = case NE.nonEmpty xs of
    Nothing -> ErrorSpec (pure ("In liftMemberSpec for (Sized List) instance, xs is the empty list"))
    Just zs -> typeSpec (ListSpec Nothing mempty (MemberSpec zs) TrueSpec NoFold)
  sizeOfTypeSpec (ListSpec _ _ _ ErrorSpec {} _) = equalSpec 0
  sizeOfTypeSpec (ListSpec _ must sizespec _ _) = sizespec <> geqSpec (sizeOf must)

-- ================================================================
-- The TypeSpec for List. Used in the HasSpec instance for Lists
-- ================================================================

data ListSpec a = ListSpec
  { listSpecHint :: Maybe Integer
  , listSpecMust :: [a]
  , listSpecSize :: Specification Integer
  , listSpecElem :: Specification a
  , listSpecFold :: FoldSpec a
  }

{-
instance (Ord a, HasSpec a, Foldy a, TypeSpec a ~ NumSpec a, Arbitrary a) => Arbitrary (ListSpec a) where
  arbitrary = ListSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
-}

instance
  ( Arbitrary a
  , Arbitrary (FoldSpec a)
  , Arbitrary (TypeSpec a)
  , HasSpec a
  ) =>
  Arbitrary (ListSpec a)
  where
  arbitrary = ListSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (ListSpec a b c d e) = [ListSpec a' b' c' d' e' | (a', b', c', d', e') <- shrink (a, b, c, d, e)]

instance HasSpec a => Show (FoldSpec a) where
  showsPrec d = shows . prettyPrec d

instance HasSpec a => Pretty (WithPrec (FoldSpec a)) where
  pretty (WithPrec _ NoFold) = "NoFold"
  pretty (WithPrec d (FoldSpec fun s)) =
    parensIf (d > 10) $
      "FoldSpec"
        /> vsep'
          [ "fn   =" <+> viaShow fun
          , "spec =" <+> pretty s
          ]

instance HasSpec a => Pretty (FoldSpec a) where
  pretty = prettyPrec 0

instance HasSpec a => Show (ListSpec a) where
  showsPrec d = shows . prettyPrec d

instance
  HasSpec a =>
  Pretty (WithPrec (ListSpec a))
  where
  pretty (WithPrec d s) =
    parensIf (d > 10) $
      "ListSpec"
        /> vsep'
          [ "hint =" <+> viaShow (listSpecHint s)
          , "must =" <+> viaShow (listSpecMust s)
          , "size =" <+> pretty (listSpecSize s)
          , "elem =" <+> pretty (listSpecElem s)
          , "fold =" <+> pretty (listSpecFold s)
          ]

instance HasSpec a => Pretty (ListSpec a) where
  pretty = prettyPrec 0

guardListSpec :: HasSpec a => [String] -> ListSpec a -> Specification [a]
guardListSpec msg l@(ListSpec _hint must size elemS _fold)
  | ErrorSpec es <- size = ErrorSpec $ (NE.fromList ("Error in size of ListSpec" : msg)) <> es
  | Just u <- knownUpperBound size
  , u < 0 =
      ErrorSpec $ NE.fromList (["Negative size in guardListSpec", show size] ++ msg)
  | not (all (`conformsToSpec` elemS) must) =
      ErrorSpec $
        ( NE.fromList
            (["Some items in the must list do not conform to 'element' spec.", "   " ++ show elemS] ++ msg)
        )
  | otherwise = (typeSpec l)

instance (Sized [a], HasSpec a) => HasSpec [a] where
  type TypeSpec [a] = ListSpec a
  type Prerequisites [a] = HasSpec a
  emptySpec = ListSpec Nothing [] mempty mempty NoFold
  combineSpec l1@(ListSpec msz must size elemS foldS) l2@(ListSpec msz' must' size' elemS' foldS') =
    let must'' = nub $ must <> must'
        elemS'' = elemS <> elemS'
        size'' = size <> size'
        foldeither = combineFoldSpec foldS foldS'
        msg = ["Error in combineSpec for ListSpec", "1) " ++ show l1, "2) " ++ show l2]
     in case foldeither of
          Left foldmsg -> ErrorSpec (NE.fromList (msg ++ foldmsg))
          Right fold'' -> guardListSpec msg $ ListSpec (unionWithMaybe min msz msz') must'' size'' elemS'' fold''

  genFromTypeSpec (ListSpec _ must _ elemS _)
    | any (not . (`conformsToSpec` elemS)) must =
        genError1 "genTypeSpecSpec @ListSpec: some elements of mustSet do not conform to elemS"
  genFromTypeSpec (ListSpec msz must TrueSpec elemS NoFold) = do
    lst <- case msz of
      Nothing -> listOfT $ genFromSpecT elemS
      Just szHint -> do
        sz <- genFromSizeSpec (leqSpec szHint)
        listOfUntilLenT (genFromSpecT elemS) (fromIntegral sz) (const True)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS NoFold) = do
    sz0 <- genFromSizeSpec (szSpec <> geqSpec (sizeOf must) <> maybe TrueSpec (leqSpec . max 0) msz)
    let sz = fromIntegral (sz0 - sizeOf must)
    lst <-
      listOfUntilLenT
        (genFromSpecT elemS)
        sz
        ((`conformsToSpec` szSpec) . (+ sizeOf must) . fromIntegral)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS (FoldSpec f foldS)) = do
    let szSpec' = szSpec <> maybe TrueSpec (leqSpec . max 0) msz
    genFromFold must szSpec' elemS f foldS

  shrinkWithTypeSpec (ListSpec _ _ _ es _) as =
    shrinkList (shrinkWithSpec es) as

  cardinalTypeSpec _ = TrueSpec

  guardTypeSpec = guardListSpec

  conformsTo xs (ListSpec _ must size elemS foldS) =
    sizeOf xs `conformsToSpec` size
      && all (`elem` xs) must
      && all (`conformsToSpec` elemS) xs
      && xs `conformsToFoldSpec` foldS

  toPreds x (ListSpec msz must size elemS foldS) =
    (forAll x $ \x' -> satisfies x' elemS)
      <> (forAll (Lit must) $ \x' -> Assert (elem_ x' x))
      <> toPredsFoldSpec x foldS
      <> satisfies (sizeOf_ x) size
      <> maybe TruePred (flip genHint x) msz

instance (Sized [a], HasSpec a) => HasGenHint [a] where
  type Hint [a] = Integer
  giveHint szHint = typeSpec $ ListSpec (Just szHint) [] mempty mempty NoFold

instance Forallable [a] a where
  fromForAllSpec es = typeSpec (ListSpec Nothing [] mempty es NoFold)
  forAllToList = id

-- ==========================================================
-- helpers

knownUpperBound ::
  (TypeSpec a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) =>
  Specification a ->
  Maybe a
knownUpperBound (ExplainSpec _ s) = knownUpperBound s
knownUpperBound TrueSpec = upperBound
knownUpperBound (MemberSpec as) = Just $ maximum as
knownUpperBound ErrorSpec {} = Nothing
knownUpperBound SuspendedSpec {} = upperBound
knownUpperBound (TypeSpec (NumSpecInterval lo hi) cant) = upper (lo <|> lowerBound) (hi <|> upperBound)
  where
    upper _ Nothing = Nothing
    upper Nothing (Just b) = listToMaybe $ [b, b - 1 ..] \\ cant
    upper (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [b, b - 1 .. a] \\ cant

knownLowerBound ::
  (TypeSpec a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) =>
  Specification a ->
  Maybe a
knownLowerBound (ExplainSpec _ s) = knownLowerBound s
knownLowerBound TrueSpec = lowerBound
knownLowerBound (MemberSpec as) = Just $ minimum as
knownLowerBound ErrorSpec {} = Nothing
knownLowerBound SuspendedSpec {} = lowerBound
knownLowerBound (TypeSpec (NumSpecInterval lo hi) cant) =
  lower (lo <|> lowerBound) (hi <|> upperBound)
  where
    lower Nothing _ = Nothing
    lower (Just a) Nothing = listToMaybe $ [a, a + 1 ..] \\ cant
    lower (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [a, a + 1 .. b] \\ cant

isEmptyNumSpec ::
  (TypeSpec a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) => Specification a -> Bool
isEmptyNumSpec = \case
  ExplainSpec _ s -> isEmptyNumSpec s
  ErrorSpec {} -> True
  TrueSpec -> False
  MemberSpec _ -> False -- MemberSpec always has at least one element (NE.NonEmpty)
  SuspendedSpec {} -> False
  TypeSpec i cant -> null $ enumerateInterval i \\ cant

-- | Note: potentially infinite list
enumerateInterval :: (Enum a, Num a, MaybeBounded a) => NumSpec a -> [a]
enumerateInterval (NumSpecInterval lo hi) =
  case (lo <|> lowerBound, hi <|> upperBound) of
    (Nothing, Nothing) -> interleave [0 ..] [-1, -2 ..]
    (Nothing, Just b) -> [b, b - 1 ..]
    (Just a, Nothing) -> [a ..]
    (Just a, Just b) -> [a .. b]
  where
    interleave [] ys = ys
    interleave (x : xs) ys = x : interleave ys xs

-- ========================================================================
-- Operations to complete the Foldy instances genNumList, genListWithSize

genNumList ::
  forall a m.
  ( MonadGenError m
  , Arbitrary a
  , Integral a
  , MaybeBounded a
  , TypeSpec a ~ NumSpec a
  , Foldy a
  , Random a
  ) =>
  Specification a ->
  Specification a ->
  GenT m [a]
genNumList elemSIn foldSIn = do
  let extraElemConstraints
        | Just l <- knownLowerBound elemSIn
        , 0 <= l
        , Just u <- knownUpperBound foldSIn =
            leqSpec u
        | otherwise = TrueSpec
      elemSIn' = elemSIn <> extraElemConstraints
  normElemS <- normalize elemSIn'
  normFoldS <- normalize foldSIn
  let narrowedSpecs = narrowFoldSpecs (normElemS, normFoldS)
  explain
    ( NE.fromList
        [ "Can't generate list of ints with fold constraint"
        , "  elemSpec = " ++ show elemSIn
        , "  normElemSpec = " ++ show normElemS
        , "  foldSpec = " ++ show foldSIn
        ]
    )
    $ gen narrowedSpecs 50 [] >>= pureGen . shuffle
  where
    normalize (ExplainSpec es x) = explainSpecOpt es <$> normalize x
    normalize spec@SuspendedSpec {} = do
      sz <- sizeT
      spec' <- buildMemberSpec sz (100 :: Int) mempty spec
      normalize $ spec'
    normalize spec =
      pure $
        maybe mempty geqSpec lowerBound
          <> maybe mempty leqSpec upperBound
          <> spec

    buildMemberSpec _ 0 es _ =
      pure
        ( memberSpecList
            (Set.toList es)
            (pure "In genNumList, in buildMemberSpec 'es' is the empty list, can't make a MemberSpec from that")
        )
    buildMemberSpec sz fuel es spec = do
      me <- scaleT (const sz) $ tryGenT (genFromSpecT spec)
      let sz'
            | sz > 100 = sz
            | isNothing me = 2 * sz + 1
            | Just e <- me, Set.member e es = 2 * sz + 1
            | otherwise = sz
      buildMemberSpec
        sz'
        (fuel - 1)
        (maybe es (flip Set.insert es) me)
        spec

    gen ::
      forall m'. MonadGenError m' => (Specification a, Specification a) -> Int -> [a] -> GenT m' [a]
    gen (elemS, foldS) fuel lst
      | fuel <= 0
      , not $ 0 `conformsToSpec` foldS =
          genError $
            NE.fromList
              [ "Ran out of fuel in genNumList"
              , "  elemSpec =" ++ show elemSIn
              , "  foldSpec = " ++ show foldSIn
              , "  lst = " ++ show (reverse lst)
              ]
      | ErrorSpec err <- foldS = genError err
      | ErrorSpec {} <- elemS = pure lst -- At this point we know that foldS admits 0 (also this should be redundant)
      | 0 `conformsToSpec` foldS = oneofT [pure lst, nonemptyList @GE] -- TODO: distribution
      | otherwise = nonemptyList
      where
        isUnsat (elemSpec, foldSpec) = isEmptyNumSpec foldSpec || not (0 `conformsToSpec` foldSpec) && isEmptyNumSpec elemSpec
        nonemptyList :: forall m''. MonadGenError m'' => GenT m'' [a]
        nonemptyList = do
          (x, specs') <-
            explain
              ( NE.fromList
                  [ "Generating an element:"
                  , "  elemS = " ++ show elemS
                  , "  foldS = " ++ show foldS
                  , "  fuel  = " ++ show fuel
                  , "  lst   = " ++ show (reverse lst)
                  ]
              )
              $ do
                sz <- sizeT
                x <- genFromSpecT elemS
                let foldS' = propagate (Context theAddFn (HOLE :<> x :<| End)) foldS
                    specs' = narrowByFuelAndSize (fromIntegral $ fuel - 1) sz (elemS, foldS')
                pure (x, specs')
                `suchThatT` not
                . isUnsat
                . snd
          gen specs' (fuel - 1) (x : lst)

narrowByFuelAndSize ::
  forall a.
  ( TypeSpec a ~ NumSpec a
  , HasSpec a
  , Arbitrary a
  , Integral a
  , Random a
  , MaybeBounded a
  ) =>
  -- | Fuel
  a ->
  -- | Integer
  Int ->
  (Specification a, Specification a) ->
  (Specification a, Specification a)
narrowByFuelAndSize fuel size specpair =
  loop (100 :: Int) (onlyOnceTransformations $ narrowFoldSpecs specpair)
  where
    loop 0 specs =
      error $
        unlines
          [ "narrowByFuelAndSize loops:"
          , "  fuel = " ++ show fuel
          , "  size = " ++ show size
          , "  specs = " ++ show specs
          , "  narrowFoldSpecs spec = " ++ show (narrowFoldSpecs specs)
          , "  go (narrowFoldSpecs specs) = " ++ show (go (narrowFoldSpecs specs))
          ]
    loop n specs = case go specs of
      Nothing -> specs
      Just specs' -> loop (n - 1) (narrowFoldSpecs specs')

    -- Transformations only applied once. It's annoying to check if you're
    -- going to change the spec with these so easier to just make sure you only apply
    -- these once
    onlyOnceTransformations (elemS, foldS)
      | fuel == 1 = (elemS <> foldS, foldS)
      | otherwise = (elemS, foldS)

    canReach _ 0 s = s == 0
    canReach e currentfuel s
      -- You can reach it in one step
      | s <= e = 0 < currentfuel
      | otherwise = canReach e (currentfuel - 1) (s - e)

    -- Precondition:
    --   a is negative
    --   the type has more negative numbers than positive ones
    safeNegate a
      | Just u <- upperBound
      , a < negate u =
          u
      | otherwise = negate a

    divCeil a b
      | b * d < a = d + 1
      | otherwise = d
      where
        d = a `div` b

    go (simplifySpec -> elemS, simplifySpec -> foldS)
      -- There is nothing we can do
      | fuel == 0 = Nothing
      | ErrorSpec {} <- elemS = Nothing
      | ErrorSpec {} <- foldS = Nothing
      -- Give up as early as possible
      | Just 0 <- knownUpperBound elemS
      , Just 0 <- knownLowerBound elemS
      , not $ 0 `conformsToSpec` foldS =
          Just (ErrorSpec (NE.fromList ["only 0 left"]), foldS)
      -- Make sure we try to generate the smallest possible list
      -- that gives you the right result - don't put a bunch of zeroes in
      -- a _small_ (size 0) list.
      | size == 0
      , 0 `conformsToSpec` elemS =
          Just (elemS <> notEqualSpec 0, foldS)
      -- Member specs with non-zero elements, TODO: explain
      | MemberSpec ys <- elemS
      , let xs = NE.toList ys
      , Just u <- knownUpperBound foldS
      , all (0 <=) xs
      , any (0 <) xs
      , let xMinP = minimum $ filter (0 <) xs
            possible x = x == u || xMinP <= u - x
            xs' = filter possible xs
      , xs' /= xs =
          Just (memberSpecList (nubOrd xs') (pure ("None of " ++ show xs ++ " are possible")), foldS)
      -- The lower bound on the number of elements is too low
      | Just e <- knownLowerBound elemS
      , e > 0
      , Just s <- knownLowerBound foldS
      , s > 0
      , let c = divCeil s fuel
      , e < c =
          Just (elemS <> geqSpec c, foldS)
      -- The upper bound on the number of elements is too high
      | Just e <- knownUpperBound elemS
      , e < 0
      , Just s <- knownUpperBound foldS
      , s < 0
      , let c = divCeil (safeNegate s) fuel
      , negate c < e
      , maybe True (c <) (knownUpperBound elemS) =
          Just (elemS <> leqSpec c, foldS)
      -- It's time to stop generating negative numbers
      | Just s <- knownLowerBound foldS
      , s > 0
      , Just e <- knownUpperBound elemS
      , e > 0
      , not $ canReach e (fuel `div` 2 + 1) s
      , maybe True (<= 0) (knownLowerBound elemS) =
          Just (elemS <> gtSpec 0, foldS)
      -- It's time to stop generating positive numbers
      | Just s <- knownUpperBound foldS
      , s < 0
      , Just e <- knownLowerBound elemS
      , e < 0
      , not $ canReach (safeNegate e) (fuel `div` 2 + 1) (safeNegate s)
      , maybe True (0 <=) (knownUpperBound elemS) =
          Just (elemS <> ltSpec 0, foldS)
      -- There is nothing we need to do
      | otherwise = Nothing

narrowFoldSpecs ::
  forall a.
  ( TypeSpec a ~ NumSpec a
  , HasSpec a
  , Arbitrary a
  , Integral a
  , Random a
  , MaybeBounded a
  ) =>
  (Specification a, Specification a) ->
  (Specification a, Specification a)
narrowFoldSpecs specs = maybe specs narrowFoldSpecs (go specs)
  where
    -- Note: make sure there is some progress when returning Just or this will loop forever
    go (simplifySpec -> elemS, simplifySpec -> foldS) = case (elemS, foldS) of
      -- Empty foldSpec
      (_, ErrorSpec {}) -> Nothing
      _ | isEmptyNumSpec foldS -> Just (elemS, ErrorSpec (NE.fromList ["Empty foldSpec:", show foldS]))
      -- Empty elemSpec
      (ErrorSpec {}, MemberSpec ys) | NE.toList ys == [0] -> Nothing
      (ErrorSpec {}, _)
        | 0 `conformsToSpec` foldS -> Just (elemS, MemberSpec (pure 0))
        | otherwise ->
            Just
              ( elemS
              , ErrorSpec $
                  NE.fromList
                    [ "Empty elemSpec and non-zero foldSpec"
                    , show $ indent 2 $ "elemSpec =" /> pretty elemS
                    , show $ indent 2 $ "foldSpec =" /> pretty foldS
                    ]
              )
      -- We can reduce the size of the `elemS` interval when it is
      -- `[l, u]` or `[l, ∞)` given that `0 <= l` and we have
      -- an upper bound on the sum - we can't pick things bigger than the
      -- upper bound.
      _
        | Just lo <- knownLowerBound elemS
        , 0 <= lo
        , Just hi <- knownUpperBound foldS
        , -- Check that we will actually be making the set smaller
          fromMaybe True ((hi <) <$> knownUpperBound elemS) ->
            Just (elemS <> typeSpec (NumSpecInterval (Just lo) (Just hi)), foldS)
      -- We can reduce the size of the foldS set by bumping the lower bound when
      -- there is a positive lower bound on the elemS, we can't generate things smaller
      -- than the lower bound on `elemS`.
      _
        | Just lo <- knownLowerBound elemS
        , 0 <= lo
        , not $ 0 `conformsToSpec` foldS
        , -- Check that we will actually be making the set smaller
          fromMaybe True ((lo >) <$> knownLowerBound foldS) ->
            Just (elemS, foldS <> typeSpec (NumSpecInterval (Just lo) Nothing))
      -- NOTE: this is far from sufficient, but it's good enough of an approximation
      -- to avoid the worst failures.
      _
        | Just lo <- knownLowerBound elemS
        , Just loS <- knownLowerBound foldS
        , Just hi <- knownUpperBound elemS
        , Just hiS <- knownUpperBound foldS
        , hi < loS
        , lo > hiS - lo ->
            Just
              ( ErrorSpec $ NE.fromList ["Can't solve diophantine equation"]
              , ErrorSpec $ NE.fromList ["Can't solve diophantine equation"]
              )
      _ -> Nothing

-- =====================================================================================
-- Like genList, but generate a list whose size conforms to s SizeSpec
-- =====================================================================================

-- | Generate a list with 'sizeSpec' elements, that add up to a total that conforms
--   to 'foldSpec'. Every element in the list should conform to 'elemSpec'
genListWithSize ::
  forall a m.
  (Foldy a, TypeSpec a ~ NumSpec a, MonadGenError m, Random a, Integral a) =>
  Specification Integer -> Specification a -> Specification a -> GenT m [a]
genListWithSize sizeSpec elemSpec foldSpec
  | TrueSpec <- sizeSpec = genList elemSpec foldSpec
  | ErrorSpec _ <- sizeSpec <> geqSpec 0 =
      fatalError
        ( NE.fromList
            [ "genListWithSize called with possible negative size"
            , "  sizeSpec = " ++ specName sizeSpec
            , "  elemSpec = " ++ specName elemSpec
            , "  foldSpec = " ++ specName foldSpec
            ]
        )
  | otherwise = do
      total <- genFromSpecT foldSpec
      -- The compatible sizes for the list, for a given choice of total
      let sizeAdjusted =
            if total /= 0
              then sizeSpec <> gtSpec 0 -- if total is not zero, we better not pick a 0 size
              else
                if noNegativeValues @a
                  then sizeSpec <> equalSpec 0 -- if it is zero, and negative numbers not allowed, then only possible size is 0
                  else sizeSpec <> gtSpec 0
          message =
            [ "\nGenSizedList fails"
            , "sizespec = " ++ specName sizeSpec
            , "elemSpec = " ++ specName elemSpec
            , "foldSpec = " ++ specName foldSpec
            , "total choosen from foldSpec = " ++ show total
            , "size adjusted for total = " ++ show sizeAdjusted
            ]
      push message $ do
        count <- genFromSpecT sizeAdjusted
        case compare total 0 of
          EQ ->
            if count == 0
              then pure []
              else pickPositive elemSpec total count
          GT -> pickPositive elemSpec total count
          LT -> pickNegative elemSpec total count

pickPositive ::
  forall t m.
  (Integral t, Random t, MonadGenError m, TypeSpec t ~ NumSpec t, Foldy t) =>
  Specification t -> t -> Integer -> GenT m [t]
pickPositive elemspec total count = do
  sol <-
    pureGen $
      pickAll
        (minFromSpec 0 elemspec) -- Search from [0..total] unless elemspec says otherwise
        (maxFromSpec total elemspec)
        (predSpecPair elemspec)
        total
        (fromInteger count)
        (Cost 0)
  case snd sol of
    No msgs -> fatalError (NE.fromList msgs)
    Yes (x :| _) -> pure x

pickNegative ::
  forall t m.
  (Integral t, Random t, MonadGenError m, TypeSpec t ~ NumSpec t, HasSpec t) =>
  Specification t -> t -> Integer -> GenT m [t]

-- | total can be either negative, or 0. If it is 0, we want count numbers that add to zero
pickNegative elemspec total count = do
  sol <-
    pureGen $
      pickAll
        -- Recall 'total' is negative here.
        -- Here is a graphic of the range we search in (smallest .. largest)
        -- [(total+n) .. total .. 0 .. (0-n)],  where n = (total `div` 4) which is negative.
        (minFromSpec (total + (total `div` 4)) elemspec)
        (maxFromSpec (0 - (total `div` 4)) elemspec)
        (predSpecPair elemspec)
        total
        (fromInteger count)
        (Cost 0)
  case snd sol of
    No msgs -> fatalError (NE.fromList msgs)
    Yes (x :| _) -> pure x

specName :: forall a. HasSpec a => Specification a -> String
specName (ExplainSpec [x] _) = x
specName x = show x

predSpecPair :: forall a. HasSpec a => Specification a -> (String, a -> Bool)
predSpecPair spec = (specName spec, (`conformsToSpec` spec))

-- | The smallest number admitted by the spec, if we can find one.
--   if not return the defaultValue 'dv'
minFromSpec ::
  forall n.
  (Ord n, TypeSpec n ~ NumSpec n) =>
  n -> Specification n -> n
minFromSpec dv (ExplainSpec _ spec) = minFromSpec @n dv spec
minFromSpec dv TrueSpec = dv
minFromSpec dv s@(SuspendedSpec _ _) =
  case simplifySpec s of
    SuspendedSpec {} -> dv
    x -> minFromSpec @n dv x
minFromSpec dv (ErrorSpec _) = dv
minFromSpec _ (MemberSpec xs) = minimum xs
minFromSpec dv (TypeSpec (NumSpecInterval lo _) _) = maybe dv id lo

-- | The largest number admitted by the spec, if we can find one.
--   if not return the defaultValue 'dv'
maxFromSpec ::
  forall n.
  (Ord n, TypeSpec n ~ NumSpec n) =>
  n -> Specification n -> n
maxFromSpec dv (ExplainSpec _ spec) = maxFromSpec @n dv spec
maxFromSpec dv TrueSpec = dv
maxFromSpec dv s@(SuspendedSpec _ _) =
  case simplifySpec s of
    SuspendedSpec {} -> dv
    x -> maxFromSpec @n dv x
maxFromSpec dv (ErrorSpec _) = dv
maxFromSpec _ (MemberSpec xs) = maximum xs
maxFromSpec dv (TypeSpec (NumSpecInterval _ hi) _) = maybe dv id hi
