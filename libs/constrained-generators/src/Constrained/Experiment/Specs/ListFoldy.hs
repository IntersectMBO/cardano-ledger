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
module Constrained.Experiment.Specs.ListFoldy where

import Constrained.Core (Evidence (..), Var (..), eqVar, unionWithMaybe)
import Constrained.Experiment.Base
import Constrained.Experiment.Conformance (conformsToSpec, mapSpec, satisfies)
import Constrained.Experiment.NumSpec
import Constrained.Experiment.Specs.Size (Sized (..), genFromSizeSpec, sizeOf_)
import Constrained.Experiment.Syntax (forAll, genHint, unsafeExists)
import Constrained.Experiment.TheKnot (caseBoolSpec, genFromSpecT, shrinkWithSpec, simplifySpec)
import Constrained.Experiment.Witness (Witness (..), showType)

import Constrained.GenT
import Constrained.List
import Constrained.SumList (Cost (..), Solution (No, Yes), pickAll)
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Int
import Data.Kind
import Data.List (isPrefixOf, isSuffixOf, nub, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import Data.Word
import GHC.Stack
import GHC.TypeLits
import Prettyprinter hiding (cat)
import System.Random (Random)
import Test.QuickCheck (Arbitrary, shrinkList, shuffle)

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- So that's why the Foldy class is in the NumberSpec module.
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseWitness
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See FlipW amd ComposeW
-- ==============================================================

{-
-- We need Arbitrary Specification to do this
instance {-# OVERLAPPABLE #-} (Arbitrary (Specification a), Arbitrary (TypeSpec a), Foldy a) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec IdW <$> arbitrary, pure NoFold]
  shrink NoFold = []
  shrink (FoldSpec (sameFunSym (IdW @a) -> Just(idW,Refl,Refl,Refl,Refl,Refl)) spec) = FoldSpec idW <$> shrink spec
  shrink FoldSpec {} = [NoFold]
-}

data FunW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  IdW :: forall a. FunW () "id_" '[a] a
  ComposeW ::
    forall b c1 c2 s1 s2 t1 t2 a r.
    ( FunSym c1 s1 t1 '[b] r
    , FunSym c2 s2 t2 '[a] b
    , HasSpec b
    ) =>
    t1 c1 s1 '[b] r -> t2 c2 s2 '[a] b -> FunW (c1, c2) "composeFn" '[a] r
  FlipW ::
    forall c sym t a b r.
    FunSym c sym t '[a, b] r =>
    t c sym '[a, b] r -> FunW c "flip_" '[b, a] r

funSem :: c => FunW c sym dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\a -> semantics f (semantics g a))
funSem (FlipW (f :: g c s d r)) = flip (semantics f)

instance Witness FunW where
  semantics = funSem

instance KnownSymbol s => Show (FunW c s dom rng) where
  show IdW = "IdW[id_]"
  show (FlipW f) = "(FlipW " ++ show f ++ ")[flip_]"
  show (ComposeW x y) = "(ComposeW " ++ show x ++ " " ++ show y ++ ")[composeFn]"

instance Eq (FunW c s dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'

compareWit ::
  forall s1 t1 c1 c2 bs1 r1 s2 t2 bs2 r2.
  (FunSym c1 s1 t1 bs1 r1, FunSym c2 s2 t2 bs2 r2) =>
  t1 c1 s1 bs1 r1 -> t2 c2 s2 bs2 r2 -> Bool
compareWit x y = case (eqT @t1 @t2, eqT @c1 @c2, eqT @s1 @s2, eqT @bs1 @bs2, eqT @r1 @r2) of
  (Just Refl, Just Refl, Just Refl, Just Refl, Just Refl) -> x == y
  _ -> False

-- ===================================
-- FunSym instances for IdW, FlipW and ComposeW
-- Also their Haskell implementations id_ flip_ composeFn

instance HasSpec a => FunSym () "id_" FunW '[a] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context _ IdW (HOLE :<> End)) spec = spec
  propagate ctxt _ = ErrorSpec (NE.fromList ["IdW (id_)", "Unreachable context, too many args", show ctxt])

  mapTypeSpec IdW ts = typeSpec ts
  rewriteRules IdW (x :> Nil) Evidence = Just x

id_ :: forall a. HasSpec a => Term a -> Term a
id_ = appTerm IdW

instance
  (forall sym t. FunSym c sym t '[a, b] r, All Typeable [a, b, r]) =>
  FunSym c "flip_" FunW '[b, a] r
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context ev (FlipW f) (HOLE :<> v :<| End)) spec = propagate (Context ev f (v :|> HOLE :<> End)) spec
  propagate (Context ev (FlipW f) (v :|> HOLE :<> End)) spec = propagate (Context ev f (HOLE :<> v :<| End)) spec
  propagate ctxt@(Context Evidence _ _) _ = ErrorSpec (NE.fromList ["FlipW (flip_)", "Unreachable context, too many args", show ctxt])

  -- Note we need Evidence to apply App to f
  rewriteRules (FlipW f) (a@Lit {} :> b :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW f) (a :> b@Lit {} :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW {}) _ Evidence = Nothing

flip_ ::
  forall (c :: Constraint) (t :: FSType) (sym :: Symbol) a b r.
  (HasSpec b, HasSpec a, HasSpec r, c, forall c1 sym1 t1. FunSym c1 sym1 t1 '[a, b] r) =>
  t c sym '[a, b] r -> Term b -> Term a -> Term r
flip_ x = appTerm (FlipW x)

instance
  ( All Typeable [a, r]
  , Typeable c1
  , Typeable c2
  ) =>
  FunSym (c1, c2) "composeFn" FunW '[a] r
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence (ComposeW (f :: t1' c1' s1' '[b'] r') (g :: t2' c2' s2' '[a'] b'')) (HOLE :<> End)) spec =
    propagate @c2' @s2' @t2' @'[a'] @b' (Context (Evidence @c2') g (HOLE :<> End)) $
      propagate @c1' @s1' @t1' @'[b'] @r' (Context (Evidence @c1') f (HOLE :<> End)) spec
  propagate ctxt@(Context Evidence _ _) _ = ErrorSpec (NE.fromList ["ComposeW (composeFn)", "Unreachable context, too many args", show ctxt])

  mapTypeSpec (ComposeW g h) ts = mapSpec g . mapSpec h $ typeSpec ts

  -- Note we need the Evidence to apply App to f, and to apply App to g
  rewriteRules (ComposeW f g) (x :> Nil) Evidence = Just $ App f (App g (x :> Nil) :> Nil)

compose_ ::
  forall b c1 c2 s1 s2 t1 t2 a r.
  ( AppRequires c1 s1 t1 '[b] r
  , AppRequires c2 s2 t2 '[a] b
  ) =>
  t1 c1 s1 '[b] r -> t2 c2 s2 '[a] b -> Term a -> Term r
compose_ f g = appTerm $ ComposeW f g -- @b @c1 @c2 @s1 @s2 @t1 @t2 @a @r f g

-- =============================================================

eqFn :: forall a. (Typeable a, Eq a) => Fun '[a, a] Bool
eqFn = Fun (Evidence @(Eq a)) EqualW

composeFn :: HasSpec b => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
composeFn (Fun (Evidence :: Evidence x) f) (Fun (Evidence :: Evidence y) g) = (Fun (Evidence @(x, y)) (ComposeW f g))

-- flipFn :: forall a b r. (All HasSpec '[b, a], HasSpec r) => Fun '[a,b] r -> Fun '[b,a] r
-- flipFn (Fun (f :: t c s '[a',b'] r')) = Fun (FlipW (f :: t c s '[a',b'] r'))
-- flipFn (Fun f) = Fun (FlipW f)

idFn :: HasSpec a => Fun '[a] a
idFn = Fun (Evidence @()) IdW

-- =======================================================
-- All the Foldy class instances are intimately tied to
-- Numbers. But that is not required, but this is a
-- convenient place to put the code.
-- =======================================================

class (HasSpec a, NumLike a, FunSym (NumLike a) "addFn" IntW '[a, a] a, NumLike a) => Foldy a where
  genList ::
    MonadGenError m => Specification a -> Specification a -> GenT m [a]
  theAddFn :: IntW (NumLike a) "addFn" '[a, a] a
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
genInverse (Fun Evidence f) argS x =
  let argSpec' = argS <> propagate (Context Evidence f (HOLE :<> End)) (equalSpec x)
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
genFromFold must (simplifySpec -> size) elemS fun@(Fun Evidence fn) foldS
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
        let elemS' = mapSpec fn elemS
            mustVal = adds (map (semantics fn) must)
            foldS' = propagate (Context Evidence theAddFn (HOLE :<> mustVal :<| End)) foldS
            sizeSpec' = propagate (Context Evidence AddW (HOLE :<> (sizeOf must) :<| End)) size
        when (isErrorLike sizeSpec') $ genError1 "Inconsistent size spec"
        results0 <- genSizedList sizeSpec' (simplifySpec elemS') (simplifySpec foldS')
        results <-
          explain
            ( NE.fromList
                [ "genInverse"
                , "  fun = " ++ show fun
                , "  results0 = " ++ show results0
                , show $ "  elemS =" <+> pretty elemS
                ]
            )
            $ mapM (genInverse fun elemS) results0
        pureGen $ shuffle $ must ++ results

adds :: Foldy a => [a] -> a
adds = foldr (semantics theAddFn) theZero

addFun :: NumLike n => Fun '[n, n] n
addFun = Fun Evidence AddW

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
combineFoldSpec (FoldSpec (Fun ev f) s) (FoldSpec (Fun _ g) s') =
  case sameFunSym f g of
    Just (_h, Refl, Refl, Refl, Refl, Refl) -> pure $ FoldSpec (Fun ev f) (s <> s')
    Nothing -> Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show g]

conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec (Fun Evidence f) s) = adds (map (semantics f) xs) `conformsToSpec` s

-- =============================================================================
-- Lists and Foldy are mutually recursive
-- Here are HasSpec instance for List and the FunSym instances for List operators

data ListW (c :: Constraint) (s :: Symbol) (args :: [Type]) (res :: Type) where
  FoldMapW :: forall a b. HasSpec a => Fun '[a] b -> ListW (Foldy b) "foldMap_" '[[a]] b
  SingletonW :: ListW () "singelton_" '[a] [a]
  AppendW :: (Typeable a, Show a) => ListW () "append_" '[[a], [a]] [a]
  ElemW :: forall a. ListW (Eq a) "elem_" '[a, [a]] Bool

-- ====================== Witness for ListW

instance Witness ListW where
  semantics = listSem

listSem :: c => ListW c s dom rng -> FunTy dom rng
listSem (FoldMapW (Fun Evidence f)) = adds . map (semantics f)
listSem SingletonW = (: [])
listSem AppendW = (++)
listSem ElemW = elem

instance Show (ListW c s d r) where
  show AppendW = "append_"
  show SingletonW = "singleton_"
  show (FoldMapW n) = "(FoldMapW  " ++ show n ++ ")"
  show ElemW = "elem_"
deriving instance (Eq (ListW c s d r))

-- ============= FunSymbol for FoldMapW

instance (Typeable a, Typeable b) => FunSym (Foldy b) "foldMap_" ListW '[[a]] b where
  simplepropagate (Context Evidence (FoldMapW f) (HOLE :<> End)) spec =
    Right $ typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f spec)
  simplepropagate _ _ = Left (pure "Unreachable context for (FunSym (Foldy b) \"foldMap_\" ListW '[[a]] b)")
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
    toFn (App fn (V v' :> Nil)) | Just Refl <- eqVar v v' = Fun Evidence fn
    toFn (App fn (t :> Nil)) = composeFn (Fun Evidence fn) (toFn t)
    toFn (V v') | Just Refl <- eqVar v v' = idFn
    toFn _ = error "foldMap_ has not been given a function of the form \\ x -> f (g ... (h x))"

foldMapFn :: forall a b. (HasSpec a, Foldy b) => Fun '[a] b -> Fun '[[a]] b
foldMapFn f = Fun Evidence (FoldMapW f)

-- | Used in the HasSpec [a] instance
toPredsFoldSpec :: (HasSpec a, HasSpec [a]) => Term [a] -> FoldSpec a -> Pred
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec funAB sspec) =
  satisfies (appFun (foldMapFn funAB) x) sspec

-- ============ FunSymbol for ElemW

instance Typeable a => FunSym (Eq a) "elem_" ListW '[a, [a]] Bool where
  simplepropagate (Context Evidence ElemW (HOLE :<> es :<| End)) spec = Right $ caseBoolSpec spec $ \case
    True -> case nub es of
      [] -> ErrorSpec (pure "propagate on (elem_ x []), The empty list, [], has no solution")
      (z : zs) -> MemberSpec (z :| zs)
    False -> notMemberSpec es
  simplepropagate (Context Evidence ElemW (e :|> HOLE :<> End)) spec = Right $ caseBoolSpec spec $ \case
    True -> typeSpec $ ListSpec Nothing [e] mempty mempty NoFold
    False -> typeSpec $ ListSpec Nothing mempty mempty (notEqualSpec e) NoFold
  simplepropagate _ _ = Left (pure "Unreachable context for (FunSym (Eq a) \"elem_\" ListW  '[a, [a]] Bool)")

  rewriteRules ElemW (_ :> Lit [] :> Nil) Evidence = Just $ Lit False
  rewriteRules ElemW (t :> Lit [a] :> Nil) Evidence = Just $ t ==. Lit a
  rewriteRules _ _ _ = Nothing

elem_ :: (Sized [a], HasSpec a) => Term a -> Term [a] -> Term Bool
elem_ = appTerm ElemW

elemFn :: HasSpec a => Fun '[a, [a]] Bool
elemFn = Fun Evidence ElemW

-- ============= FunSymbol for SingletonW

instance Typeable a => FunSym () "singelton_" ListW '[a] [a] where
  simplepropagate (Context Evidence SingletonW (HOLE :<> End)) spec =
    case spec of
      MemberSpec xss ->
        case [a | [a] <- NE.toList xss] of
          [] -> Left $ (pure "PropagateSpec SingletonW  with MemberSpec which has no lists of length 1")
          (x : xs) -> Right $ MemberSpec (x :| xs)
      TypeSpec (ListSpec _ m sz e f) cant
        | length m > 1 ->
            Left $
              NE.fromList
                [ "Too many required elements for SingletonW : "
                , "  " ++ show m
                ]
        | not $ 1 `conformsToSpec` sz ->
            Left $ pure $ "Size spec requires too many elements for SingletonW : " ++ show sz
        | bad@(_ : _) <- filter (not . (`conformsToSpec` e)) m ->
            Left $
              NE.fromList
                [ "The following elements of the must spec do not conforms to the elem spec:"
                , show bad
                ]
        -- There is precisely one required element in the final list, so the argument to singletonList_ has to
        -- be that element and we have to respect the cant and fold specs
        | [a] <- m -> Right $ equalSpec a <> notMemberSpec [z | [z] <- cant] <> reverseFoldSpec f
        -- We have to respect the elem-spec, the can't spec, and the fold spec.
        | otherwise -> Right $ e <> notMemberSpec [a | [a] <- cant] <> reverseFoldSpec f
      _ -> Left (pure "Unreachable Specification in simplepropagate singletonW FunSym instance")
  simplepropagate ctx@(Context Evidence _ _) _ = Left (pure $ "Ill formed context in singletonW FunSym instance: " ++ show ctx)

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec SingletonW ts = typeSpec (ListSpec Nothing [] (equalSpec 1) (typeSpec ts) NoFold)

reverseFoldSpec :: FoldSpec a -> Specification a
reverseFoldSpec NoFold = TrueSpec
-- The single element list has to sum to something that obeys spec, i.e. `conformsToSpec (f a) spec`
reverseFoldSpec (FoldSpec (Fun Evidence fn) spec) = propagate (Context Evidence fn (HOLE :<> End)) spec

singleton_ :: (Sized [a], HasSpec a) => Term a -> Term [a]
singleton_ = appTerm SingletonW

singletonFn :: forall a. HasSpec a => Fun '[a] [a]
singletonFn = Fun Evidence SingletonW

-- ============== FunSymbol for AppendW

instance (Sized [a], Typeable a) => FunSym () "append_" ListW '[[a], [a]] [a] where
  simplepropagate ctx spec = case spec of
    MemberSpec xss
      | Context Evidence AppendW (HOLE :<> (ys :: [a]) :<| End) <- ctx
      , Evidence <- prerequisites @[a] ->
          -- Only keep the prefixes of the elements of xss that can
          -- give you the correct resulting list
          case suffixedBy ys (NE.toList xss) of
            [] ->
              Left
                ( NE.fromList
                    [ "propagateSpecFun (append HOLE ys) with (MemberSpec xss)"
                    , "there are no elements in xss with suffix ys"
                    ]
                )
            (x : xs) -> Right $ MemberSpec (x :| xs)
      | Context Evidence AppendW ((ys :: [a]) :|> HOLE :<> End) <- ctx
      , Evidence <- prerequisites @[a] ->
          -- Only keep the suffixes of the elements of xss that can
          -- give you the correct resulting list
          case prefixedBy ys (NE.toList xss) of
            [] ->
              Left
                ( NE.fromList
                    [ "propagateSpecFun (append ys HOLE) with (MemberSpec xss)"
                    , "there are no elements in xss with prefix ys"
                    ]
                )
            (x : xs) -> Right (MemberSpec (x :| xs))
    TypeSpec ts@ListSpec {listSpecElem = e} cant
      | Context Evidence AppendW (HOLE :<> (ys :: [a]) :<| End) <- ctx
      , Evidence <- prerequisites @[a]
      , all (`conformsToSpec` e) ys ->
          Right $
            TypeSpec (alreadyHave ys ts) (suffixedBy ys cant)
      | Context Evidence AppendW ((ys :: [a]) :|> HOLE :<> End) <- ctx
      , Evidence <- prerequisites @[a]
      , all (`conformsToSpec` e) ys ->
          Right $
            TypeSpec (alreadyHave ys ts) (prefixedBy ys cant)
    _ -> Left $ pure "The spec given to propagateSpecFun AppendSpec is inconsistent!"

prefixedBy :: Eq a => [a] -> [[a]] -> [[a]]
prefixedBy ys xss = [drop (length ys) xs | xs <- xss, ys `isPrefixOf` xs]

suffixedBy :: Eq a => [a] -> [[a]] -> [[a]]
suffixedBy ys xss = [take (length xs - length ys) xs | xs <- xss, ys `isSuffixOf` xs]

alreadyHave :: (Eq a, Typeable a, Show a) => [a] -> ListSpec a -> ListSpec a
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

alreadyHaveFold :: (Typeable a, Show a) => [a] -> FoldSpec a -> FoldSpec a
alreadyHaveFold _ NoFold = NoFold
alreadyHaveFold ys (FoldSpec fn spec) =
  FoldSpec
    fn
    (constrained $ \s -> appTerm theAddFn s (foldMap_ (appFun fn) (Lit ys)) `satisfies` spec)

appendFn :: forall a. (Sized [a], HasSpec a) => Fun '[[a], [a]] [a]
appendFn = Fun Evidence AppendW

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
  , TypeSpec a ~ NumSpec a
  , Arbitrary a
  , Integral a
  , MaybeBounded a
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
                let foldS' = propagate (Context Evidence theAddFn (HOLE :<> x :<| End)) foldS
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
  (Foldy a, MonadGenError m, Random a, Integral a, TypeSpec a ~ NumSpec a) =>
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
  | otherwise =
      let message =
            [ "\nGenSizedList fails"
            , "sizespec = " ++ specName sizeSpec
            , "elemSpec = " ++ specName elemSpec
            , "foldSpec = " ++ specName foldSpec
            , "size adjusted = " ++ show (sizeSpec <> geqSpec 0)
            ]
       in push message $ do
            count <- genFromSpecT (sizeSpec <> geqSpec 0) -- Sizes cannot be negative.
            total <- genFromSpecT foldSpec
            case compare total 0 of
              EQ ->
                if conformsToSpec 0 sizeSpec
                  then pure []
                  else
                    if noNegativeValues @a
                      then
                        fatalError1
                          ("Can't find a non-zero number of things that add to 0 at type " ++ showType @a)
                      else pickNegative elemSpec total count
              GT -> pickPositive elemSpec total count
              LT -> pickNegative elemSpec total count

pickPositive ::
  forall t m.
  (Integral t, Random t, MonadGenError m, Foldy t, TypeSpec t ~ NumSpec t) =>
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
