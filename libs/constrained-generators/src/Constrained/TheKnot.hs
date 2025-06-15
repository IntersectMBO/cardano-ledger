{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

-- | All the things that are mutually recursive.
module Constrained.TheKnot where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generation
import Constrained.Generic
import Constrained.List
import Constrained.NumOrd (
  IntW (..),
  NumLike,
  NumSpec (..),
  Numeric,
  cardinality,
  caseBoolSpec,
  geqSpec,
  leqSpec,
  (<=.),
 )
import Constrained.PrettyUtils
import Constrained.SumList
-- TODO: some strange things here, why is SolverStage in here?!
-- Because it is mutually recursive with something else in here.
import Constrained.Syntax
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Int
import Data.Kind
import Data.List (isPrefixOf, isSuffixOf, nub, (\\))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Word
import GHC.Natural
import GHC.Stack
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, Witness, forAll, witness)
import Prelude hiding (cycle, pred)

-- ===================================================================================
-- We call this module TheKnot because it binds a mutually recursive set of
-- things The heart of TheKNot is "genFromSpecT" the user interface to
-- generating random instances of a Spec.  It is mutually recursive with the 3
-- simplest HasSpec instances (Bool,Integer,Sum), and 'simplifySpec'.  Every
-- HasSpec instance is dependant on HasSpec Integer because the Cardinality
-- properties are expressed in terms of Integer. Generic HasSpec instances are
-- implemented in terms of a Sum of Product Simple Rep. And every HasSpec
-- instance has a genFromTypeSpec method, on which GenFromSpecT depends. There
-- is no avoiding the Knot.  The only saving grace, is that genFromTypeSpec is
-- a HasSpec method, so new things depending only on things defined here, or
-- things defined in the same file as the the HasSpec instance can escape from
-- TheKnot.
--
-- Here is a graph of the dependencies.
--
--      +---->HasSpec Integer
--      |                   ^
--      |                    \
--      |                     \
--      |                      HasSpec Sum
--      |                           /   ^
--      |                          /    |
--      <.                        /     |
--      <=.                      /      |
--      |                       v       |
--      |               genFromSpecT    |
--      |                     |         |
--      |                     |         |
--      +-------caseBoolSpec  |    caseSum
--                      ^     |    ^
--                      |     |    |
--                      |     v    |
--                     simplifySpec
--
-- ===============================================================================
-- Things left TODO
-- 1) Use of UnionPat below requires HasSpec(Set a) instance

-- ======= Logic instance OrW(or_)

-- ======= Logic instance EqualW(==.)  CAN WE MOVE THIS OUT OF TheKnot?
instance Numeric a => Complete a where
  simplifyA = simplifySpec
  genFromSpecA = genFromSpecT

-- | If the `Specification Bool` doesn't constrain the boolean you will get a `TrueSpec` out.
ifElse :: (IsPred p, IsPred q) => Term Bool -> p -> q -> Pred
ifElse b p q = whenTrue b p <> whenTrue (not_ b) q

-- --------------- Simplification of Sum types --------------------

-- =======================================================================================

-- | Functor like property for Specification, but instead of a Haskell function (a -> b),
--   it takes a function symbol (t '[a] b) from a to b.
--   Note, in this context, a function symbol is some constructor of a witnesstype.
--   Eg. ProdFstW, InjRightW, SingletonW, etc. NOT the lifted versions like fst_ singleton_,
--   which construct Terms. We had to wait until here to define this because it
--   depends on Semigroup property of Specification, and Asserting equality
mapSpec ::
  forall t a b.
  AppRequires t '[a] b =>
  t '[a] b ->
  Specification a ->
  Specification b
mapSpec f (ExplainSpec es s) = explainSpecOpt es (mapSpec f s)
mapSpec f TrueSpec = mapTypeSpec f (emptySpec @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ NE.nub $ fmap (semantics f) as
mapSpec f (SuspendedSpec x p) =
  constrained $ \x' ->
    Exists (\_ -> fatalError "mapSpec") (x :-> fold [Assert $ (x' ==. appTerm f (V x)), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec f ts <> notMemberSpec (map (semantics f) cant)

-- ================================================================
-- HasSpec for Products
-- ================================================================

pairView :: Term (Prod a b) -> Maybe (Term a, Term b)
pairView (App (getWitness -> Just ProdW) (x :> y :> Nil)) = Just (x, y)
pairView _ = Nothing

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
    satisfies (prodFst_ x) sf
      <> satisfies (prodSnd_ x) ss

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

-- ==================================================
-- Logic instances for Prod
-- ==================================================

-- ========= ProdFstW

data ProdW :: [Type] -> Type -> Type where
  ProdW :: (HasSpec a, HasSpec b) => ProdW '[a, b] (Prod a b)
  ProdFstW :: (HasSpec a, HasSpec b) => ProdW '[Prod a b] a
  ProdSndW :: (HasSpec a, HasSpec b) => ProdW '[Prod a b] b

deriving instance Eq (ProdW as b)

deriving instance Show (ProdW as b)

instance Syntax ProdW

instance Semantics ProdW where
  semantics ProdW = Prod
  semantics ProdFstW = prodFst
  semantics ProdSndW = prodSnd

instance Logic ProdW where
  propagateTypeSpec ProdFstW (Unary HOLE) ts cant = cartesian (TypeSpec ts cant) TrueSpec
  propagateTypeSpec ProdSndW (Unary HOLE) ts cant =
    cartesian TrueSpec (TypeSpec ts cant)
  propagateTypeSpec ProdW (a :>: HOLE) sc@(Cartesian sa sb) cant
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show (TypeSpec sc cant)]
          )
  propagateTypeSpec ProdW (HOLE :<: b) sc@(Cartesian sa sb) cant
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show (TypeSpec sc cant)]
          )

  propagateMemberSpec ProdFstW (Unary HOLE) es = cartesian (MemberSpec es) TrueSpec
  propagateMemberSpec ProdSndW (Unary HOLE) es = cartesian TrueSpec (MemberSpec es)
  propagateMemberSpec ProdW (a :>: HOLE) es =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagateMemberSpec ProdW (HOLE :<: b) es =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]

  rewriteRules ProdFstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules ProdSndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec ProdFstW (Cartesian s _) = s
  mapTypeSpec ProdSndW (Cartesian _ s) = s

prodFst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
prodFst_ = appTerm ProdFstW

prodSnd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
prodSnd_ = appTerm ProdSndW

-- ========= ProdW
sameFst :: Eq a1 => a1 -> [Prod a1 a2] -> [a2]
sameFst a ps = [b | Prod a' b <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [Prod a2 a1] -> [a2]
sameSnd b ps = [a | Prod a b' <- ps, b == b']

prod_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (Prod a b)
prod_ = appTerm ProdW

pattern Product ::
  forall c.
  () =>
  forall a b.
  ( c ~ Prod a b
  , AppRequires ProdW '[a, b] (Prod a b)
  ) =>
  Term a ->
  Term b ->
  Term c
pattern Product x y <- (App (getWitness -> Just ProdW) (x :> y :> Nil))

-- =================================================
-- CAN WE MOVE THIS OUT OF TheKnot?

data ElemW :: [Type] -> Type -> Type where
  ElemW :: HasSpec a => ElemW '[a, [a]] Bool

deriving instance Eq (ElemW dom rng)

instance Show (ElemW dom rng) where
  show ElemW = "elem_"

instance Syntax ElemW

instance Semantics ElemW where
  semantics ElemW = elem

instance Logic ElemW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate ElemW (HOLE :<: (x :: [w])) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ElemW ((v' :: Term w) :> Lit x :> Nil)) (v :-> ps)
  propagate ElemW (x :>: HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ElemW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate ElemW (HOLE :<: es) spec =
    caseBoolSpec spec $ \case
      True -> memberSpecList (nub es) (pure "propagate on (elem_ x []), The empty list, [], has no solution")
      False -> notMemberSpec es
  propagate ElemW (e :>: HOLE) spec =
    caseBoolSpec spec $ \case
      True -> typeSpec (ListSpec Nothing [e] mempty mempty NoFold)
      False -> typeSpec (ListSpec Nothing mempty mempty (notEqualSpec e) NoFold)

  rewriteRules ElemW (_ :> Lit [] :> Nil) Evidence = Just $ Lit False
  rewriteRules ElemW (t :> Lit [a] :> Nil) Evidence = Just $ t ==. (Lit a)
  rewriteRules _ _ _ = Nothing

  saturate ElemW ((FromGeneric (Product (x :: Term a) (y :: Term b)) :: Term c) :> Lit zs :> Nil)
    | Just Refl <- eqT @c @(a, b) = case zs of
        (w : ws) -> [ElemPred True x (fmap fst (w :| ws))]
        [] -> [FalsePred (pure $ "empty list, zs , in elem_ " ++ show (x, y) ++ " zs")]
    | otherwise = []
  saturate ElemW (x :> Lit (y : ys) :> Nil) = [satisfies x (MemberSpec (y :| ys))]
  saturate _ _ = []

infix 4 `elem_`

elem_ :: (Sized [a], HasSpec a) => Term a -> Term [a] -> Term Bool
elem_ = appTerm ElemW

elemFn :: HasSpec a => Fun '[a, [a]] Bool
elemFn = Fun ElemW

pattern Elem ::
  forall b.
  () =>
  forall a.
  (b ~ Bool, Eq a, HasSpec a) =>
  Term a ->
  Term [a] ->
  Term b
pattern Elem x y <-
  ( App
      (getWitness -> Just ElemW)
      (x :> y :> Nil)
    )

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
        genError "genTypeSpecSpec @ListSpec: some elements of mustSet do not conform to elemS"
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
    sizeOf xs
      `conformsToSpec` size
      && all (`elem` xs) must
      && all (`conformsToSpec` elemS) xs
      && xs
        `conformsToFoldSpec` foldS

  toPreds x (ListSpec msz must size elemS foldS) =
    (forAll x $ \x' -> satisfies x' elemS)
      <> (forAll (Lit must) $ \x' -> Assert (elem_ x' x))
      <> toPredsFoldSpec x foldS
      <> satisfies (sizeOf_ x) size
      <> maybe TruePred (flip genHint x) msz

sizeOf_ :: (HasSpec a, Sized a) => Term a -> Term Integer
sizeOf_ = curryList (App SizeOfW)

-- | Because Sizes should always be >= 0, We provide this alternate generator
--   that can be used to replace (genFromSpecT @Integer), to ensure this important property
genFromSizeSpec :: MonadGenError m => Specification Integer -> GenT m Integer
genFromSizeSpec integerSpec = genFromSpecT (integerSpec <> geqSpec 0)

instance (Sized [a], HasSpec a) => HasGenHint [a] where
  type Hint [a] = Integer
  giveHint szHint = typeSpec $ ListSpec (Just szHint) [] mempty mempty NoFold

instance Forallable [a] a where
  fromForAllSpec es = typeSpec (ListSpec Nothing [] mempty es NoFold)
  forAllToList = id

-- =====================================================================
-- Syntax, Semantics and Logic instances for function symbols on List

data ListW (args :: [Type]) (res :: Type) where
  FoldMapW :: forall a b. (Foldy b, HasSpec a) => Fun '[a] b -> ListW '[[a]] b
  SingletonListW :: HasSpec a => ListW '[a] [a]
  AppendW :: (HasSpec a, Typeable a, Show a) => ListW '[[a], [a]] [a]

instance Semantics ListW where
  semantics = listSem

instance Syntax ListW where
  prettySymbol AppendW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> short n <+> prettyPrec 10 y
  prettySymbol AppendW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> prettyPrec 10 y <+> short n
  prettySymbol _ _ _ = Nothing

listSem :: ListW dom rng -> FunTy dom rng
listSem (FoldMapW (Fun f)) = adds . map (semantics f)
listSem SingletonListW = (: [])
listSem AppendW = (++)

instance Show (ListW d r) where
  show AppendW = "append_"
  show SingletonListW = "singletonList_"
  show (FoldMapW n) = "(FoldMapW  " ++ show n ++ ")"

deriving instance (Eq (ListW d r))

instance Logic ListW where
  propagateTypeSpec (FoldMapW f) (Unary HOLE) ts cant =
    typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f (TypeSpec ts cant))
  propagateTypeSpec SingletonListW (Unary HOLE) (ListSpec _ m sz e f) cant
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
  propagateTypeSpec AppendW ctx (ts@ListSpec {listSpecElem = e}) cant
    | (HOLE :? Value (ys :: [a]) :> Nil) <- ctx
    , Evidence <- prerequisites @[a]
    , all (`conformsToSpec` e) ys =
        TypeSpec (alreadyHave ys ts) (suffixedBy ys cant)
    | (Value (ys :: [a]) :! Unary HOLE) <- ctx
    , Evidence <- prerequisites @[a]
    , all (`conformsToSpec` e) ys =
        TypeSpec (alreadyHave ys ts) (prefixedBy ys cant)
    | otherwise = ErrorSpec $ pure "The spec given to propagate for AppendW is inconsistent!"

  propagateMemberSpec (FoldMapW f) (Unary HOLE) es =
    typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f (MemberSpec es))
  propagateMemberSpec SingletonListW (Unary HOLE) xss =
    case [a | [a] <- NE.toList xss] of
      [] ->
        ErrorSpec $ (pure "PropagateSpec SingletonListW  with MemberSpec which has no lists of length 1")
      (x : xs) -> MemberSpec (x :| xs)
  propagateMemberSpec AppendW ctx xss
    | (HOLE :<: (ys :: [a])) <- ctx
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
    | ((ys :: [a]) :>: HOLE) <- ctx
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

  mapTypeSpec SingletonListW ts = typeSpec (ListSpec Nothing [] (equalSpec 1) (typeSpec ts) NoFold)
  mapTypeSpec (FoldMapW g) ts =
    constrained $ \x ->
      unsafeExists $ \x' ->
        Assert (x ==. appFun (foldMapFn g) x') <> toPreds x' ts

foldMap_ :: forall a b. (Foldy b, HasSpec a) => (Term a -> Term b) -> Term [a] -> Term b
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

-- function symbol definitions for List
sum_ ::
  Foldy a =>
  Term [a] ->
  Term a
sum_ = foldMap_ id

singletonList_ :: (Sized [a], HasSpec a) => Term a -> Term [a]
singletonList_ = appTerm SingletonListW

append_ :: (Sized [a], HasSpec a) => Term [a] -> Term [a] -> Term [a]
append_ = appTerm AppendW

-- Fun types for lists and their helper functions

appendFn :: forall a. (Sized [a], HasSpec a) => Fun '[[a], [a]] [a]
appendFn = Fun AppendW

singletonListFn :: forall a. HasSpec a => Fun '[a] [a]
singletonListFn = Fun SingletonListW

foldMapFn :: forall a b. (HasSpec a, Foldy b) => Fun '[a] b -> Fun '[[a]] b
foldMapFn f = Fun (FoldMapW f)

reverseFoldSpec :: FoldSpec a -> Specification a
reverseFoldSpec NoFold = TrueSpec
-- The single element list has to sum to something that obeys spec, i.e. `conformsToSpec (f a) spec`
reverseFoldSpec (FoldSpec (Fun fn) spec) = propagate fn (HOLE :? Nil) spec

-- ==============  Helper functions

prefixedBy :: Eq a => [a] -> [[a]] -> [[a]]
prefixedBy ys xss = [drop (length ys) xs | xs <- xss, ys `isPrefixOf` xs]

suffixedBy :: Eq a => [a] -> [[a]] -> [[a]]
suffixedBy ys xss = [take (length xs - length ys) xs | xs <- xss, ys `isSuffixOf` xs]

alreadyHave :: Eq a => [a] -> ListSpec a -> ListSpec a
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

alreadyHaveFold :: [a] -> FoldSpec a -> FoldSpec a
alreadyHaveFold _ NoFold = NoFold
alreadyHaveFold ys (FoldSpec fn spec) =
  FoldSpec
    fn
    (constrained $ \s -> appTerm theAddFn s (foldMap_ (appFun fn) (Lit ys)) `satisfies` spec)

-- | Used in the HasSpec [a] instance
toPredsFoldSpec :: HasSpec a => Term [a] -> FoldSpec a -> Pred
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec funAB sspec) =
  satisfies (appFun (foldMapFn funAB) x) sspec

-- =======================================================
-- FoldSpec is a Spec that appears inside of ListSpec

data FoldSpec a where
  NoFold :: FoldSpec a
  FoldSpec ::
    forall b a.
    ( HasSpec a
    , HasSpec b
    , Foldy b
    ) =>
    Fun '[a] b ->
    Specification b ->
    FoldSpec a

preMapFoldSpec :: HasSpec a => Fun '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s

composeFn :: (HasSpec b, HasSpec c) => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
composeFn (Fun f) (Fun g) = (Fun (ComposeW f g))

idFn :: HasSpec a => Fun '[a] a
idFn = Fun IdW

combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (Fun f) s) (FoldSpec (Fun g) s') =
  case sameFunSym f g of
    Just (_, _, Refl) -> pure $ FoldSpec (Fun f) (s <> s')
    Nothing -> Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show g]

conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec (Fun f) s) = adds (map (semantics f) xs) `conformsToSpec` s

class (HasSpec a, NumLike a, Logic IntW) => Foldy a where
  genList ::
    MonadGenError m => Specification a -> Specification a -> GenT m [a]
  theAddFn :: IntW '[a, a] a
  theAddFn = AddW
  theZero :: a
  theZero = 0
  genSizedList ::
    MonadGenError m =>
    Specification Integer ->
    Specification a ->
    Specification a ->
    GenT m [a]
  noNegativeValues :: Bool

-- ================
-- Sized
-- ================

type SizeSpec = NumSpec Integer

class Sized t where
  sizeOf :: t -> Integer
  default sizeOf :: (HasSimpleRep t, Sized (SimpleRep t)) => t -> Integer
  sizeOf = sizeOf . toSimpleRep

  liftSizeSpec :: HasSpec t => SizeSpec -> [Integer] -> Specification t
  default liftSizeSpec ::
    ( Sized (SimpleRep t)
    , GenericRequires t
    ) =>
    SizeSpec ->
    [Integer] ->
    Specification t
  liftSizeSpec sz cant = fromSimpleRepSpec $ liftSizeSpec sz cant

  liftMemberSpec :: HasSpec t => [Integer] -> Specification t
  default liftMemberSpec ::
    ( Sized (SimpleRep t)
    , GenericRequires t
    ) =>
    [Integer] ->
    Specification t
  liftMemberSpec = fromSimpleRepSpec . liftMemberSpec

  sizeOfTypeSpec :: HasSpec t => TypeSpec t -> Specification Integer
  default sizeOfTypeSpec ::
    ( HasSpec (SimpleRep t)
    , Sized (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    TypeSpec t ->
    Specification Integer
  sizeOfTypeSpec = sizeOfTypeSpec @(SimpleRep t)

adds :: Foldy a => [a] -> a
adds = foldr (semantics theAddFn) theZero

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseW
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See FlipW amd ComposeW
-- ==============================================================

data FunW (dom :: [Type]) (rng :: Type) where
  IdW :: forall a. FunW '[a] a
  ComposeW ::
    forall b t1 t2 a r.
    ( AppRequires t1 '[b] r
    , AppRequires t2 '[a] b
    , HasSpec b
    ) =>
    t1 '[b] r ->
    t2 '[a] b ->
    FunW '[a] r
  FlipW ::
    forall t a b r.
    AppRequires t '[a, b] r =>
    t '[a, b] r ->
    FunW '[b, a] r

funSem :: FunW dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\a -> semantics f (semantics g a))
funSem (FlipW (f :: g d r)) = flip (semantics f)

instance Semantics FunW where
  semantics = funSem

instance Syntax FunW

instance Show (FunW dom rng) where
  show IdW = "id_"
  show (FlipW f) = "(flip_ " ++ show f ++ ")"
  show (ComposeW x y) = "(compose_ " ++ show x ++ " " ++ show y ++ ")"

instance Eq (FunW dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'
  _ == _ = False

compareWit ::
  forall t1 bs1 r1 t2 bs2 r2.
  (AppRequires t1 bs1 r1, AppRequires t2 bs2 r2) =>
  t1 bs1 r1 ->
  t2 bs2 r2 ->
  Bool
compareWit x y = case (eqT @t1 @t2, eqT @bs1 @bs2, eqT @r1 @r2) of
  (Just Refl, Just Refl, Just Refl) -> x == y
  _ -> False

-- ===================================
-- Logic instances for IdW, FlipW and ComposeW
-- Also their Haskell implementations id_ flip_ composeFn

instance Logic FunW where
  propagate IdW (Unary HOLE) = id
  propagate (FlipW f) ctx = propagate f (flipCtx ctx)
  propagate (ComposeW f g) (Unary HOLE) = propagate g (Unary HOLE) . propagate f (Unary HOLE)

  mapTypeSpec IdW ts = typeSpec ts
  mapTypeSpec (ComposeW g h) ts = mapSpec g . mapSpec h $ typeSpec ts

  -- Note we need the Evidence to apply App to f, and to apply App to g
  rewriteRules (ComposeW f g) (x :> Nil) Evidence = Just $ App f (App g (x :> Nil) :> Nil)
  rewriteRules IdW (x :> Nil) Evidence = Just x
  rewriteRules (FlipW f) (a@Lit {} :> b :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW f) (a :> b@Lit {} :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW {}) _ Evidence = Nothing

id_ :: forall a. HasSpec a => Term a -> Term a
id_ = appTerm IdW

--   -- Note we need Evidence to apply App to f

flip_ ::
  forall (t :: [Type] -> Type -> Type) a b r.
  (HasSpec b, HasSpec a, AppRequires t '[a, b] r) =>
  t '[a, b] r ->
  Term b ->
  Term a ->
  Term r
flip_ x = appTerm (FlipW x)

compose_ ::
  forall b t1 t2 a r.
  ( AppRequires t1 '[b] r
  , AppRequires t2 '[a] b
  ) =>
  t1 '[b] r ->
  t2 '[a] b ->
  Term a ->
  Term r
compose_ f g = appTerm $ ComposeW f g -- @b @c1 @c2 @s1 @s2 @t1 @t2 @a @r f g

-- =======================================================
-- The Foldy class instances for Numbers
-- =======================================================

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
  let argSpec' = argS <> propagate f (HOLE :? Nil) (equalSpec x)
   in explainNE
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
      fatalErrorNE (NE.cons "genFromFold has ErrorLike sizeSpec" (errorLikeMessage size))
  | isErrorLike elemS =
      fatalErrorNE (NE.cons "genFromFold has ErrorLike elemSpec" (errorLikeMessage elemS))
  | isErrorLike foldS =
      fatalErrorNE (NE.cons "genFromFold has ErrorLike totalSpec" (errorLikeMessage foldS))
  | otherwise = ( explainNE
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
            foldS' = propagate theAddFn (HOLE :? Value mustVal :> Nil) foldS
            sizeSpec' :: Specification Integer
            sizeSpec' = propagate AddW (HOLE :? Value (sizeOf must) :> Nil) size
        when (isErrorLike sizeSpec') $ genError "Inconsistent size spec"
        results0 <- case sizeSpec' of
          TrueSpec -> genList (simplifySpec elemS') (simplifySpec foldS')
          _ -> genSizedList sizeSpec' (simplifySpec elemS') (simplifySpec foldS')
        results <-
          explainNE
            ( NE.fromList
                [ "genInverse"
                , "  fun = " ++ show fun
                , "  results0 = " ++ show results0
                , show $ "  elemS' =" <+> pretty elemS'
                ]
            )
            $ mapM (genInverse fun elemS) results0
        pureGen $ shuffle $ must ++ results

addFun :: NumLike n => Fun '[n, n] n
addFun = Fun AddW

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

-- ======================================================================
-- Size and its 'generic' operations over Sized types.
-- ======================================================================

data SizeW (dom :: [Type]) rng :: Type where
  SizeOfW :: (Sized n, HasSpec n) => SizeW '[n] Integer

deriving instance Eq (SizeW ds r)

instance Show (SizeW d r) where
  show SizeOfW = "sizeOf_"

instance Semantics SizeW where
  semantics SizeOfW = sizeOf -- From the Sized class.

instance Syntax SizeW

instance Logic SizeW where
  propagateTypeSpec SizeOfW (Unary HOLE) ts cant = liftSizeSpec ts cant

  propagateMemberSpec SizeOfW (Unary HOLE) es = liftMemberSpec (NE.toList es)

  mapTypeSpec (SizeOfW :: SizeW '[a] b) ts =
    constrained $ \x ->
      unsafeExists $ \x' -> Assert (x ==. sizeOf_ x') <> toPreds @a x' ts

sizeOfFn :: forall a. (HasSpec a, Sized a) => Fun '[a] Integer
sizeOfFn = Fun SizeOfW

-- ======================================

rangeSize :: Integer -> Integer -> SizeSpec
rangeSize a b | a < 0 || b < 0 = error ("Negative Int in call to rangeSize: " ++ show a ++ " " ++ show b)
rangeSize a b = NumSpecInterval (Just a) (Just b)

between :: (HasSpec a, TypeSpec a ~ NumSpec a) => a -> a -> Specification a
between lo hi = TypeSpec (NumSpecInterval (Just lo) (Just hi)) []

-- | The widest interval whose largest element is admitted by the original spec
maxSpec :: Specification Integer -> Specification Integer
maxSpec (ExplainSpec es s) = explainSpecOpt es (maxSpec s)
maxSpec TrueSpec = TrueSpec
maxSpec s@(SuspendedSpec _ _) =
  constrained $ \x -> unsafeExists $ \y -> [y `satisfies` s, Explain (pure "maxSpec on SuspendedSpec") $ Assert (x <=. y)]
maxSpec (ErrorSpec xs) = ErrorSpec xs
maxSpec (MemberSpec xs) = leqSpec (maximum xs)
maxSpec (TypeSpec (NumSpecInterval _ hi) bad) = TypeSpec (NumSpecInterval Nothing hi) bad

-- How to constrain the size of any type, with a Sized instance
hasSize :: (HasSpec t, Sized t) => SizeSpec -> Specification t
hasSize sz = liftSizeSpec sz []
