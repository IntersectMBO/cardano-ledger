{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | `TypeSpec` definition for `[]` and functions for writing constraints over
-- lists
module Constrained.Spec.List (
  ListSpec (..),
  ListW (..),
  ElemW (..),
  pattern Elem,

  -- * Functions for writing constraints on lists
  append_,
  singletonList_,
  elem_,
  sum_,
  foldMap_,

  -- * FoldSpec and Foldy definitions and helper functions
  Foldy (..),
  FoldSpec (..),
  preMapFoldSpec,
  toPredsFoldSpec,
  adds,
  conformsToFoldSpec,
  combineFoldSpec,
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generation
import Constrained.Generic
import Constrained.List
import Constrained.NumOrd
import Constrained.PrettyUtils
import Constrained.SumList
import Constrained.Syntax
import Constrained.TheKnot
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

-- | `TypeSpec` for `[]`
data ListSpec a = ListSpec
  { listSpecHint :: Maybe Integer
  -- ^ Hint for the length of the list
  , listSpecMust :: [a]
  -- ^ Things that must be in the list
  , listSpecSize :: Specification Integer
  -- ^ Spec for the size of the list
  , listSpecElem :: Specification a
  -- ^ Spec for every element
  , listSpecFold :: FoldSpec a
  -- ^ Spec for the sum (or fold) of the list
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

-- | Witness type for `elem_`
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
      True -> memberSpec (nub es) (pure "propagate on (elem_ x []), The empty list, [], has no solution")
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

-- | Check if a term is an element of the list
elem_ :: HasSpec a => Term a -> Term [a] -> Term Bool
elem_ = appTerm ElemW

-- | Pattern for extracting the v`ElemW` symbol, useful for writing custom
-- rewrite rules for functions that deal with lists
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

instance HasSpec a => HasSpec [a] where
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

instance HasSpec a => HasGenHint [a] where
  type Hint [a] = Integer
  giveHint szHint = typeSpec $ ListSpec (Just szHint) [] mempty mempty NoFold

instance Forallable [a] a where
  fromForAllSpec es = typeSpec (ListSpec Nothing [] mempty es NoFold)
  forAllToList = id

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

-- | Function symbols for talking about lists
data ListW (args :: [Type]) (res :: Type) where
  FoldMapW :: forall a b. (Foldy b, HasSpec a) => Fun '[a] b -> ListW '[[a]] b
  SingletonListW :: HasSpec a => ListW '[a] [a]
  AppendW :: (HasSpec a, Typeable a, Show a) => ListW '[[a], [a]] [a]

instance Semantics ListW where
  semantics (FoldMapW (Fun f)) = adds . map (semantics f)
  semantics SingletonListW = (: [])
  semantics AppendW = (++)

instance Syntax ListW where
  prettySymbol AppendW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> short n <+> prettyPrec 10 y
  prettySymbol AppendW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> prettyPrec 10 y <+> short n
  prettySymbol _ _ _ = Nothing

instance Show (ListW d r) where
  show AppendW = "append_"
  show SingletonListW = "singletonList_"
  show (FoldMapW n) = "(FoldMapW  " ++ show n ++ ")"

deriving instance (Eq (ListW d r))

------------------------------------------------------------------------
-- Functions for writing constraints on lists
------------------------------------------------------------------------

-- | Sum over a `Foldy` list
sum_ ::
  Foldy a =>
  Term [a] ->
  Term a
sum_ = foldMap_ id

-- | Like @[a]@
singletonList_ :: HasSpec a => Term a -> Term [a]
singletonList_ = appTerm SingletonListW

-- | Append two lists, like `(++)`
append_ :: HasSpec a => Term [a] -> Term [a] -> Term [a]
append_ = appTerm AppendW

-- | Map a function over a list and fold the results via the `Foldy` instance
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

-- Fun types for lists and their helper functions

foldMapFn :: forall a b. (HasSpec a, Foldy b) => Fun '[a] b -> Fun '[[a]] b
foldMapFn f = Fun (FoldMapW f)

reverseFoldSpec :: FoldSpec a -> Specification a
reverseFoldSpec NoFold = TrueSpec
-- The single element list has to sum to something that obeys spec, i.e. `conformsToSpec (f a) spec`
reverseFoldSpec (FoldSpec (Fun fn) spec) = propagate fn (HOLE :? Nil) spec

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

-- | Specification for how a thing sums together, used to represent `foldMap_`-related constraints
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

-- | Take a `FoldSpec` and turn it into a `FoldSpec` for a function applied
-- before the original spec
preMapFoldSpec :: HasSpec a => Fun '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s

composeFn :: (HasSpec b, HasSpec c) => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
composeFn (Fun f) (Fun g) = (Fun (ComposeW f g))

idFn :: HasSpec a => Fun '[a] a
idFn = Fun IdW

-- | Possibly failing conjuction of `FoldSpec`s
combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (Fun f) s) (FoldSpec (Fun g) s') =
  case sameFunSym f g of
    Just (_, _, Refl) -> pure $ FoldSpec (Fun f) (s <> s')
    Nothing -> Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show g]

-- | Check if a list sums like what's required by a `FoldSpec`
conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec (Fun f) s) = adds (map (semantics f) xs) `conformsToSpec` s

-- | Talk about how to add together values and generate lists of values that
-- conform to `FoldSpec`s
class (HasSpec a, NumLike a) => Foldy a where
  genList ::
    MonadGenError m => Specification a -> Specification a -> GenT m [a]
  default genList ::
    (MonadGenError m, GenericallyInstantiated a, Foldy (SimpleRep a)) =>
    Specification a -> Specification a -> GenT m [a]
  genList s s' = map fromSimpleRep <$> genList (toSimpleRepSpec s) (toSimpleRepSpec s')

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
  default genSizedList ::
    (MonadGenError m, GenericallyInstantiated a, Foldy (SimpleRep a)) =>
    Specification Integer ->
    Specification a ->
    Specification a ->
    GenT m [a]
  genSizedList sz elemSpec foldSpec =
    map fromSimpleRep
      <$> genSizedList sz (toSimpleRepSpec elemSpec) (toSimpleRepSpec foldSpec)

  noNegativeValues :: Bool
  noNegativeValues = False

-- | Semantics of `foldMap_`
adds :: Foldy a => [a] -> a
adds = foldr (semantics theAddFn) theZero

------------------------------------------------------------------------
-- Foldy instances
------------------------------------------------------------------------

instance Foldy Integer where
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int where
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int8 where
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int16 where
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int32 where
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int64 where
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

instance Sized [a] where
  sizeOf = toInteger . length
  liftSizeSpec spec cant = typeSpec (ListSpec Nothing mempty (TypeSpec spec cant) TrueSpec NoFold)
  liftMemberSpec xs = case NE.nonEmpty xs of
    Nothing -> ErrorSpec (pure ("In liftMemberSpec for (Sized List) instance, xs is the empty list"))
    Just zs -> typeSpec (ListSpec Nothing mempty (MemberSpec zs) TrueSpec NoFold)
  sizeOfTypeSpec (ListSpec _ _ _ ErrorSpec {} _) = equalSpec 0
  sizeOfTypeSpec (ListSpec _ must sizespec _ _) = sizespec <> geqSpec (sizeOf must)
