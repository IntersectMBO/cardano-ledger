{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Spec.Tree (BinTree (..), RoseTree (..), RoseTreeFn, roseRoot_) where

import Data.Kind

import Constrained.Base
import Constrained.Core
import Constrained.GenT
import Constrained.List
import Constrained.Spec.Generics
import Constrained.Spec.Pairs
import Constrained.Univ

------------------------------------------------------------------------
-- The types
------------------------------------------------------------------------

data BinTree a
  = BinTip
  | BinNode (BinTree a) a (BinTree a)
  deriving (Ord, Eq, Show)

data RoseTree a = RoseNode a [RoseTree a]
  deriving (Ord, Eq, Show)

------------------------------------------------------------------------
-- HasSpec for BinTree
------------------------------------------------------------------------

data BinTreeSpec fn a = BinTreeSpec Integer (Spec fn (BinTree a, a, BinTree a))
  deriving (Show)

instance Forallable (BinTree a) (BinTree a, a, BinTree a) where
  forAllSpec = typeSpec . BinTreeSpec 1000
  forAllToList BinTip = []
  forAllToList (BinNode left a right) = (left, a, right) : forAllToList left ++ forAllToList right

instance HasSpec fn a => HasSpec fn (BinTree a) where
  type TypeSpec fn (BinTree a) = BinTreeSpec fn a

  emptySpec = BinTreeSpec 1000 TrueSpec

  combineSpec (BinTreeSpec sz s) (BinTreeSpec sz' s') =
    typeSpec $ BinTreeSpec (min sz sz') (s <> s')

  conformsTo BinTip _ = True
  conformsTo (BinNode left a right) s@(BinTreeSpec _ es) =
    and
      [ (left, a, right) `conformsToSpec` es
      , left `conformsTo` s
      , right `conformsTo` s
      ]

  genFromTypeSpec (BinTreeSpec sz s)
    | sz <= 0 = pure BinTip
    | otherwise = do
        let sz' = sz `div` 2
        oneofT
          [ do
              (left, a, right) <- genFromSpec @fn @(BinTree a, a, BinTree a) $
                constrained $ \ctx ->
                  [ match ctx $ \left _ right ->
                      [ forAll left (`satisfies` s)
                      , genHint sz' left
                      , forAll right (`satisfies` s)
                      , genHint sz' right
                      ]
                  , ctx `satisfies` s
                  ]
              pure $ BinNode left a right
          , pure BinTip
          ]

  toPreds t (BinTreeSpec sz s) =
    (forAll t $ \n -> n `satisfies` s)
      <> genHint sz t

instance HasSpec fn a => HasGenHint fn (BinTree a) where
  type Hint (BinTree a) = Integer
  giveHint h = typeSpec $ BinTreeSpec h TrueSpec

------------------------------------------------------------------------
-- HasSpec for RoseTree
------------------------------------------------------------------------

data RoseTreeSpec fn a = RoseTreeSpec
  { roseTreeAvgLength :: Maybe Integer
  , roseTreeMaxSize :: Integer
  , roseTreeRootSpec :: Spec fn a
  , roseTreeCtxSpec :: Spec fn (a, [RoseTree a])
  }

deriving instance (HasSpec fn a, Member (RoseTreeFn fn) fn) => Show (RoseTreeSpec fn a)

instance Forallable (RoseTree a) (a, [RoseTree a]) where
  forAllSpec = guardRoseSpec . RoseTreeSpec Nothing 8000 TrueSpec
  forAllToList (RoseNode a children) = (a, children) : concatMap forAllToList children

isErrorLike :: Spec fn a -> Bool
isErrorLike ErrorSpec {} = True
isErrorLike (MemberSpec []) = True
isErrorLike _ = False

-- TODO: get rid of this when we implement `cardinality`
-- in `HasSpec`
guardRoseSpec :: HasSpec fn (RoseTree a) => RoseTreeSpec fn a -> Spec fn (RoseTree a)
guardRoseSpec spec@(RoseTreeSpec _ _ rs s)
  | isErrorLike rs = ErrorSpec ["guardRoseSpec: rootSpec is error"]
  | isErrorLike s = ErrorSpec ["guardRoseSpec: ctxSpec is error"]
  | otherwise = TypeSpec spec []

instance (HasSpec fn a, Member (RoseTreeFn fn) fn) => HasSpec fn (RoseTree a) where
  type TypeSpec fn (RoseTree a) = RoseTreeSpec fn a

  emptySpec = RoseTreeSpec Nothing 8000 TrueSpec TrueSpec

  combineSpec (RoseTreeSpec mal sz rs s) (RoseTreeSpec mal' sz' rs' s')
    | isErrorLike (typeSpec (Cartesian rs'' TrueSpec) <> s'') = ErrorSpec []
    | otherwise =
        guardRoseSpec $
          RoseTreeSpec
            (unionWithMaybe max mal mal')
            (min sz sz')
            rs''
            s''
    where
      rs'' = rs <> rs'
      s'' = s <> s'

  conformsTo (RoseNode a children) spec@(RoseTreeSpec _ _ rs s) =
    and
      [ (a, children) `conformsToSpec` s
      , all (`conformsTo` spec) children
      , a `conformsToSpec` rs
      ]

  genFromTypeSpec (RoseTreeSpec mal sz rs s) = do
    let sz' = maybe (sz `div` 4) (sz `div`) mal
    fmap (uncurry RoseNode) $
      genFromSpec @fn @(a, [RoseTree a]) $
        constrained $ \ctx ->
          [ forAll (snd_ ctx) $ \t ->
              [ forAll t (`satisfies` s)
              , genHint (mal, sz') t
              ]
          , assert $ length_ (snd_ ctx) <=. lit sz
          , fst_ ctx `satisfies` rs
          , ctx `satisfies` s
          ]

  toPreds t (RoseTreeSpec mal sz rs s) =
    (forAll t $ \n -> n `satisfies` s)
      <> roseRoot_ t `satisfies` rs
      <> genHint (mal, sz) t

instance (Member (RoseTreeFn fn) fn, HasSpec fn a) => HasGenHint fn (RoseTree a) where
  type Hint (RoseTree a) = (Maybe Integer, Integer)
  giveHint (avgLen, sz) = typeSpec $ RoseTreeSpec avgLen sz TrueSpec TrueSpec

data RoseTreeFn (fn :: [Type] -> Type -> Type) args res where
  RoseRoot :: RoseTreeFn fn '[RoseTree a] a

deriving instance Eq (RoseTreeFn fn args res)
deriving instance Show (RoseTreeFn fn args res)

instance FunctionLike (RoseTreeFn fn) where
  sem RoseRoot = \(RoseNode a _) -> a

instance (Member (RoseTreeFn fn) fn, BaseUniverse fn) => Functions (RoseTreeFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun _ _ (MemberSpec []) = MemberSpec []
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec v ps <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (v :-> ps)
    RoseRoot ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: RoseTreeFn fn '[RoseTree a] a)
          | NilCtx HOLE <- ctx -> typeSpec $ RoseTreeSpec Nothing 8000 spec TrueSpec

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    RoseRoot ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: RoseTreeFn fn '[RoseTree a] a)
          | RoseTreeSpec _ _ rs _ <- ts -> rs

roseRoot_ ::
  forall fn a.
  (Member (RoseTreeFn fn) fn, HasSpec fn a) =>
  Term fn (RoseTree a) ->
  Term fn a
roseRoot_ = app (injectFn $ RoseRoot @fn)
