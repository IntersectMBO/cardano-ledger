{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Constrained.Spec.Tree (BinTree (..), TreeFn, rootLabel_, TreeSpec (..)) where

import Data.Kind
import Data.Tree
import GHC.Generics
import Test.QuickCheck (shrinkList)

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
  deriving (Ord, Eq, Show, Generic)

------------------------------------------------------------------------
-- HasSpec for BinTree
------------------------------------------------------------------------

data BinTreeSpec fn a = BinTreeSpec Integer (Specification fn (BinTree a, a, BinTree a))
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

  shrinkWithTypeSpec _ BinTip = []
  shrinkWithTypeSpec s (BinNode left a right) =
    BinTip
      : left
      : right
      : (BinNode left a <$> shrinkWithTypeSpec s right)
      ++ ((\l -> BinNode l a right) <$> shrinkWithTypeSpec s left)

  toPreds t (BinTreeSpec sz s) =
    (forAll t $ \n -> n `satisfies` s)
      <> genHint sz t

instance HasSpec fn a => HasGenHint fn (BinTree a) where
  type Hint (BinTree a) = Integer
  giveHint h = typeSpec $ BinTreeSpec h TrueSpec

------------------------------------------------------------------------
-- HasSpec for Tree
------------------------------------------------------------------------

data TreeSpec fn a = TreeSpec
  { roseTreeAvgLength :: Maybe Integer
  , roseTreeMaxSize :: Integer
  , roseTreeRootSpec :: Specification fn a
  , roseTreeCtxSpec :: Specification fn (a, [Tree a])
  }

deriving instance (HasSpec fn a, Member (TreeFn fn) fn) => Show (TreeSpec fn a)

instance Forallable (Tree a) (a, [Tree a]) where
  forAllSpec = guardRoseSpec . TreeSpec Nothing 8000 TrueSpec
  forAllToList (Node a children) = (a, children) : concatMap forAllToList children

-- TODO: get rid of this when we implement `cardinality`
-- in `HasSpec`
guardRoseSpec :: HasSpec fn (Tree a) => TreeSpec fn a -> Specification fn (Tree a)
guardRoseSpec spec@(TreeSpec _ _ rs s)
  | isErrorLike rs = ErrorSpec ["guardRoseSpec: rootSpec is error"]
  | isErrorLike s = ErrorSpec ["guardRoseSpec: ctxSpec is error"]
  | otherwise = TypeSpec spec []

instance (HasSpec fn a, Member (TreeFn fn) fn) => HasSpec fn (Tree a) where
  type TypeSpec fn (Tree a) = TreeSpec fn a

  emptySpec = TreeSpec Nothing 8000 TrueSpec TrueSpec

  combineSpec (TreeSpec mal sz rs s) (TreeSpec mal' sz' rs' s')
    | isErrorLike (typeSpec (Cartesian rs'' TrueSpec) <> s'') = ErrorSpec []
    | otherwise =
        guardRoseSpec $
          TreeSpec
            (unionWithMaybe max mal mal')
            (min sz sz')
            rs''
            s''
    where
      rs'' = rs <> rs'
      s'' = s <> s'

  conformsTo (Node a children) (TreeSpec _ _ rs s) =
    and
      [ (a, children) `conformsToSpec` s
      , all (\(Node a' children') -> (a', children') `conformsToSpec` s) children
      , a `conformsToSpec` rs
      ]

  genFromTypeSpec (TreeSpec mal sz rs s) = do
    let sz' = maybe (sz `div` 4) (sz `div`) mal
        childrenSpec =
          typeSpec $
            ListSpec
              (Just sz')
              []
              TrueSpec
              (typeSpec $ TreeSpec mal sz' TrueSpec s)
              NoFold
        innerSpec = s <> typeSpec (Cartesian rs childrenSpec)
    fmap (uncurry Node) $
      genFromSpec @fn @(a, [Tree a]) innerSpec

  shrinkWithTypeSpec (TreeSpec _ _ rs ctxSpec) (Node a ts) =
    [Node a [] | not $ null ts]
      ++ ts
      ++ [Node a' ts | a' <- shrinkWithSpec rs a]
      ++ [Node a [t] | t <- ts]
      ++ [ Node a ts'
         | ts' <- shrinkList (shrinkWithTypeSpec (TreeSpec Nothing 8000 TrueSpec ctxSpec)) ts
         ]

  toPreds t (TreeSpec mal sz rs s) =
    (forAll t $ \n -> n `satisfies` s)
      <> rootLabel_ t `satisfies` rs
      <> genHint (mal, sz) t

instance (Member (TreeFn fn) fn, HasSpec fn a) => HasGenHint fn (Tree a) where
  type Hint (Tree a) = (Maybe Integer, Integer)
  giveHint (avgLen, sz) = typeSpec $ TreeSpec avgLen sz TrueSpec TrueSpec

data TreeFn (fn :: [Type] -> Type -> Type) args res where
  RootLabel :: TreeFn fn '[Tree a] a

deriving instance Eq (TreeFn fn args res)
deriving instance Show (TreeFn fn args res)

instance FunctionLike (TreeFn fn) where
  sem RootLabel = \(Node a _) -> a

instance (Member (TreeFn fn) fn, BaseUniverse fn) => Functions (TreeFn fn) fn where
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
    RootLabel ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: TreeFn fn '[Tree a] a)
          | NilCtx HOLE <- ctx -> typeSpec $ TreeSpec Nothing 8000 spec TrueSpec

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    RootLabel ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: TreeFn fn '[Tree a] a)
          | TreeSpec _ _ rs _ <- ts -> rs

rootLabel_ ::
  forall fn a.
  (Member (TreeFn fn) fn, HasSpec fn a) =>
  Term fn (Tree a) ->
  Term fn a
rootLabel_ = app (injectFn $ RootLabel @fn)
