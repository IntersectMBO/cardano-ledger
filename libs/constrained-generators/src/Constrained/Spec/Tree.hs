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

module Constrained.Spec.Tree (BinTree (..), TreeW (..), rootLabel_, TreeSpec (..)) where

import Constrained.Core (unionWithMaybe)
import Constrained.Base
import Constrained.Conformance (conformsToSpec, satisfies)
import Constrained.Spec.ListFoldy (FoldSpec (..), ListSpec (..))
import Constrained.Spec.SumProd (PairSpec (Cartesian), match)
import Constrained.Syntax (forAll, genHint)
import Constrained.TheKnot (genFromSpecT, shrinkWithSpec)
import Constrained.GenT (oneofT)
import Constrained.List (List (..))
import Data.Kind
import Data.Tree
import GHC.Generics
import GHC.TypeLits hiding (Text)
import Test.QuickCheck (shrinkList)

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

data BinTreeSpec a = BinTreeSpec (Maybe Integer) (Specification (BinTree a, a, BinTree a))
  deriving (Show)

instance Forallable (BinTree a) (BinTree a, a, BinTree a) where
  fromForAllSpec = typeSpec . BinTreeSpec Nothing
  forAllToList BinTip = []
  forAllToList (BinNode left a right) = (left, a, right) : forAllToList left ++ forAllToList right

instance HasSpec a => HasSpec (BinTree a) where
  type TypeSpec (BinTree a) = BinTreeSpec a

  emptySpec = BinTreeSpec Nothing TrueSpec

  combineSpec (BinTreeSpec sz s) (BinTreeSpec sz' s') =
    typeSpec $ BinTreeSpec (unionWithMaybe min sz sz') (s <> s')

  conformsTo BinTip _ = True
  conformsTo (BinNode left a right) s@(BinTreeSpec _ es) =
    and
      [ (left, a, right) `conformsToSpec` es
      , left `conformsTo` s
      , right `conformsTo` s
      ]

  genFromTypeSpec (BinTreeSpec msz s)
    | Just sz <- msz, sz <= 0 = pure BinTip
    | otherwise = do
        let sz = maybe 20 id msz
            sz' = sz `div` 2
        oneofT
          [ do
              (left, a, right) <- genFromSpecT @(BinTree a, a, BinTree a) $
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

  cardinalTypeSpec _ = TrueSpec

  toPreds t (BinTreeSpec msz s) =
    (forAll t $ \n -> n `satisfies` s)
      <> maybe TruePred (flip genHint t) msz

instance HasSpec a => HasGenHint (BinTree a) where
  type Hint (BinTree a) = Integer
  giveHint h = typeSpec $ BinTreeSpec (Just h) TrueSpec

------------------------------------------------------------------------
-- HasSpec for Tree
------------------------------------------------------------------------

data TreeSpec a = TreeSpec
  { roseTreeAvgLength :: Maybe Integer
  , roseTreeMaxSize :: Maybe Integer
  , roseTreeRootSpec :: Specification a
  , roseTreeCtxSpec :: Specification (a, [Tree a])
  }

deriving instance HasSpec a => Show (TreeSpec a)

instance Forallable (Tree a) (a, [Tree a]) where
  fromForAllSpec = guardRoseSpec . TreeSpec Nothing Nothing TrueSpec
  forAllToList (Node a children) = (a, children) : concatMap forAllToList children

-- TODO: get rid of this when we implement `cardinality`
-- in `HasSpec`
guardRoseSpec :: HasSpec (Tree a) => TreeSpec a -> Specification (Tree a)
guardRoseSpec spec@(TreeSpec _ _ rs s)
  | isErrorLike rs = ErrorSpec (pure "guardRoseSpec: rootSpec is error")
  | isErrorLike s = ErrorSpec (pure "guardRoseSpec: ctxSpec is error")
  | otherwise = TypeSpec spec []

instance HasSpec a => HasSpec (Tree a) where
  type TypeSpec (Tree a) = TreeSpec a

  emptySpec = TreeSpec Nothing Nothing TrueSpec TrueSpec

  combineSpec (TreeSpec mal sz rs s) (TreeSpec mal' sz' rs' s')
    | isErrorLike alteredspec = ErrorSpec (errorLikeMessage alteredspec)
    | otherwise =
        guardRoseSpec $
          TreeSpec
            (unionWithMaybe max mal mal')
            (unionWithMaybe min sz sz')
            rs''
            s''
    where
      alteredspec = (typeSpec (Cartesian rs'' TrueSpec) <> s'')
      rs'' = rs <> rs'
      s'' = s <> s'

  conformsTo (Node a children) (TreeSpec _ _ rs s) =
    and
      [ (a, children) `conformsToSpec` s
      , all (\(Node a' children') -> (a', children') `conformsToSpec` s) children
      , a `conformsToSpec` rs
      ]

  genFromTypeSpec (TreeSpec mal msz rs s) = do
    let sz = maybe 20 id msz
        sz' = maybe (sz `div` 4) (sz `div`) mal
        childrenSpec =
          typeSpec $
            ListSpec
              (Just sz')
              []
              TrueSpec
              (typeSpec $ TreeSpec mal (Just sz') TrueSpec s)
              NoFold
        innerSpec = s <> typeSpec (Cartesian rs childrenSpec)
    fmap (uncurry Node) $
      genFromSpecT @(a, [Tree a]) innerSpec

  shrinkWithTypeSpec (TreeSpec _ _ rs ctxSpec) (Node a ts) =
    [Node a [] | not $ null ts]
      ++ ts
      ++ [Node a' ts | a' <- shrinkWithSpec rs a]
      ++ [Node a [t] | t <- ts]
      ++ [ Node a ts'
         | ts' <- shrinkList (shrinkWithTypeSpec (TreeSpec Nothing Nothing TrueSpec ctxSpec)) ts
         ]

  cardinalTypeSpec _ = TrueSpec

  toPreds t (TreeSpec mal msz rs s) =
    (forAll t $ \n -> n `satisfies` s)
      <> rootLabel_ t `satisfies` rs
      <> maybe TruePred (\sz -> genHint (mal, sz) t) msz

instance HasSpec a => HasGenHint (Tree a) where
  type Hint (Tree a) = (Maybe Integer, Integer)
  giveHint (avgLen, sz) = typeSpec $ TreeSpec avgLen (Just sz) TrueSpec TrueSpec

data TreeW (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  RootLabelW :: TreeW "rootLabel_" '[Tree a] a

deriving instance Eq (TreeW s d r)
deriving instance Show (TreeW s d r)

instance Semantics TreeW where
  semantics RootLabelW = \(Node a _) -> a

instance Syntax TreeW where
  inFix _ = False

instance HasSpec a => Logic "rootLabel_" TreeW '[Tree a] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context RootLabelW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App RootLabelW (v' :> Nil)) (v :-> ps)
  propagate (Context RootLabelW (HOLE :<> End)) spec =
    typeSpec $ TreeSpec Nothing Nothing spec TrueSpec
  propagate ctx _ =
    ErrorSpec $ pure ("FunSym instance for RootLabel with wrong number of arguments. " ++ show ctx)

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec RootLabelW (TreeSpec _ _ rs _) = rs

rootLabel_ ::
  forall a.
  HasSpec a =>
  Term (Tree a) ->
  Term a
rootLabel_ = appTerm RootLabelW
