{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | `HasSpec` instance for `Tree`
module Constrained.Spec.Tree (
  TreeSpec (..),
  rootLabel_,
  TreeW (..),
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.Generation
import Constrained.List
import Constrained.Spec.List
import Constrained.Spec.SumProd ()
import Constrained.Syntax
import Constrained.TheKnot
import Data.Kind
import Data.Tree
import Test.QuickCheck (shrinkList)

------------------------------------------------------------------------
-- HasSpec for Tree
------------------------------------------------------------------------

-- | t`TypeSpec` for `Tree`
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

  cardinalTypeSpec _ = mempty

  toPreds t (TreeSpec mal msz rs s) =
    (forAll t $ \n -> n `satisfies` s)
      <> rootLabel_ t
        `satisfies` rs
      <> maybe TruePred (\sz -> genHint (mal, sz) t) msz

instance HasSpec a => HasGenHint (Tree a) where
  type Hint (Tree a) = (Maybe Integer, Integer)
  giveHint (avgLen, sz) = typeSpec $ TreeSpec avgLen (Just sz) TrueSpec TrueSpec

-- | Function symbols for talking about trees
data TreeW (dom :: [Type]) (rng :: Type) where
  RootLabelW :: HasSpec a => TreeW '[Tree a] a

deriving instance Eq (TreeW d r)

deriving instance Show (TreeW d r)

instance Semantics TreeW where
  semantics RootLabelW = \(Node a _) -> a

instance Syntax TreeW

instance Logic TreeW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate RootLabelW (Unary HOLE) (SuspendedSpec v ps) = constrained $ \v' -> Let (App RootLabelW (v' :> Nil)) (v :-> ps)
  propagate RootLabelW (Unary HOLE) spec = typeSpec $ TreeSpec Nothing Nothing spec TrueSpec

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec RootLabelW (TreeSpec _ _ rs _) = rs

-- | Get the label of the root of the `Tree`
rootLabel_ ::
  forall a.
  HasSpec a =>
  Term (Tree a) ->
  Term a
rootLabel_ = appTerm RootLabelW
