{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Constrained.Examples.BinTree where

import Constrained.API
import GHC.Generics

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

  emptySpec = BinTreeSpec Nothing mempty

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

  cardinalTypeSpec _ = mempty

  toPreds t (BinTreeSpec msz s) =
    (forAll t $ \n -> n `satisfies` s)
      <> maybe mempty (flip genHint t) msz

instance HasSpec a => HasGenHint (BinTree a) where
  type Hint (BinTree a) = Integer
  giveHint h = typeSpec $ BinTreeSpec (Just h) mempty
