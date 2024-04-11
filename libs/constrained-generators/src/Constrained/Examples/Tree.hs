{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Constrained.Examples.Tree where

import Data.Tree

import Constrained

allZeroTree :: Specification BaseFn (BinTree Int)
allZeroTree = constrained $ \t ->
  [ forAll' t $ \_ a _ -> a ==. 0
  , genHint 10 t
  ]

isBST :: Specification BaseFn (BinTree Int)
isBST = constrained $ \t ->
  [ forAll' t $ \left a right ->
      -- TODO: if there was a `binTreeRoot` function on trees
      -- this wouldn't need to be quadratic as we would
      -- only check agains the head of the left and right
      -- subtrees, not _every element_
      [ forAll' left $ \_ l _ -> l <. a
      , forAll' right $ \_ h _ -> a <. h
      ]
  , genHint 10 t
  ]

noChildrenSameTree :: Specification BaseFn (BinTree Int)
noChildrenSameTree = constrained $ \t ->
  [ forAll' t $ \left a right ->
      [ forAll' left $ \_ l _ -> l /=. a
      , forAll' right $ \_ r _ -> r /=. a
      ]
  , genHint 8 t
  ]

type RoseFn = Fix (OneofL (TreeFn : BaseFns))

isAllZeroTree :: Specification RoseFn (Tree Int)
isAllZeroTree = constrained $ \t ->
  [ forAll' t $ \a cs ->
      [ a ==. 0
      , length_ cs <=. 4
      ]
  , genHint (Just 2, 30) t
  ]

noSameChildrenTree :: Specification RoseFn (Tree Int)
noSameChildrenTree = constrained $ \t ->
  [ forAll' t $ \a cs ->
      [ assert $ a `elem_` lit [1 .. 8]
      , forAll cs $ \t' ->
          forAll' t' $ \b _ ->
            b /=. a
      ]
  , genHint (Just 2, 30) t
  ]

successiveChildren :: Specification RoseFn (Tree Int)
successiveChildren = constrained $ \t ->
  [ forAll' t $ \a cs ->
      [ forAll cs $ \t' ->
          rootLabel_ t' ==. a + 1
      ]
  , genHint (Just 2, 10) t
  ]

successiveChildren8 :: Specification RoseFn (Tree Int)
successiveChildren8 = constrained $ \t ->
  [ t `satisfies` successiveChildren
  , forAll' t $ \a _ -> a `elem_` lit [1 .. 5]
  ]

roseTreeList :: Specification RoseFn [Tree Int]
roseTreeList = constrained $ \ts ->
  [ assert $ length_ ts <=. 10
  , forAll ts $ \t ->
      [ forAll t $ \_ -> False
      ]
  ]

roseTreePairs :: Specification RoseFn (Tree ([Int], [Int]))
roseTreePairs = constrained $ \t ->
  [ assert $ rootLabel_ t ==. lit ([1 .. 10], [1 .. 10])
  , forAll' t $ \p ts ->
      forAll ts $ \t' ->
        fst_ (rootLabel_ t') ==. snd_ p
  , genHint (Nothing, 10) t
  ]

roseTreeMaybe :: Specification RoseFn (Tree (Maybe (Int, Int)))
roseTreeMaybe = constrained $ \t ->
  [ forAll' t $ \mp ts ->
      forAll ts $ \t' ->
        onJust mp $ \p ->
          onJust (rootLabel_ t') $ \p' ->
            fst_ p' ==. snd_ p
  , forAll' t $ \mp _ -> isJust mp
  , genHint (Nothing, 10) t
  ]

badTreeInteraction :: Specification RoseFn (Tree (Either Int Int))
badTreeInteraction = constrained $ \t ->
  [ forAll' t $ \n ts' ->
      [ isCon @"Right" n
      , forAll ts' $ \_ -> True
      ]
  , forAll' t $ \n ts' ->
      forAll ts' $ \t' ->
        [ genHint (Just 4, 10) t'
        , assert $ rootLabel_ t' ==. n
        ]
  , genHint (Just 4, 10) t
  ]
