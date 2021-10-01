{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Compact.Set where

import Data.Compact.Class
import Data.Primitive.PrimArray( PrimArray )
import Data.Primitive.Types (Prim (..)) 
import Data.List(sort,nub)

-- ================================================================================
-- Sets of objects as lists of Nodes. The array in the Node stores its elements
-- in ascneding order. The Nodes are stored in order of ascending size.
-- Each size is a power of 2. In each (Node size primarray) the 'primarray' has
-- 'size' components. The sum of the sizes is the total number of elements in the set.
-- This trick of using a list of ascending size arrays supports efficient insert.

newtype FlexSet arr t = FlexSet [Node(arr t)]

instance (Indexable arr t,Show t) => Show(FlexSet arr t) where
  show (FlexSet ts) = show(map unNode ts)
    where unNode(Node _ xs) = tolist xs

hasElemL :: Search (arr t) t => t -> [Node(arr t)] -> Bool
hasElemL t xs =
  case search t xs of
    Nothing -> False
    Just _ -> True

insertElemL :: (ArrayPair arr marr t,Search (arr t) t) => t -> [Node(arr t)] -> [Node(arr t)]
insertElemL t nodes =
  if hasElemL t nodes
     then nodes
     else case splitAtFullPrefix nodesize 1 (Node 1 (fromlist [t])) nodes of
           (size,prefix,rest) -> ((mergeNodes size prefix):rest)

makeSetL :: (Ord t,Indexable arr t) => [t] -> [Node (arr t)]
makeSetL ts = (map node nodes) where
    nodes = pieces (nub ts)
    node (n, ps) = Node n (fromlist (sort ps))


instance (Ord t,ArrayPair arr marr t,Search (arr t) t) => Setlike (FlexSet arr) t where
  makeset xs = FlexSet (makeSetL xs)
  elemset k (FlexSet xs) = hasElemL k xs
  insertset k (FlexSet xs) = FlexSet(insertElemL k xs)
  emptyset = FlexSet []
    
-- ========================================
-- Here we fix the type of the array

newtype CompactPrimSet t = CompactPrimSet [Node (PrimArray t)]

instance (Prim t,Show t) => Show(CompactPrimSet t) where
  show (CompactPrimSet ts) = show(map unNode ts)
    where unNode(Node _ xs) = tolist xs
    
instance (Ord t,Prim t) => Setlike CompactPrimSet t where
  makeset xs = CompactPrimSet (makeSetL xs)
  elemset k (CompactPrimSet xs) = hasElemL k xs
  insertset k (CompactPrimSet xs) = CompactPrimSet (insertElemL k xs)
  emptyset = CompactPrimSet []

mergeNodes :: (Ord t, ArrayPair arr marr t) => Int -> [Node (arr t)] -> Node (arr t)
mergeNodes n xs = Node n (mergeWithAction n (map arrayPart xs) action1)

-- =================================================================
-- here we fix the type of the element

newtype IntSet = IntSet [Node (PrimArray Int)]

instance Show IntSet where
  show (IntSet ts) = show(map unNode ts)
    where unNode(Node _ xs) = tolist xs

makeIntSet :: [Int] -> IntSet
makeIntSet xs = IntSet (makeSetL xs)

elemIntSet :: Int -> IntSet -> Bool
elemIntSet k (IntSet xs) = hasElemL k xs

insertIntSet :: Int -> IntSet -> IntSet
insertIntSet k (IntSet xs) = IntSet (insertElemL k xs)

emptyIntSet :: IntSet
emptyIntSet = IntSet []


ss1, ss2, ss3, ss4, ss5, ss6, ss7, ss8, ss9 :: IntSet
ss1 = emptyIntSet
ss2 = insertIntSet 99 ss1
ss3 = insertIntSet 33 ss2
ss4 = insertIntSet 12 ss3
ss5 = insertIntSet 6 ss4
ss6 = insertIntSet 22 ss5
ss7 = insertIntSet 71 ss6
ss8 = insertIntSet 81 ss7
ss9 = insertIntSet 51 ss8

