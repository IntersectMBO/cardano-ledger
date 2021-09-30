{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Introduces a number of new Array type constructors which are instances of Indexable. These
--   are usefull because they can be used to make small (compact) values with low memory use.
module Data.Compact.Arrays where

import qualified Data.Array as A
import qualified Data.Primitive.Array as PA
import qualified Data.Array.MArray as MutA
import GHC.Arr(STArray(..),unsafeFreezeSTArray,newSTArray)
import Data.Primitive.PrimArray
 ( PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray, copyPrimArray,
   MutablePrimArray,unsafeFreezePrimArray , newPrimArray,sizeofMutablePrimArray, readPrimArray, writePrimArray,
 )
import Data.ByteString.Short (ShortByteString,toShort,fromShort)
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
import Data.Primitive.Types (Prim (..))
import Cardano.Prelude (HeapWords (..))
import GHC.Exts( IsList (toList) )
import Control.Monad.ST (ST, runST)
import Data.List(sort,sortBy)
import Debug.Trace(trace)
import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
    serialize',
    unsafeDeserialize',
  )
import Data.Text(Text,pack)
import Data.STRef(STRef,newSTRef,readSTRef,writeSTRef)
import Data.Compact.Class


shorten :: ToCBOR t => t -> ShortByteString
shorten x = toShort (serialize' x)

-- ============================================================
-- FlexArray. A list of arrays with exponentially increasing sizes
-- the size of each array is a power of 2. This allows FlexArrays to grow
-- gracefully by pushing an element on the end. By pushing on the end
-- the index of each element stays the same as the array grows.

-- | A node carries a 'size' and some array-like type 'arr'
data Node arr = Node {-# UNPACK #-} !Int arr
  deriving Show

arrayPart (Node _ arr) = arr
nodesize (Node i _) = i

data FlexArray arr t where
  FlexArray:: Indexable arr t =>
              {-# UNPACK #-} !Int ->
              [Node(arr t)] ->
              FlexArray arr t

instance (Indexable arr t,Show t) => Show (FlexArray arr t) where
  show (FlexArray _ ns) = concat (map showNode (reverse ns))
    where showNode (Node _ arr) = show (reverse (tolist arr))

instance Indexable arr t => Indexable (FlexArray arr) t where
  index (FlexArray n nodes) i = indexL nodes ((n - i) + 1)
  isize (FlexArray _ nodes) = isizeL nodes
  fromlist xs = FlexArray (length xs) (fromlistL xs)
  tolist (FlexArray n nodes) = tolistL nodes

indexL :: Indexable arr t => [Node(arr t)] -> Int -> t
indexL [] i = error ("Index, "++show i++", out of bounds on empty [Node (arr t)]")
indexL ((Node n arr):more) i = if i < n then index arr i else indexL more (i - n)

isizeL :: Indexable arr t => [Node(arr t)] -> Int
isizeL xs = sum(map (\ (Node n _) -> n) xs)

fromlistL :: Indexable t a => [a] -> [Node (t a)]
fromlistL xs = map node pairs
   where pairs = pieces (reverse xs)
         node (n,ys) = Node n (fromlist ys)

tolistL :: Indexable t a => [Node (t a)] -> [a]
tolistL xs = concat(map (tolist . arrayPart) xs)

pushD :: (ArrayPair arr marr t,Indexable arr t) => FlexArray arr t -> t -> FlexArray arr t
pushD (FlexArray _ []) t = fromlist [t]
pushD (FlexArray siz nodes) t =
   case splitAtFullPrefix nodesize 1 (Node 1 (fromlist [t])) nodes of
     (size,prefix,tail) -> FlexArray siz (Node size (catArray (map arrayPart prefix)):tail)

flex10, flex11, flex12 :: FlexArray PrimArray Int
flex10 = fromlist [1..19] 
flex11 = pushD flex10 20
flex12 = pushD flex11 21
flex13 = pushD flex12 22
flex14 = pushD flex13 23
flex15 = pushD flex14 24

instance (Indexable arr key,Ord key) => Search (FlexArray arr key) key
   where search key v = binsearch 0 (isize v - 1) key v   
