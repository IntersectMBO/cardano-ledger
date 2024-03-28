
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}



{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Count
  ( samePtr,Count(..),
    t1,t2,t3,t4,
  )
  where

import Data.Map.Internal(Map(..))
import qualified Data.Map.Internal as Map(link)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Control.Monad.State(State) -- ,runState,get,put,state)
import Data.Kind
import Debug.Trace
import Control.DeepSeq (NFData, ($!!))
import GHC.Exts.Heap(Box, asBox, areBoxesEqual)

-- ===========================================

samePtr :: a -> a -> IO Bool
samePtr !x !y = areBoxesEqual (asBox x) (asBox y)

class Count t where
  type Store t :: Type
  countup :: t -> Store t -> IO (Store t)

count :: (Store t ~ Map k a, Count t) => t -> IO (Store t)
count x = countup x Map.empty

addC :: Ord x => x -> Map x Int -> IO (Map x Int)
addC !x !Tip = pure (Map.singleton x 1)
addC !x !t@(Bin _ !k !v !l !r) =
       case compare x k of
            LT -> do z <- addC x l
                     pure (Map.link k v z r)
            EQ -> do b <- samePtr x k
                     if b then pure t else pure $ Map.link k (v+1) l r
            GT -> do z <- addC x r
                     pure (Map.link k v l z)

addCs :: Ord x => [x] -> Map x Int -> IO (Map x Int)
addCs [] m = pure m
addCs (x :xs) m = do
  m1 <- addC x m
  addCs xs m1

instance Count T where
  type Store T = Map T Int
  {-# NOINLINE countup #-}
  countup !t !s = addC t s

instance Count x => Count [x] where
  type Store [x] = Store x
  {-# NOINLINE countup #-}
  countup [] m = pure m
  countup (y:ys) m = do
     m1 <- countup y m
     countup ys m1

data T = T Char Bool deriving (Eq,Ord,Generic,NFData)
instance Show T where show (T x _) = "T"++[x]

t1, t2, t3, t4 :: [T]
t1 = [ T c True | c <- "aaaa" ]
a = T 'a' True
b = T 'b' True
t2 = replicate 4 a
t3 = replicate 2 a ++ replicate 2 b
t4 = [ T c True| c <- "abcd" ]

b1 :: Map T Int
b1 = Map.singleton a 1


-- =================================
