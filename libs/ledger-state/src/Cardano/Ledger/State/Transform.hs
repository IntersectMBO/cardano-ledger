{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Transform where

import Data.Map.Strict.Internal

intern :: Ord k => k -> Map k a -> k
intern !k m =
  case internMaybe k m of
    Just kx -> kx
    Nothing -> k

internMaybe :: Ord k => k -> Map k a -> Maybe k
internMaybe !k = go
  where
    go Tip = Nothing
    go (Bin _ kx _ l r) =
      case compare k kx of
        LT -> go l
        GT -> go r
        EQ -> Just kx

internVal :: (Eq a, Ord k) => k -> a -> Map k a -> a
internVal !k !a m =
  case internValMaybe k a m of
    Just ax -> ax
    Nothing -> a

internsVal :: (Eq a, Ord k) => k -> a -> [Map k a] -> a
internsVal !k !a = go
  where
    go [] = a
    go (m : ms) =
      case internValMaybe k a m of
        Just ax -> ax
        Nothing -> go ms

internValMaybe :: (Eq a, Ord k) => k -> a -> Map k a -> Maybe a
internValMaybe !k !a = go
  where
    go Tip = Nothing
    go (Bin _ kx ax l r) =
      case compare k kx of
        LT -> go l
        GT -> go r
        EQ
          | a == ax -> Just ax
          | otherwise -> Nothing
