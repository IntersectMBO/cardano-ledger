{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Indexable
  ( Indexable (..)
  ) where

import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import Data.Word (Word64)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Map.Strict as Map

-- =======================================

class Indexable elem container where
  indexOf :: elem -> container -> StrictMaybe Word64
  fromIndex :: Word64 -> container -> StrictMaybe elem

instance Ord k => Indexable k (Set k) where
  indexOf n set = case Set.lookupIndex n set of
    Just x -> SJust (fromIntegral x)
    Nothing -> SNothing
  fromIndex i set =
    if fromIntegral i < Set.size set
      then SJust $ Set.elemAt (fromIntegral i) set
      else SNothing

instance Eq k => Indexable k (StrictSeq k) where
  indexOf n seqx = case StrictSeq.findIndexL (== n) seqx of
    Just m -> SJust (fromIntegral m)
    Nothing -> SNothing
  fromIndex i seqx = maybeToStrictMaybe $ StrictSeq.lookup (fromIntegral i) seqx

instance Ord k => Indexable k (Map.Map k v) where
  indexOf n mp = case Map.lookupIndex n mp of
    Just x -> SJust (fromIntegral x)
    Nothing -> SNothing
  fromIndex i mp =
    if fromIntegral i < Map.size mp
      then SJust . fst $ Map.elemAt (fromIntegral i) mp
      else SNothing
