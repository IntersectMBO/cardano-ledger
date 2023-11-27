{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.VMap.TreeDiff () where

import Data.TreeDiff.Class (ToExpr (toExpr))
import Data.VMap
import qualified Data.Vector.Generic as VG

instance (VG.Vector kv k, VG.Vector vv v, ToExpr k, ToExpr v) => ToExpr (VMap kv vv k v) where
  toExpr = toExpr . toMap
