{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.TreeDiff where

import Data.Foldable qualified as F
import Data.OMap.Strict
import Data.OSet.Strict
import Test.Cardano.Ledger.Binary.TreeDiff (ToExpr (..))

deriving instance ToExpr a => ToExpr (OSet a)

instance (HasOKey k v, ToExpr v) => ToExpr (OMap k v) where
  listToExpr = listToExpr . F.toList
  toExpr = toExpr . F.toList
