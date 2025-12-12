{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.TreeDiff where

import Data.Foldable (Foldable (..))
import Data.Foldable qualified as F
import Data.OMap.Strict
import Data.OSet.Strict
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty qualified as NES
import Test.Cardano.Ledger.Binary.TreeDiff (Expr (..), ToExpr (..))

instance ToExpr a => ToExpr (OSet a) where
  toExpr x = App "OSet.fromList" [toExpr $ toList x]

instance (HasOKey k v, ToExpr v) => ToExpr (OMap k v) where
  listToExpr = listToExpr . F.toList
  toExpr = toExpr . F.toList

instance ToExpr a => ToExpr (NonEmptySet a) where
  toExpr = toExpr . NES.toList
