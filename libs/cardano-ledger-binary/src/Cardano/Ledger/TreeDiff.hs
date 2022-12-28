{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Collect all the basic TreeDiff stuff in one place.
--   Including orphan instances from external types.
--   So anyplace this module is imported to get access to the
--   ToExpr class, one also gets the orphan instances.
module Cardano.Ledger.TreeDiff (
  Expr (App, Rec, Lst),
  ToExpr (listToExpr, toExpr),
  defaultExprViaShow,
  trimExprViaShow,
  diffExpr,
  ediffEq,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Hash.Class ()
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..), WithOrigin (..))
import Data.Foldable (toList)
import Data.IP (IPv4, IPv6)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.TreeDiff (ansiWlEditExpr)
import Data.TreeDiff.Class (ToExpr (listToExpr, toExpr), defaultExprViaShow, ediff)
import Data.TreeDiff.Expr (Expr (App, Lst, Rec))
import Data.TreeDiff.QuickCheck (ediffEq)

-- =====================================================
-- Cardano functions that deal with TreeDiff and ToExpr

trimExprViaShow :: Show a => Int -> a -> Expr
trimExprViaShow _n x = defaultExprViaShow x -- App (take n (drop 1 (show x)) ++ "..") []

diffExpr :: ToExpr a => a -> a -> String
diffExpr x y = show (ansiWlEditExpr (ediff x y))

-- ===========================================================
-- Orphan instances from external imports

instance ToExpr IPv4

instance ToExpr IPv6

instance ToExpr SlotNo

instance ToExpr BlockNo

instance ToExpr EpochNo

instance ToExpr EpochSize

instance ToExpr x => ToExpr (WithOrigin x)

instance ToExpr (Hash.Hash c index) where
  toExpr x = trimExprViaShow 10 x

instance ToExpr a => ToExpr (StrictSeq a) where
  toExpr x = App "StrictSeqFromList" [listToExpr (toList x)]

instance ToExpr a => ToExpr (StrictMaybe a)
