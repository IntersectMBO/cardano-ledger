{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.BeTreeTests where

import Cardano.Ledger.Pretty (PDoc, ppMap', ppRecord, ppSexp, ppString)
import Data.BeTree
import qualified Data.Map as Map
import Data.Messages
import Prettyprinter (viaShow)
import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary (..), generate, testProperty, withMaxSuccess)

ppMessage :: Show v => Message v -> PDoc
ppMessage (Edit x) = ppSexp "Edit" [viaShow x]
ppMessage Delete = ppString "Delete"
ppMessage (Upsert x) = ppSexp "Upsert" [viaShow x]

ppBe :: (Show k, Show v) => BeTree k v -> PDoc
ppBe (Leaf x) = ppSexp "Leaf" [ppMap' mempty viaShow viaShow x]
ppBe (Internal sub (Delta buffer)) =
  ppRecord
    "Internal"
    [ ("subtrees", ppMap' mempty viaShow ppBe sub),
      ("buffer", ppMap' mempty viaShow ppMessage buffer)
    ]

instance (Show k, Show v) => Show (BeTree k v) where
  show x = show (ppBe x)

-- ================================================================
-- Some tests

-- | Build a consectutuve BeTree
go :: Int -> BeTree Int Int
go x = fromList [(i, i) | i <- [(1 :: Int) .. x]] (Leaf Map.empty)

converts :: Map.Map Int Int -> Bool
converts x = beTreeToMap (mapToBeTree x) == x

lookups :: Int -> Map.Map Int Int -> Bool
lookups k x = lookupB k (mapToBeTree x) == Map.lookup k x

main :: IO ()
main =
  defaultMain $
    testGroup
      "BeTree tests"
      [ testProperty "MapToBeTree" (withMaxSuccess 1000 converts),
        testProperty "BeTree lookup" (withMaxSuccess 1000 lookups)
      ]

someTree :: IO (BeTree Int Int)
someTree = generate (mapToBeTree <$> arbitrary)
