{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.ListSplit where

import Data.List (elemIndices, isPrefixOf, isSuffixOf)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, fromMaybe)
import Prettyprinter
import Test.QuickCheck

data ListSplit a = Split (Maybe [a]) (Maybe [a]) deriving (Eq, Show)

instance Show a => Pretty (ListSplit a) where
  pretty (Split Nothing Nothing) = "noSplit"
  pretty (Split Nothing (Just xs)) = parens $ sep ["Split", "...", "++", viaShow xs]
  pretty (Split (Just ys) (Just xs)) = parens $ sep ["Split", viaShow ys, "++", viaShow xs]
  pretty (Split (Just ys) Nothing) = parens $ sep ["Split", viaShow ys, "++", "..."]

noSplit :: ListSplit a
noSplit = Split Nothing Nothing

splitContents :: ListSplit a -> [a]
splitContents (Split Nothing Nothing) = []
splitContents (Split (Just xs) (Just ys)) = xs ++ ys
splitContents (Split Nothing (Just ys)) = ys
splitContents (Split (Just xs) Nothing) = xs

genFromListSplit :: Arbitrary a => ListSplit a -> Gen [a]
genFromListSplit (Split Nothing Nothing) = vectorOf 4 arbitrary
genFromListSplit (Split (Just xs) (Just ys)) = pure (xs ++ ys)
genFromListSplit (Split Nothing (Just ys)) = (++ ys) <$> arbitrary
genFromListSplit (Split (Just ys) Nothing) = (ys ++) <$> arbitrary

conformSplit :: Eq a => [a] -> ListSplit a -> Bool
conformSplit xs (Split mp ms) =
  maybe True (`isPrefixOf` xs) mp && maybe True (`isSuffixOf` xs) ms

prop_genFromListSplit_sound :: Property
prop_genFromListSplit_sound =
  withMaxSuccess 10000 $
  forAll arbitrary $ \ (pf :: ListSplit Int) ->
  forAll (genFromListSplit pf) $ \ x -> conformSplit x pf

prop_merge_sound :: Property
prop_merge_sound =
  withMaxSuccess 10000 $
  forAll arbitrary $ \ (pf1 :: ListSplit Int) ->
  forAll arbitrary $ \ pf2 ->
  case merge pf1 pf2 of
    Left _ -> discard
    Right pf3 -> forAll (genFromListSplit pf3) $ \ x ->
        counterexample (unlines [show pf1, show pf2, show pf3, show x]) $
          classify (nullListSplit pf1) "null pf1" $
          classify (nullListSplit pf2) "null pf2" $
          classify (nullListSplit pf3) "null pf3" $
          conformSplit x pf1 && conformSplit x pf2 && conformSplit x pf3

nullListSplit :: ListSplit a -> Bool
nullListSplit (Split p s) = null $ fromMaybe [] p ++ fromMaybe [] s

genSmall :: Arbitrary a => Gen (Maybe [a])
genSmall =
  frequency
    [
      ( 3
      , Just <$> do
          n <- choose (1, 5)
          vectorOf n (resize 5 arbitrary)
      )
    , (1, pure Nothing)
    ]

instance Arbitrary a => Arbitrary (ListSplit a) where
  arbitrary = Split <$> genSmall <*> genSmall

-- | if (merge x y) == Right z, then anything that conforms to z, should also conform to both x and y
merge :: (Eq a, Show a) => ListSplit a -> ListSplit a -> Either (NE.NonEmpty String) (ListSplit a)
merge (Split Nothing Nothing) x = Right x
merge x (Split Nothing Nothing) = Right x
merge (Split (Just ps) Nothing) (Split (Just xs) Nothing)
  | isPrefixOf ps xs = Right (Split (Just xs) Nothing)
  | isPrefixOf xs ps = Right (Split (Just ps) Nothing)
  | True = Left $ NE.fromList ["Non matching prefixes in merge" ++ show ps ++ " " ++ show xs]
merge (Split (Just ps) Nothing) (Split Nothing (Just ys)) = Right (mergeOverlap ps ys)
merge p@(Split (Just ps) Nothing) q@(Split (Just xs) (Just ys))
  | isPrefixOf ps (xs ++ ys) = Right q
  | True = Left $ NE.fromList ["Non matching ListSplit in merge", "  " ++ show p, " " ++ show q]
merge (Split Nothing (Just ss)) (Split (Just xs) Nothing) = Right (mergeOverlap xs ss)
merge (Split Nothing (Just ss)) (Split Nothing (Just ys))
  | isSuffixOf ss ys = Right (Split Nothing (Just ys))
  | isSuffixOf ys ss = Right (Split Nothing (Just ss))
  | True = Left $ NE.fromList ["Non matching suffixes in merge" ++ show ss ++ " " ++ show ys]
merge p@(Split Nothing (Just ss)) q@(Split (Just xs) (Just ys))
  | isSuffixOf ss (xs ++ ys) = Right q
  | True = Left $ NE.fromList ["Non matching ListSplit in merge", "  " ++ show p, " " ++ show q]
merge p@(Split (Just ps) (Just ss)) (Split q@(Just xs) Nothing)
  | isPrefixOf xs (ps ++ ss) = Right p
  | True = Left $ NE.fromList ["Non ListSplit in merge", "  " ++ show p, " " ++ show q]
merge p@(Split (Just ps) (Just ss)) q@(Split Nothing (Just ys))
  | isSuffixOf ys (ps ++ ss) = Right p
  | True = Left $ NE.fromList ["Non ListSplit in merge", "  " ++ show p, " " ++ show q]
merge p@(Split (Just ps) (Just ss)) q@(Split (Just xs) (Just ys)) =
  if (ps ++ ss) == (xs ++ ys)
    then Right p
    else Left $ NE.fromList ["Two rigid ListSplits are inconsistent.", "  " ++ show p, "  " ++ show q]

-- | "abcde" "defghi" have an over lap of "de", so return ("abc","de","fgi")
--   "abc" "xyz" do Not have an overlap so return ("abc","","xyz")
overlap :: Eq a => [a] -> [a] -> ([a], [a], [a])
overlap xs [] = (xs, [], [])
overlap xs (y : ys) = first (reverse (elemIndices y xs)) xs (y : ys)
  where
    first [] prefix suffix = (prefix, [], suffix)
    first (n : ns) prefix suffix =
      let (before, after) = splitAt n prefix
       in if isPrefixOf after suffix
            then (before, after, drop (length after) suffix)
            else first ns prefix suffix

mergeOverlap :: Eq a => [a] -> [a] -> ListSplit a
mergeOverlap xs ys = Split (Just (before ++ lap)) (Just after)
  where
    (before, lap, after) = overlap xs ys

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix whole =
  case stripPrefix (reverse suffix) (reverse whole) of
    Nothing -> Nothing
    Just x -> Just (reverse x)

dropSuffix :: Eq a => [a] -> [[a]] -> [[a]]
dropSuffix xs xss = mapMaybe (stripSuffix xs) xss

dropPrefix :: Eq a => [a] -> [[a]] -> [[a]]
dropPrefix xs xss = mapMaybe (stripPrefix xs) xss
