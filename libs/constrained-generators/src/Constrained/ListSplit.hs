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
import Data.Maybe (mapMaybe)
import Prettyprinter
import Test.QuickCheck

data ListSplit a = Split [a] [a] deriving (Eq, Show)

instance Show a => Pretty (ListSplit a) where
  pretty (Split [] []) = "noSplit"
  pretty (Split xs ys) = parens $ sep ["Split", viaShow xs, viaShow ys]

noSplit :: ListSplit a
noSplit = Split [] []

splitContents :: ListSplit a -> [a]
splitContents (Split xs ys) = xs ++ ys

genFromListSplit :: (Arbitrary a, Eq a) => ListSplit a -> Gen [a]
genFromListSplit (Split xs ys) =
  -- TODO: this misses things like [1,2,3,3] [3,3,1] -> [1,2,3,3,3,1]?
  frequency [ (4, (xs ++) . (++ ys) <$> arbitrary)
            , (1, let (pre, join, post) = overlap xs ys in pure (pre ++ join ++ post))
            ]

conformSplit :: Eq a => [a] -> ListSplit a -> Bool
conformSplit xs (Split p s) =
  p `isPrefixOf` xs && s `isSuffixOf` xs

prop_genFromListSplit_sound :: Property
prop_genFromListSplit_sound =
  withMaxSuccess 10000 $
  forAll arbitrary $ \ (pf :: ListSplit Int) ->
  forAll (genFromListSplit pf) $ \ x -> conformSplit x pf

prop_merge_sound :: Property
prop_merge_sound =
  withMaxSuccess 10000 $
  forAllShrink arbitrary shrink $ \ (pf1 :: ListSplit Int) ->
  forAllShrink arbitrary shrink $ \ pf2 ->
  case merge pf1 pf2 of
    Left _ -> discard
    Right pf3 -> counterexample (show pf3) $
        forAll (genFromListSplit pf3) $ \ x ->
          classify (nullListSplit pf1) "null pf1" $
          classify (nullListSplit pf2) "null pf2" $
          classify (nullListSplit pf3) "null pf3" $
          conformSplit x pf1 && conformSplit x pf2 && conformSplit x pf3

nullListSplit :: ListSplit a -> Bool
nullListSplit (Split p s) = null p && null s

genSmall :: Arbitrary a => Gen [a]
genSmall = do
  n <- choose (0, 5)
  vectorOf n (resize 5 arbitrary)

instance Arbitrary a => Arbitrary (ListSplit a) where
  arbitrary = Split <$> genSmall <*> genSmall

  shrink (Split xs ys) = map (uncurry Split) (shrink (xs, ys))

-- | if (merge x y) == Right z, then anything that conforms to z, should also conform to both x and y
merge :: (Eq a, Show a) => ListSplit a -> ListSplit a -> Either (NE.NonEmpty String) (ListSplit a)
merge (Split p s) (Split p' s') = do
  p'' <- prefix
  s'' <- suffix
  return $ Split p'' s''
  where
    prefix
      | p `isPrefixOf` p' = return p'
      | p' `isPrefixOf` p = return p
      | otherwise         = Left $ pure $ "Incompatible prefixes: " ++ show p ++ " " ++ show p'
    suffix
      | s `isSuffixOf` s' = return s'
      | s' `isSuffixOf` s = return s
      | otherwise         = Left $ pure $ "Incompatible suffixes: " ++ show s ++ " " ++ show s'

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
