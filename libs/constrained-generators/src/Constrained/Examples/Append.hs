{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.Append where

import Constrained
import Constrained.Base
import Constrained.ListSplit
import Test.QuickCheck (Gen, Property, counterexample, generate, property)
import Prelude hiding (even)

-- =========================================================
-- Useful specs to use in Examples

even :: Specification BaseFn Int
even = MemberSpec [0, 2, 4, 6, 8, 10]

endsWith :: [Int] -> Specification BaseFn [Int]
endsWith suffix = TypeSpec (ListSpec Nothing [] TrueSpec TrueSpec NoFold (Split Nothing (Just suffix))) []

beginsWith :: [Int] -> Specification BaseFn [Int]
beginsWith prefix = TypeSpec (ListSpec Nothing [] TrueSpec TrueSpec NoFold (Split (Just prefix) Nothing)) []

-- | Run an example
go :: forall a. HasSpec BaseFn a => Specification BaseFn a -> IO a
go spec = do
  !ans <- generate $ genFromSpec spec
  pure ans

-- | The spec actually generates something.
succeeds :: forall a. HasSpec BaseFn a => Specification BaseFn a -> Gen Property
succeeds spec = do
  !ans <- genFromSpec @BaseFn @a spec
  pure (property (conformsToSpec ans spec))

-- | The spec fails to generate something
fails :: forall a. HasSpec BaseFn a => Specification BaseFn a -> Gen Property
fails spec = do
  ans <- catchGen (genFromSpecT spec)
  case ans of
    Left _ -> pure (property True)
    Right _ ->
      pure (counterexample ("\nFAILING test does not fail as expected.\n" ++ show spec) (property False))

-- =============================================
-- The examples

-- | Completely constrained by Split
app1 :: Specification BaseFn [Int]
app1 = constrained $
  \x -> assert $ x ==. append_ (lit [1, 2, 3]) (lit [4, 5, 6])

-- | Fails because the cant set is over constrained
app2 :: Specification BaseFn ([Int], [Int])
app2 = constrained' $
  \ [var|x|] [var|y|] ->
    [ dependsOn y x
    , assert $ x ==. append_ (lit [1, 2, 3]) y
    , assert $ y ==. lit [4, 5, 6]
    , assert $ x /=. lit [1, 2, 3, 4, 5, 6]
    ]

-- | x and y only constrained by suffix
appSuffix2 :: Specification BaseFn ([Int], [Int])
appSuffix2 = constrained' $
  \ [var|x|] [var|y|] -> assert $ x ==. append_ y (lit [4, 5, 6])

-- | test that the Split is constrained by even
appSuffix3 :: Specification BaseFn ([Int], [Int])
appSuffix3 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ y (lit [4, 6])
    ]

-- | test that the Split fails because suffix has element not even
appSuffix4 :: Specification BaseFn ([Int], [Int])
appSuffix4 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ y (lit [4, 5, 6]) -- 5 fails even
    ]

-- | test that the suffix makes the length too long for the size constraint
appSuffix5 :: Specification BaseFn ([Int], [Int])
appSuffix5 = constrained' $
  \ [var|x|] [var|y|] ->
    [ assert $ sizeOf_ x <=. 4
    , assert $ x ==. append_ y (lit [1, 2, 3, 4, 5])
    ]

-- | test that 'x' is overconstrained in the suffix.
appSuffix6 :: Specification BaseFn ([Int])
appSuffix6 = constrained $
  \ [var|x|] -> [satisfies x (endsWith [1, 2]), satisfies x (endsWith [3, 4])]

-- | test that 'x' has conflicting suffix requirements
appSuffix7 :: Specification BaseFn ([Int], [Int])
appSuffix7 = constrained' $
  \ [var|x|] [var|y|] -> [assert $ x ==. append_ (lit [1, 2, 3]) (lit [4, 5, 6]), assert $ x ==. append_ y (lit [1, 2])]

-- | another test that 'x' has conflicting suffix requirements
appSuffix8 :: Specification BaseFn ([Int], [Int])
appSuffix8 = constrained' $
  \ [var|x|] [var|y|] -> [satisfies x (endsWith [3, 4]), assert $ x ==. append_ y (lit [1, 2])]

-- | x and y only constrained by prefix
appPrefix2 :: Specification BaseFn ([Int], [Int])
appPrefix2 = constrained' $
  \ [var|x|] [var|y|] -> assert $ x ==. append_ (lit [4, 5, 6]) y

-- | test that the Split is constrained by even
appPrefix3 :: Specification BaseFn ([Int], [Int])
appPrefix3 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ (lit [4, 6]) y
    ]

-- | test that the Split fails because prefix has element not even
appPrefix4 :: Specification BaseFn ([Int], [Int])
appPrefix4 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ (lit [4, 5, 6]) y -- 5 fails even
    ]

-- | test that the prefix makes the length too long for the size constraint
appPrefix5 :: Specification BaseFn ([Int], [Int])
appPrefix5 = constrained' $
  \ [var|x|] [var|y|] ->
    [ assert $ sizeOf_ x <=. 4
    , assert $ x ==. append_ (lit [1, 2, 3, 4, 5]) y
    ]

-- | test that 'x' is overconstrained in the prefix.
appPrefix6 :: Specification BaseFn ([Int])
appPrefix6 = constrained $
  \ [var|x|] -> [satisfies x (beginsWith [1, 2]), satisfies x (beginsWith [3, 4])]

-- | test that 'y' fills out what is missing from 'x'
appPrefix7 :: Specification BaseFn ([Int], [Int])
appPrefix7 = constrained' $
  \ [var|x|] [var|y|] -> [assert $ x ==. append_ (lit [1, 2, 3]) (lit [4, 5, 6]), assert $ x ==. append_ (lit [1, 2]) y]

-- | test that prefix is ambiguous
appPrefix8 :: Specification BaseFn ([Int], [Int])
appPrefix8 = constrained' $
  \ [var|x|] [var|y|] -> [satisfies x (beginsWith [3, 4]), assert $ x ==. append_ (lit [1, 2]) y]

-- | test that suffix and size interact correctly constrain the prefix in the HOLE.
appPrefix9 :: Specification BaseFn ([Int])
appPrefix9 = constrained $
  \ [var|y|] ->
    [ satisfies y (endsWith [3, 4] <> (hasSize $ NumSpecInterval (Just 2) (Just 2)))
    ]

-- | test that suffix and size interact to constrain the size of the prefix in the HOLE.
appPrefix10 :: Specification BaseFn ([Int])
appPrefix10 = constrained $
  \ [var|y|] ->
    [ satisfies
        y
        (TypeSpec (ListSpec Nothing [] (MemberSpec [5]) TrueSpec NoFold (Split Nothing (Just [1, 2]))) [])
    ]

-- | test that prefix and size interact to constrain the size of the suffix in the HOLE.
appSuffix11 :: Specification BaseFn ([Int])
appSuffix11 = constrained $
  \ [var|y|] ->
    [ satisfies
        y
        (TypeSpec (ListSpec Nothing [] (MemberSpec [5]) TrueSpec NoFold (Split (Just [1, 2]) Nothing)) [])
    ]
