{-# LANGUAGE ImportQualifiedPost #-}

module Constrained.Examples.Either where

import Data.Set qualified as Set

import Constrained

eitherSpec :: Specification BaseFn (Either Int Int)
eitherSpec = constrained $ \e ->
  (caseOn e)
    (branch $ \i -> i <=. 0)
    (branch $ \i -> 0 <=. i)

foldTrueCases :: Specification BaseFn (Either Int Int)
foldTrueCases = constrained $ \x ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [Left 10]))
  , letBind (pair_ x (lit (0 :: Int))) $ \p ->
      caseOn
        (fst_ p)
        (branch $ \_ -> True)
        (branch $ \_ -> True)
  ]
