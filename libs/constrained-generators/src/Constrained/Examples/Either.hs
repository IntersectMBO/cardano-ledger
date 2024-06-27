{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.Either where

import Data.Set qualified as Set

import Constrained

eitherSpec :: Specification BaseFn (Either Int Int)
eitherSpec = constrained $ \ [var|e|] ->
  (caseOn e)
    (branch $ \ [var|left|] -> left <=. 0)
    (branch $ \ [var|right|] -> 0 <=. right)

foldTrueCases :: Specification BaseFn (Either Int Int)
foldTrueCases = constrained $ \ [var|x|] ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [Left 10]))
  , letBind (pair_ x (lit (0 :: Int))) $ \ [var|p|] ->
      [ dependsOn p x
      , caseOn
          (fst_ p)
          (branch $ \_ -> True)
          (branch $ \_ -> True)
      ]
  ]
