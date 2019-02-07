module Data.Aeson.Options
  ( defaultOptions
  )
where

import Control.Category ((.))
import Data.Char (isLower, isPunctuation, isUpper, toLower)
import Data.List (drop, dropWhile, filter, findIndex)
import Data.String (String)

import qualified Data.Aeson as A

import Cardano.Prelude (Int, (-), ($), maybe, not, flip)

headToLower :: String -> String
headToLower []       = []
headToLower (x : xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

stripConstructorPrefix :: String -> String
stripConstructorPrefix t = maybe t (flip drop t . decrementSafe)
  $ findIndex isLower t
 where
  decrementSafe :: Int -> Int
  decrementSafe 0 = 0
  decrementSafe i = i - 1

-- | These options do the following transformations:
-- 1. Names of field
-- records are assumed to be camelCased, `camel` part is removed,
-- `Cased` part is converted to `cased`. So `camelCased` becomes
-- `cased`. Also all punctuation symbols are dropped before doing it.
-- 2. Constructors are assumed to start with some capitalized prefix
-- (which finished right before the last capital letter). This prefix
-- is dropped and then the first letter is lowercased.
defaultOptions :: A.Options
defaultOptions = A.defaultOptions
  { A.fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
  , A.constructorTagModifier = headToLower . stripConstructorPrefix
  , A.sumEncoding        = A.ObjectWithSingleField
  }
