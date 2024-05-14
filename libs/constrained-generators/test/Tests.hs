module Main where

import Constrained.Test
import Data.Maybe
import System.Environment
import Test.Hspec

main :: IO ()
main = do
  nightly <- isJust <$> lookupEnv "NIGHTLY"
  hspec $ tests nightly
