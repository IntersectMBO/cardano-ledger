module Main where

import Constrained.Tests
import Data.Maybe
import System.Environment
import Test.Hspec

main :: IO ()
main = do
  nightly <- isJust <$> lookupEnv "NIGHTLY"
  hspec $ tests nightly
