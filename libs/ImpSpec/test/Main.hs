module Main where

import Test.ImpSpec
import Test.Suite.ImpSpec (spec)

main :: IO ()
main = impSpecMain $ describe "ImpSpec" spec
