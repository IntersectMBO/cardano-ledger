module Main where

import NonIntegral

import Data.Ratio ((%))

rational :: IO ()
rational = do
  print (fromRational $ 2 *** (1%2))
  print (fromRational $ 3 *** (1%2))

double :: IO ()
double = do
  print (2.0 *** 0.5)
  print (3.0 *** 0.5)

main :: IO ()
main = do
  double
  rational
