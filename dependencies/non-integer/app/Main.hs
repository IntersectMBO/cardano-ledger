module Main where

import NonIntegral

import Data.Ratio ((%))

main :: IO ()
main = do
  --putStrLn $ show (fromRational $ exp' 0.1234567)
  putStrLn $ show (fromRational $ 2 *** (1%2))
  putStrLn $ show (fromRational $ 3 *** (1%2))
