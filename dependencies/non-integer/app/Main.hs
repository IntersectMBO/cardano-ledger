{-# LANGUAGE EmptyDataDecls        #-}

module Main where

import System.IO (isEOF)

import NonIntegral

import qualified Data.Fixed as FP

data E34

instance FP.HasResolution E34 where
    resolution _ = 10000000000000000000000000000000000

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

precision :: FixedPoint
precision = 10000000000000000000000000000000000

epsilon :: FixedPoint
epsilon = 100000000000000000

main :: IO ()
main = do
  b <- isEOF
  if b then return ()
    else do
    line <- getLine
    let base     = read (takeWhile (/= ' ') line)        :: FixedPoint
    let exponent = read (tail $ dropWhile (/= ' ') line) :: FixedPoint
    putStrLn $ show ((base / precision) *** (exponent / precision))
            ++ " " ++ show (exp'' (base / precision))
    main
