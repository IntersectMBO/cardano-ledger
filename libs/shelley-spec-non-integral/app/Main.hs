module Main where

import qualified Data.Fixed as FP
import NonIntegral
import System.IO (isEOF)

data E34

instance FP.HasResolution E34 where
  resolution _ = 10000000000000000000000000000000000

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

precision :: FixedPoint
precision = 10000000000000000000000000000000000

epsilon :: FixedPoint
epsilon = 100000000000000000

doTestsFromStdin :: IO ()
doTestsFromStdin = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      let splitLine = words line
      let x = (read (splitLine !! 0) :: FixedPoint) / precision
      let a = (read (splitLine !! 1) :: FixedPoint) / precision
      let b = (read (splitLine !! 2) :: FixedPoint) / precision
      let c = ln' (1 - f)
      let res = taylorExpCmp 3 (1 / (1 - a)) (- (b * c))
      putStrLn $
        show (exp' x) ++ " "
          ++ show (- (ln' a))
          ++ " "
          ++ show (1 - ((1 - f) *** b))
          ++ " "
          ++ ( case res of
                 BELOW acc n -> (show acc) ++ " LT " ++ show n
                 ABOVE acc n -> (show acc) ++ " GT " ++ show n
                 MaxReached _ -> "error"
             )
      doTestsFromStdin

f :: FixedPoint
f = 1 / 10

main :: IO ()
main = doTestsFromStdin
