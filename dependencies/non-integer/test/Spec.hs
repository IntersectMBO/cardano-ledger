{-# LANGUAGE PartialTypeSignatures #-}

import           Debug.Trace

import           Data.Ratio ((%))

import           Test.QuickCheck

import           NonIntegral

prop_Monotonic ::
     (Rational -> Bool) -> (Rational -> Rational) -> Rational -> Rational -> Property
prop_Monotonic constrain f x y =
  (constrain x && constrain y) ==>
  if x <= y
    then f x <= f y
    else f x > f y

eps = 1 / 10^10

normalizeInts :: Integer -> Integer -> (Integer, Integer)
normalizeInts x y = (x'', y'')
    where x' = abs x
          y' = abs y
          x'' = max x' y'
          y'' = min x' y'

-- | Takes very long, but (e *** b) *** c is not an operation that we use.
prop_ExpLaw :: Integer -> Integer -> Integer -> Integer -> Property
prop_ExpLaw x y a b =
    b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==> expdiff x'' y'' a'' b'' < eps
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b

expdiff :: Integer -> Integer -> Integer -> Integer -> Rational
expdiff x'' y'' a'' b'' =
    trace (show x'' ++ " "++ show y'' ++ " "
        ++ show a'' ++ " " ++ show b'' ++ " e1: "
        ++ show (fromRational e1) ++ " e2: " ++ show (fromRational e2)) $
    abs(e1 - e2)
      where e1 = (((b'' % a'') *** (1% x'')) *** fromIntegral y'')
            e2 = (((b'' % a'') *** fromIntegral y'') *** (1% x''))

prop_ExpUnitInterval :: Integer -> Integer -> Integer -> Integer -> Property
prop_ExpUnitInterval x y a b =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

prop_IdemPotent :: Integer -> Integer -> Property
prop_IdemPotent a b =
    b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    where (a'', b'') = normalizeInts a b

prop_IdemPotent' :: Integer -> Integer -> Property
prop_IdemPotent' a b =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (b'' % a'')) - (b'' % a'') < eps
    where (a'', b'') = normalizeInts a b

prop_LnSplit :: Integer -> Integer -> Property
prop_LnSplit a b =
    a > 0 && b > 0 ==> x' >= 0
      where (_, x') = splitLn (a%b)

main :: IO ()
main = do
  -- putStrLn "prop1"
  -- quickCheck (withMaxSuccess 1000 $ prop_Monotonic (const True) exp')
  -- putStrLn "prop2"
  -- quickCheck (withMaxSuccess 1000 $ prop_Monotonic (> 0) ln')
  -- putStrLn "prop3"
  -- quickCheck (withMaxSuccess 1000 $ prop_Monotonic (> 0) (\x -> x *** 0.5))
  -- putStrLn "prop5"
  -- quickCheck (withMaxSuccess 1000 prop_ExpUnitInterval)
  -- putStrLn "prop6"
  -- quickCheck (withMaxSuccess 1000 prop_IdemPotent)
  -- putStrLn "prop7"
  -- quickCheck (withMaxSuccess 1000 prop_IdemPotent')
  -- putStrLn "prop8"
  -- quickCheck (withMaxSuccess 1000 prop_spltiLn)
  putStrLn "prop9"
  quickCheck (withMaxSuccess 40 prop_ExpLaw)
