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
epsD = 1.0 / 10^10

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

--prop_IdemPotent :: Integer -> Integer -> Property
prop_IdemPotent :: Rational -> Property
prop_IdemPotent a =
    a > 0 ==> (exp' $ ln' a) - a < eps
    --b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    --where (a'', b'') = normalizeInts a b

prop_IdemPotent' :: Integer -> Integer -> Property
prop_IdemPotent' a b =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (b'' % a'')) - (b'' % a'') < eps
    where (a'', b'') = normalizeInts a b

-----------------------------------
-- Double versions of properties --
-----------------------------------

prop_DMonotonic ::
     (Double -> Bool) -> (Double -> Double) -> Double -> Double -> Property
prop_DMonotonic constrain f x y =
  (constrain x && constrain y) ==>
  if x <= y
    then f x <= f y
    else f x > f y

-- | Takes very long, but (e *** b) *** c is not an operation that we use.
prop_DExpLaw :: Integer -> Integer -> Integer -> Integer -> Property
prop_DExpLaw x y a b =
    b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==> expdiffD x'' y'' a'' b'' < epsD
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b

expdiffD :: Integer -> Integer -> Integer -> Integer -> Double
expdiffD x'' y'' a'' b'' =
    -- trace (show x'' ++ " "++ show y'' ++ " "
    --     ++ show a'' ++ " " ++ show b'' ++ " e1: "
    --     ++ show e1 ++ " e2: " ++ show e2) $
    abs(e1 - e2)
      where e1 = (((fromIntegral b'' / fromIntegral a'') *** (1.0 / fromIntegral x'')) *** fromIntegral y'')
            e2 = (((fromIntegral b'' / fromIntegral a'') *** fromIntegral y'') *** (1.0/ fromIntegral x''))

prop_DExpUnitInterval :: Integer -> Integer -> Integer -> Integer -> Property
prop_DExpUnitInterval x y a b =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

--prop_DIdemPotent :: Integer -> Integer -> Property
prop_DIdemPotent :: Double -> Property
prop_DIdemPotent a =
    a > 0 ==> (exp' $ ln' a) - a < epsD
    --b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    --where (a'', b'') = normalizeInts a b

prop_DIdemPotent' :: Integer -> Integer -> Property
prop_DIdemPotent' a b =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::Double) - ((fromIntegral b'' / fromIntegral a'')::Double) < epsD
    where (a'', b'') = normalizeInts a b


main :: IO ()
main = do
  -- putStrLn "test Rationals"
  -- putStrLn "property exp is monotonic"
  -- quickCheck $ prop_Monotonic (const True) exp'
  -- putStrLn "property ln is monotonic"
  -- quickCheck $ prop_Monotonic (> 0) ln'
  -- putStrLn "property sqrt is monotonic"
  -- quickCheck $ prop_Monotonic (> 0) (\x -> x *** 0.5)
  -- putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  -- quickCheck prop_ExpUnitInterval
  -- putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  -- quickCheck prop_IdemPotent
  -- putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  -- quickCheck prop_IdemPotent'
  -- putStrLn "prop9"
  -- quickCheck (withMaxSuccess 40 prop_ExpLaw)

  putStrLn "test Double"
  putStrLn "property exp is monotonic"
  quickCheck $ prop_DMonotonic (const True) exp'
  putStrLn "property ln is monotonic"
  quickCheck $ prop_DMonotonic (> 0) ln'
  putStrLn "property sqrt is monotonic"
  quickCheck $ prop_DMonotonic (> 0) (\x -> x *** 0.5)
  putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  quickCheck prop_DExpUnitInterval
  putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  quickCheck prop_DIdemPotent
  putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  quickCheck prop_DIdemPotent'
