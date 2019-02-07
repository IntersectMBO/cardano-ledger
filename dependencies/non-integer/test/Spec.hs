{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

import           Data.Ratio ((%))

import           Test.QuickCheck

import           NonIntegral

epsD :: Double
epsD   = 1.0 / 10.0^(12::Integer)

-- | Normalizes the integers, return a pair of integers, both non-negative and
-- fst <= snd.
normalizeInts :: Integer -> Integer -> (Integer, Integer)
normalizeInts x y = (x'', y'')
    where x' = abs x
          y' = abs y
          x'' = max x' y'
          y'' = min x' y'

type PosInt = Positive Integer

-- | Takes very long, but (e *** b) *** c is not an operation that we use.
-- prop_ExpLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
-- prop_ExpLaw (Positive x) (Positive y) (Positive a) (Positive b) =
--     b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==> expdiff x'' y'' a'' b'' < eps
--     where (x'', y'') = normalizeInts x y
--           (a'', b'') = normalizeInts a b

-- expdiff :: Integer -> Integer -> Integer -> Integer -> Rational
-- expdiff x'' y'' a'' b'' =
--     trace (show x'' ++ " "++ show y'' ++ " "
--         ++ show a'' ++ " " ++ show b'' ++ " e1: "
--         ++ show (fromRational e1) ++ " e2: " ++ show (fromRational e2)) $
--     abs(e1 - e2)
--       where e1 = (((b'' % a'') *** (1% x'')) *** fromIntegral y'')
--             e2 = (((b'' % a'') *** fromIntegral y'') *** (1% x''))

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
prop_DExpLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_DExpLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==> expdiffD x'' y'' a'' b'' < epsD
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b

prop_DExpLaw' :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_DExpLaw' (Positive x) (Positive y) (Positive a) (Positive b) =
    (abs (exp' (a'/b' + x'/y') - (exp'(a'/b') * exp'(x'/y'))) < epsD) === True
        where (b'', a'') = normalizeInts a b
              (y'', x'') = normalizeInts x y
              a' = fromIntegral a''
              b' = fromIntegral b''
              x' = fromIntegral x''
              y' = fromIntegral y''

expdiffD :: Integer -> Integer -> Integer -> Integer -> Double
expdiffD x'' y'' a'' b'' =
    -- trace (show x'' ++ " "++ show y'' ++ " "
    --     ++ show a'' ++ " " ++ show b'' ++ " e1: "
    --     ++ show e1 ++ " e2: " ++ show e2) $
    abs(e1 - e2)
      where e1 = (((fromIntegral b'' / fromIntegral a'') *** (1.0 / fromIntegral x'')) *** fromIntegral y'')
            e2 = (((fromIntegral b'' / fromIntegral a'') *** fromIntegral y'') *** (1.0/ fromIntegral x''))

prop_DExpUnitInterval :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_DExpUnitInterval (Positive x) (Positive y) (Positive a) (Positive b) =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

prop_DIdemPotent :: Positive Double -> Property
prop_DIdemPotent (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < epsD
    --b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    --where (a'', b'') = normalizeInts a b

prop_DIdemPotent' :: PosInt -> PosInt -> Property
prop_DIdemPotent' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::Double) - ((fromIntegral b'' / fromIntegral a'')::Double) < epsD
    where (a'', b'') = normalizeInts a b

prop_DfindD :: Positive Double -> Property
prop_DfindD (Positive a) = (e ^^ n <= a && e ^^ (n + 1) > a) === True
    where e = exp' 1
          n = findE a

prop_DlnLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_DlnLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    ((ln' ((a''/b'') *** (x''/y'')) - (x''/y'') * ln' (a''/b'')) < epsD) === True
    where (b', a') = normalizeInts a b
          (y', x') = normalizeInts x y
          a'' = fromIntegral a'
          b'' = fromIntegral b'
          x'' = fromIntegral x'
          y'' = fromIntegral y'

main :: IO ()
main = do
  putStrLn "quickcheck properties for non-integral calculation\n"

  putStrLn "------------------------"
  putStrLn "-- Test of `Double` --"
  putStrLn "------------------------"
  putStrLn "property exp is monotonic"
  quickCheck (withMaxSuccess 1000 $ prop_DMonotonic (const True) exp')
  putStrLn "property ln is monotonic"
  quickCheck (withMaxSuccess 1000 $ prop_DMonotonic (> 0) ln')
  putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  quickCheck (withMaxSuccess 1000 prop_DExpUnitInterval)
  putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  quickCheck (withMaxSuccess 1000 prop_DIdemPotent)
  putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  quickCheck (withMaxSuccess 1000 prop_DIdemPotent')
  putStrLn "property exponential law in [0,1]: (((a/b)^1/x)^y) = (((a/b)^y)^1/x)"
  quickCheck (withMaxSuccess 1000 prop_DExpLaw)
  putStrLn "property exponential law in [0,1]: exp(q * p) = exp(q) + exp(p)"
  quickCheck (withMaxSuccess 1000 prop_DExpLaw')
  putStrLn "property ln law in [0,1]: ln(q^p) = p*ln(q)"
  quickCheck (withMaxSuccess 1000 prop_DlnLaw)
  putStrLn "check bound of `findE`"
  quickCheck (withMaxSuccess 1000 prop_DfindD)
  putStrLn ""
