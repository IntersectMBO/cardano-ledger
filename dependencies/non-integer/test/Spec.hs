{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

import           Debug.Trace

import           Data.Ratio ((%))

import qualified Data.FixedPoint as FBV

import           Test.QuickCheck

import           NonIntegral

eps :: Rational
eps    = 1 / 10^12

epsD :: Double
epsD   = 1.0 / 10^5

epsFBV :: FixedPoint
epsFBV = fromRational eps

prop_Monotonic ::
     (Rational -> Bool) -> (Rational -> Rational) -> Rational -> Rational -> Property
prop_Monotonic constrain f x y =
  (constrain x && constrain y) ==>
  if x <= y
    then f x <= f y
    else f x > f y

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

prop_ExpLaw' :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_ExpLaw' (Positive x) (Positive y) (Positive a) (Positive b) =
    (abs (exp' ( (a'%b') + (x'%y')) - (exp'(a'%b') * exp' (x'%y'))) < eps) === True
    where (b', a') = normalizeInts a b
          (y', x') = normalizeInts x y

prop_ExpUnitInterval :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_ExpUnitInterval (Positive x) (Positive y) (Positive a) (Positive b) =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

--prop_IdemPotent :: Integer -> Integer -> Property
prop_IdemPotent :: Positive Rational -> Property
prop_IdemPotent (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < eps
    --b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    --where (a'', b'') = normalizeInts a b

prop_IdemPotent' :: PosInt -> PosInt -> Property
prop_IdemPotent' (Positive a) (Positive b) =
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

prop_DExpUnitInterval :: Integer -> Integer -> Integer -> Integer -> Property
prop_DExpUnitInterval x y a b =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

--prop_DIdemPotent :: Integer -> Integer -> Property
prop_DIdemPotent :: Positive Double -> Property
prop_DIdemPotent (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < epsD
    --b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    --where (a'', b'') = normalizeInts a b

prop_DIdemPotent' :: PosInt -> PosInt -> Property
prop_DIdemPotent' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::Double) - ((fromIntegral b'' / fromIntegral a'')::Double) < epsD
    where (a'', b'') = normalizeInts a b


-----------------------------------------
-- Fixed-point versions of properties  --
-----------------------------------------

type FixedPoint = FBV.FixedPoint256256

instance Arbitrary FixedPoint where
    arbitrary = do
      NonNegative a <- arbitrary
      Positive b    <- arbitrary
      pure $ fromRational (a%b)
    shrink _ = [] -- don't try to shrink values of fixedpoint

prop_FBVMonotonic ::
     (FixedPoint -> Bool) -> (FixedPoint -> FixedPoint) -> FixedPoint -> FixedPoint -> Property
prop_FBVMonotonic constrain f x y =
  (constrain x && constrain y) ==>
  if x <= y
    then f x <= f y
    else f x > f y

-- | Takes very long, but (e *** b) *** c is not an operation that we use.
prop_FBVExpLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FBVExpLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==> expdiffFBV x'' y'' a'' b'' < epsFBV
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b

prop_FBVExpLaw' :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FBVExpLaw' (Positive x) (Positive y) (Positive a) (Positive b) =
    (abs (exp' (a'/b' + x'/y') - (exp'(a'/b') * exp'(x'/y'))) < epsFBV) === True
        where (b'', a'') = normalizeInts a b
              (y'', x'') = normalizeInts x y
              a' = fromIntegral a''
              b' = fromIntegral b''
              x' = fromIntegral x''
              y' = fromIntegral y''

expdiffFBV :: Integer -> Integer -> Integer -> Integer -> FixedPoint
expdiffFBV x'' y'' a'' b'' =
    -- trace (show x'' ++ " "++ show y'' ++ " "
    --     ++ show a'' ++ " " ++ show b'' ++ " e1: "
    --     ++ show e1 ++ " e2: " ++ show e2) $
    abs(e1 - e2)
      where e1 = (((fromIntegral b'' / fromIntegral a'') *** (1.0 / fromIntegral x'')) *** fromIntegral y'')
            e2 = (((fromIntegral b'' / fromIntegral a'') *** fromIntegral y'') *** (1.0/ fromIntegral x''))

prop_FBVExpUnitInterval :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FBVExpUnitInterval (Positive x) (Positive y) (Positive a) (Positive b) =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

--prop_FBVIdemPotent :: Integer -> Integer -> Property
prop_FBVIdemPotent :: FixedPoint -> Property
prop_FBVIdemPotent a =
    a > 0 ==> (exp' $ ln' a) - a < epsFBV
    --b'' > 0 && a'' > 0 ==> (exp' $ ln' (b'' % a'')) - (b'' % a'') < eps
    --where (a'', b'') = normalizeInts a b

prop_FBVIdemPotent' :: PosInt -> PosInt -> Property
prop_FBVIdemPotent' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::FixedPoint) - ((fromIntegral b'' / fromIntegral a'')::FixedPoint) < epsFBV
    where (a'', b'') = normalizeInts a b

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
  putStrLn "property sqrt is monotonic"
  quickCheck (withMaxSuccess 1000 $ prop_DMonotonic (> 0) (\x -> x *** 0.5))
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
  putStrLn ""

  putStrLn "----------------------"
  putStrLn "-- Test of `Rational`--"
  putStrLn "----------------------"
  putStrLn "property exp is monotonic"
  quickCheck $ prop_Monotonic (const True) exp'
  putStrLn "property ln is monotonic"
  quickCheck $ prop_Monotonic (> 0) ln'
  putStrLn "property sqrt is monotonic"
  quickCheck $ prop_Monotonic (> 0) (\x -> x *** 0.5)
  putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  quickCheck prop_ExpUnitInterval
  putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  quickCheck prop_IdemPotent
  putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  quickCheck prop_IdemPotent'
  -- putStrLn "prop9"
  -- quickCheck (withMaxSuccess 40 prop_ExpLaw)
  putStrLn "property exponential law in [0,1]: exp(q * p) = exp(q) + exp(p)"
  quickCheck prop_ExpLaw'
  putStrLn ""

  putStrLn "------------------------------"
  putStrLn "-- Test of FixedPoint256256 --"
  putStrLn "------------------------------"
  putStrLn "property exp is monotonic"
  quickCheck (withMaxSuccess 100 $ prop_FBVMonotonic (const True) exp')
  putStrLn "property ln is monotonic"
  quickCheck (withMaxSuccess 100 $ prop_FBVMonotonic (> 0) ln')
  putStrLn "property sqrt is monotonic"
  quickCheck (withMaxSuccess 100 $ prop_FBVMonotonic (> 0) (\x -> x *** fromRational (1%2)))
  putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  quickCheck (withMaxSuccess 100 prop_FBVExpUnitInterval)
  putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  quickCheck (withMaxSuccess 100 prop_FBVIdemPotent)
  putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  quickCheck (withMaxSuccess 100 prop_FBVIdemPotent')
  putStrLn "property exponential law in [0,1]: (((a/b)^1/x)^y) = (((a/b)^y)^1/x)"
  quickCheck (withMaxSuccess 100 prop_FBVExpLaw)
  putStrLn "property exponential law in [0,1]: exp(q * p) = exp(q) + exp(p)"
  quickCheck (withMaxSuccess 100 prop_FBVExpLaw')
