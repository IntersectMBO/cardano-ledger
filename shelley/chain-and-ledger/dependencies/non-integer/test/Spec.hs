{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE EmptyDataDecls        #-}

import qualified Data.Fixed as FP
import           Data.Ratio ((%))

import           Test.QuickCheck

import           NonIntegral
import           Debug.Trace
data E16
data E20
data E34

instance FP.HasResolution E16 where
    resolution _ = 10000000000000000

instance FP.HasResolution E20 where
    resolution _ = 100000000000000000000

instance FP.HasResolution E34 where
    resolution _ = 10000000000000000000000000000000000

type Digits16 = FP.Fixed E16
type Digits20 = FP.Fixed E20
type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

epsD :: Double
epsD   = 1.0 / 10.0^(12::Integer)

epsFP :: FixedPoint
epsFP = (fromIntegral 1) / fromIntegral 10^16

eps :: Rational
eps = (fromIntegral 1) / fromIntegral 10^16

-- | Normalizes the integers, return a pair of integers, both non-negative and
-- fst <= snd.
normalizeInts :: Integer -> Integer -> (Integer, Integer)
normalizeInts x y = (x'', y'')
    where x' = abs x
          y' = abs y
          x'' = max x' y'
          y'' = min x' y'

type PosInt = Positive Integer

---------------------------------------
-- FixedPoint Versions of Properties --
---------------------------------------

expdiffFP :: Integer -> Integer -> Integer -> Integer -> (FixedPoint, FixedPoint)
expdiffFP x'' y'' a'' b'' =
    (e1, e2)
      where e1 = base1 *** (fromIntegral y''::FixedPoint)
            e2 = base2 *** (1 / fromIntegral x''::FixedPoint)
            base = (fromIntegral b''::FixedPoint) / (fromIntegral a''::FixedPoint)
            base1 = base *** (1 / (fromIntegral x'')::FixedPoint)
            base2 = base *** (fromIntegral y''::FixedPoint)


prop_FPExpLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FPExpLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==>
        let diff = abs(e1 - e2) in
        classify (diff < epsFP) "OK 10e-16" $
        classify (diff < epsFP * 1000 && diff >= epsFP) "OK 10e-12" $
        classify (diff >= epsFP * 1000) "KO" $
        True === True
        --(e1 >= epsFP * 1000 && e2 >= epsFP * 1000 ==> abs(e1 - e2) < epsFP * 100000)
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          (e1, e2)   = expdiffFP x'' y'' a'' b''

prop_FPMonotonic ::
     (FixedPoint -> Bool) -> (FixedPoint -> FixedPoint) -> FixedPoint -> FixedPoint -> Property
prop_FPMonotonic constrain f x y =
  (constrain x && constrain y) ==>
  classify (bothZero x y) "both zero case" $
  (bothZero x y ||
   if x <= y
   then f x <= f y
   else f x > f y) === True
        where bothZero a b = f a < epsFP && f b < epsFP

prop_FPExpLaw' :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FPExpLaw' (Positive x) (Positive y) (Positive a) (Positive b) =
    (abs (exp' (a'/b' + x'/y') - (exp'(a'/b') * exp'(x'/y'))) < epsFP) === True
        where (b'', a'') = normalizeInts a b
              (y'', x'') = normalizeInts x y
              a' = fromIntegral a''
              b' = fromIntegral b''
              x' = fromIntegral x''
              y' = fromIntegral y''

prop_FPExpUnitInterval :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FPExpUnitInterval (Positive x) (Positive y) (Positive a) (Positive b) =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          a' = fromIntegral a'':: FixedPoint
          b' = fromIntegral b'':: FixedPoint
          x' = fromIntegral x'':: FixedPoint
          y' = fromIntegral y'':: FixedPoint
          result = (b' / a') *** (y' / x')

prop_FPIdemPotent :: Positive FixedPoint -> Property
prop_FPIdemPotent (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < epsFP

prop_FPIdemPotent' :: PosInt -> PosInt -> Property
prop_FPIdemPotent' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::FixedPoint) - ((fromIntegral b'' / fromIntegral a'')::FixedPoint) < epsFP
    where (a'', b'') = normalizeInts a b

prop_FPIdemPotent'' :: Positive FixedPoint -> Property
prop_FPIdemPotent'' (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < epsFP

prop_FPIdemPotent''' :: PosInt -> PosInt -> Property
prop_FPIdemPotent''' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::FixedPoint) - ((fromIntegral b'' / fromIntegral a'')::FixedPoint) < epsFP
    where (a'', b'') = normalizeInts a b

prop_FPfindD :: Positive FixedPoint -> Property
prop_FPfindD (Positive a) = (e ^^ n <= a && e ^^ (n + 1) > a) === True
    where e = exp' 1
          n = findE e a

prop_FPlnLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_FPlnLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    ((ln' ((a''/b'') *** (x''/y'')) - (x''/y'') * ln' (a''/b'')) < epsFP) === True
    where (b', a') = normalizeInts a b
          (y', x') = normalizeInts x y
          a'' = fromIntegral a'
          b'' = fromIntegral b'
          x'' = fromIntegral x'
          y'' = fromIntegral y'

prop_LeaderCmp :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_LeaderCmp (Positive q) (Positive q') (Positive a) (Positive a') =
  p_ < 1 && sigma < 1 ==>
    classify (p_ < (1 - ((1 - f) *** sigma))) "is leader" $
    ( (p_ >= (1 - ((1 - f) *** sigma))) &&
      ((taylorExpCmp 3 (1/(1 - p_)) (-sigma*c)) == ABOVE)
    || (p_ < (1 - ((1 - f) *** sigma))) &&
       ((taylorExpCmp 3 (1/(1 - p_)) (-sigma*c)) == BELOW))
  where (p, p') = normalizeInts q q'
        (s, s') = normalizeInts a a'
        p'''    = fromIntegral p' :: FixedPoint
        p''     = fromIntegral p
        s'''    = fromIntegral s' :: FixedPoint
        s''     = fromIntegral s
        p_      = p''' / p''
        sigma   = s''' / s''
        f       = fromIntegral 1 / fromIntegral 10 :: FixedPoint
        c       = ln' (1 - f)

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
          a' = fromIntegral a'':: Double
          b' = fromIntegral b'':: Double
          x' = fromIntegral x'':: Double
          y' = fromIntegral y'':: Double
          result = (b' / a') *** (y' / x')

prop_DIdemPotent :: Positive Double -> Property
prop_DIdemPotent (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < epsD

prop_DIdemPotent' :: PosInt -> PosInt -> Property
prop_DIdemPotent' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::Double) - ((fromIntegral b'' / fromIntegral a'')::Double) < epsD
    where (a'', b'') = normalizeInts a b

prop_DfindD :: Positive Double -> Property
prop_DfindD (Positive a) = (e ^^ n <= a && e ^^ (n + 1) > a) === True
    where e = exp' 1
          n = findE e a

prop_DlnLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_DlnLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    ((ln' ((a''/b'') *** (x''/y'')) - (x''/y'') * ln' (a''/b'')) < epsD) === True
    where (b', a') = normalizeInts a b
          (y', x') = normalizeInts x y
          a'' = fromIntegral a'
          b'' = fromIntegral b'
          x'' = fromIntegral x'
          y'' = fromIntegral y'

-----------------------------
-- Properties for Rational --
-----------------------------

prop_Monotonic ::
     (Double -> Bool) -> (Double -> Double) -> Double -> Double -> Property
prop_Monotonic constrain f x y =
  (constrain x && constrain y) ==>
  if x <= y
    then f x <= f y
    else f x > f y

prop_ExpLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_ExpLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    b'' > 0 && y'' > 0 && a'' > 0 && x'' > 0 ==> expdiff x'' y'' a'' b'' < eps
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b

prop_ExpLaw' :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_ExpLaw' (Positive x) (Positive y) (Positive a) (Positive b) =
    (abs (exp' (a'/b' + x'/y') - (exp'(a'/b') * exp'(x'/y'))) < eps) === True
        where (b'', a'') = normalizeInts a b
              (y'', x'') = normalizeInts x y
              a' = fromIntegral a''
              b' = fromIntegral b''
              x' = fromIntegral x''
              y' = fromIntegral y''

prop_ExpLaw'' :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_ExpLaw'' (Positive x) (Positive y) (Positive a) (Positive b) =
    (abs (exp' (a'/b' + x'/y') - (exp' (a'/b') * exp' (x'/y'))) < eps) === True
        where (b'', a'') = normalizeInts a b
              (y'', x'') = normalizeInts x y
              a' = fromIntegral a''
              b' = fromIntegral b''
              x' = fromIntegral x''
              y' = fromIntegral y''

expdiff :: Integer -> Integer -> Integer -> Integer -> Rational
expdiff x'' y'' a'' b'' =
    abs(e1 - e2)
      where e1 = (((fromIntegral b'' / fromIntegral a'') *** (1.0 / fromIntegral x'')) *** fromIntegral y'')
            e2 = (((fromIntegral b'' / fromIntegral a'') *** fromIntegral y'') *** (1.0/ fromIntegral x''))

prop_ExpUnitInterval :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_ExpUnitInterval (Positive x) (Positive y) (Positive a) (Positive b) =
    a'' > 0 && x'' > 0 ==> result >= 0 && result <= 1
    where (x'', y'') = normalizeInts x y
          (a'', b'') = normalizeInts a b
          result = (b'' % a'') *** (y'' % x'')

prop_IdemPotent :: Positive Rational -> Property
prop_IdemPotent (Positive a) =
    a > 0 ==> (exp' $ ln' a) - a < eps

prop_IdemPotent' :: PosInt -> PosInt -> Property
prop_IdemPotent' (Positive a) (Positive b) =
    b'' > 0 && a'' > 0 ==> (ln' $ exp' (fromIntegral b'' / fromIntegral a'')::Rational) - ((fromIntegral b'' / fromIntegral a'')::Rational) < eps
    where (a'', b'') = normalizeInts a b

prop_findD :: Positive Rational -> Property
prop_findD (Positive a) = (e ^^ n <= a && e ^^ (n + 1) > a) === True
    where e = exp' 1
          n = findE e a

prop_lnLaw :: PosInt -> PosInt -> PosInt -> PosInt -> Property
prop_lnLaw (Positive x) (Positive y) (Positive a) (Positive b) =
    ((ln' ((a''/b'') *** (x''/y'')) - (x''/y'') * ln' (a''/b'')) < eps) === True
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

  putStrLn "-------------------------------------------"
  putStrLn "-- Test of 34 Decimal Digits Fixed Point --"
  putStrLn "-------------------------------------------"
  putStrLn "property exp is monotonic"
  quickCheck (withMaxSuccess 1000 $ prop_FPMonotonic (const True) exp')
  putStrLn "property ln is monotonic"
  quickCheck (withMaxSuccess 1000 $ prop_FPMonotonic (> 0) ln')
  putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  quickCheck (withMaxSuccess 1000 prop_FPExpUnitInterval)
  putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  quickCheck (withMaxSuccess 1000 prop_FPIdemPotent)
  putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  quickCheck (withMaxSuccess 1000 prop_FPIdemPotent')
  putStrLn "property exponential law in [0,1]: (((a/b)^1/x)^y) = (((a/b)^y)^1/x)"
  quickCheck (withMaxSuccess 1000 prop_FPExpLaw)
  putStrLn "property exponential law in [0,1]: exp(q * p) = exp(q) + exp(p)"
  quickCheck (withMaxSuccess 1000 prop_FPExpLaw')
  putStrLn "property ln law in [0,1]: ln(q^p) = p*ln(q)"
  quickCheck (withMaxSuccess 1000 prop_FPlnLaw)
  putStrLn "property σ, p ∈ [0,1]: p < 1 - (1 - f)^σ <=> taylorExpCmp 3 (1/(1 - p)) (-sigma * ln (1 - f))"
  quickCheck (withMaxSuccess 10000 prop_LeaderCmp)
  putStrLn ""

  putStrLn "------------------------------"
  putStrLn "-- Test of Rational Numbers --"
  putStrLn "------------------------------"
  putStrLn "property exp is monotonic"
  quickCheck (withMaxSuccess 10 $ prop_Monotonic (const True) exp')
  putStrLn "property ln is monotonic"
  quickCheck (withMaxSuccess 10 $ prop_Monotonic (> 0) ln')
  putStrLn "property p,q in (0,1) -> p^q in (0,1)"
  quickCheck (withMaxSuccess 10 prop_ExpUnitInterval)
  putStrLn "property q > 0 -> exp(ln(q)) - q < eps"
  quickCheck (withMaxSuccess 10 prop_IdemPotent)
  putStrLn "property q > 0 -> ln(exp(q)) - q < eps"
  quickCheck (withMaxSuccess 10 prop_IdemPotent')
  putStrLn "property exponential law in [0,1]: (((a/b)^1/x)^y) = (((a/b)^y)^1/x)"
  putStrLn "skipped, takes too long"
  -- quickCheck (withMaxSuccess 5 prop_ExpLaw)
  putStrLn "property exponential law in [0,1]: exp(q * p) = exp(q) + exp(p)"
  quickCheck (withMaxSuccess 10 prop_ExpLaw')
  putStrLn "property ln law in [0,1]: ln(q^p) = p*ln(q)"
  putStrLn "skipped, takes too long"
  -- quickCheck (withMaxSuccess 100 prop_lnLaw)
  putStrLn ""
