module Cardano.Ledger.NonIntegral
  ( (***),
    exp',
    ln',
    findE,
    splitLn,
    scaleExp,
    CompareResult (..),
    taylorExpCmp,
  )
where

data CompareResult a
  = BELOW a Int
  | ABOVE a Int
  | MaxReached Int
  deriving (Show, Eq)

scaleExp :: (RealFrac a) => a -> (Integer, a)
scaleExp x = (x', x / fromIntegral x')
  where
    x' = ceiling x

-- | Exponentiation
(***) :: (RealFrac a, Enum a, Show a) => a -> a -> a
a *** b
  | b == 0 = 1
  | a == 0 = 0
  | a == 1 = 1
  | otherwise = exp' (b * ln' a)

ipow' :: Num a => a -> Integer -> a
ipow' x n
  | n == 0 = 1
  | m == 0 = let y = ipow' x d in y * y
  | otherwise = x * ipow' x (n - 1)
  where
    (d, m) = divMod n 2

ipow :: Fractional a => a -> Integer -> a
ipow x n
  | n < 0 = 1 / ipow' x (-n)
  | otherwise = ipow' x n

logAs :: (Num a) => a -> [a]
logAs a = a' : a' : logAs (a + 1)
  where
    a' = a * a

-- | Approximate ln(1+x) for x \in [0, \infty)
-- a_1 = x, a_{2k} = a_{2k+1} = x·k^2, k >= 1
-- b_n = n, n >= 0
lncf :: (Fractional a, Enum a, Ord a, Show a) => Int -> a -> a
lncf maxN x
  | x < 0 = error ("x = " ++ show x ++ " is not inside domain [0,..)")
  | otherwise = cf maxN 0 eps Nothing 1 0 0 1 as [1, 2 ..]
  where
    as = x : map (* x) (logAs 1)

eps :: (Fractional a) => a
eps = 1 / 10 ^ (24 :: Int)

-- | Compute continued fraction using max steps or bounded list of a/b factors.
-- The 'maxN' parameter gives the maximum recursion depth, 'n' gives the current
-- rursion depth, 'lastVal' is the optional last value ('Nothing' for the first
-- iteration). 'aNm2' / 'bNm2' are A_{n-2} / B_{n-2}, 'aNm1' / 'bNm1' are
-- A_{n-1} / B_{n-1}, and 'aN' / 'bN' are A_n / B_n respectively, 'an' / 'bn'
-- are lists of succecsive a_n / b_n values for the recurrence relation:
--
-- A_{-1} = 1,    A_0 = b_0
-- B_{-1} = 0,    B_0 = 1
-- A_n = b_n*A_{n-1} + a_n*A_{n-2}
-- B_n = b_n*B_{n-1} + a_n*B_{n-2}
--
-- The convergent 'xn' is calculated as x_n = A_n/B_n
--
--                        a_1
-- result = b_0 + ---------------------
--                           a_2
--                b_1 + ---------------
--                              a_3
--                      b_2 + ---------
--                                  .
--                            b_3 +  .
--                                    .
--
-- The recursion stops once 'maxN' iterations have been reached, or either the
-- list 'as' or 'bs' is exhausted or 'lastVal' differs less than 'epsilon' from the
-- new convergent.
cf ::
  (Fractional a, Ord a, Show a) =>
  Int ->
  Int ->
  a ->
  Maybe a ->
  a ->
  a ->
  a ->
  a ->
  [a] ->
  [a] ->
  a
cf maxN n epsilon lastVal aNm2 bNm2 aNm1 bNm1 (an : as) (bn : bs)
  | maxN == n = xn
  | converges = xn
  | otherwise = cf maxN (n + 1) epsilon (Just xn) aNm1 bNm1 aN bN as bs
  where
    converges = maybe False (\x -> abs (x - xn) < epsilon) lastVal
    xn = aN / bN -- convergent
    aN = bn * aNm1 + an * aNm2
    bN = bn * bNm1 + an * bNm2
cf _ _ _ _ _ _ aN bN _ _ = aN / bN

-- | Simple way to find integer powers that bound x. At every step the bounds
-- are doubled. Assumption x > 0, the calculated bound is `factor^l <= x <=
-- factor^u`, initially x' is assumed to be `1/factor` and x'' `factor`, l = -1
-- and u = 1.
bound ::
  (Fractional a, Ord a) =>
  a ->
  a ->
  a ->
  a ->
  Integer ->
  Integer ->
  (Integer, Integer)
bound factor x x' x'' l u
  | x' <= x && x <= x'' = (l, u)
  | otherwise = bound factor x (x' * x') (x'' * x'') (2 * l) (2 * u)

-- | Bisect bounds to find the smallest integer power such that
-- `factor^n<=x<factor^(n+1)`.
contract ::
  (Fractional a, Ord a) =>
  a ->
  a ->
  Integer ->
  Integer ->
  Integer
contract factor x = go
  where
    go l u
      | l + 1 == u = l
      | otherwise =
          if x < x'
            then go l mid
            else go mid u
      where
        mid = l + ((u - l) `div` 2)
        x' = ipow factor mid

exp1 :: (RealFrac a, Show a) => a
exp1 = exp' 1

-- | find n with `e^n<=x<e^(n+1)`
findE :: (RealFrac a) => a -> a -> Integer
findE e x = contract e x lower upper
  where
    (lower, upper) = bound e x (1 / e) e (-1) 1

-- | Compute natural logarithm via continued fraction, first splitting integral
-- part and then using continued fractions approximation for `ln(1+x)`
ln' :: (RealFrac a, Enum a, Show a) => a -> a
ln' x
  | x <= 0 = error (show x ++ " is not in domain of ln")
  | otherwise = fromIntegral n + lncf 1000 x'
  where
    (n, x') = splitLn x

splitLn :: (RealFrac a, Show a) => a -> (Integer, a)
splitLn x = (n, x')
  where
    n = findE exp1 x
    y' = ipow exp1 n
    x' = (x / y') - 1 -- x / e^n > 1!

exp' :: (RealFrac a, Show a) => a -> a
exp' x
  | x < 0 = 1 / exp' (-x)
  | otherwise = ipow x' n
  where
    (n, x_) = scaleExp x
    x' = taylorExp 1000 1 x_ 1 1 1

taylorExp :: (RealFrac a, Show a) => Int -> Int -> a -> a -> a -> a -> a
taylorExp maxN n x lastX acc divisor
  | maxN == n = acc
  | abs nextX < eps = acc
  | otherwise = taylorExp maxN (n + 1) x nextX (acc + nextX) (divisor + 1)
  where
    nextX = (lastX * x) / divisor

-- | Efficient way to compare the result of the Taylor expansion of the
-- exponential function to a threshold value. Using error estimation one can
-- stop early, once it's known the result will certainly be above or below the
-- target value.
taylorExpCmp :: (RealFrac a) => a -> a -> a -> CompareResult a
taylorExpCmp boundX cmp x = go 1000 0 x 1 1
  where
    go maxN n err acc divisor
      | maxN == n = MaxReached n
      | cmp >= acc' + errorTerm = ABOVE acc' (n + 1)
      | cmp < acc' - errorTerm = BELOW acc' (n + 1)
      | otherwise = go maxN (n + 1) err' acc' divisor'
      where
        errorTerm = abs (err' * boundX)
        divisor' = divisor + 1
        nextX = err
        err' = (err * x) / divisor'
        acc' = acc + nextX
