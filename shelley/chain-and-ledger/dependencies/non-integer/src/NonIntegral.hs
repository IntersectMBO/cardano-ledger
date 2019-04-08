module NonIntegral
  ( (***)
  , exp'
  , ln'
  , findE
  , splitLn
  , scaleExp
  , CompareResult(..)
  , taylorExpCmp
  ) where

data CompareResult = BELOW
                   | ABOVE
                   | UNKNOWN
  deriving (Show, Eq)

scaleExp :: (RealFrac b) => b -> (Integer, b)
scaleExp x = (ceiling x, x / fromIntegral (ceiling x :: Integer))

-- | Exponentiation
(***) :: (RealFrac a, Enum a, Show a) => a -> a -> a
a *** b
  | a == 0 = if b == 0 then 1 else 0
  | a == 1 = 1
  | otherwise = exp' l
    where l = b * ln' a

ipow' :: Num a => a -> Integer -> a
ipow' x n
    | n == 0     = 1
    | mod n 2 == 0 = let res = ipow' x (div n 2) in res * res
    | otherwise  = x * ipow' x (n - 1)

ipow :: Fractional a => a -> Integer -> a
ipow x n
    | n < 0 = 1 / (ipow x (-n))
    | otherwise = ipow' x n

logAs :: (Num a) => a -> [a]
logAs a = a' : a' : logAs (a + 1)
  where
    a' = a * a

-- | Approximate ln(1+x) for x \in [0, \infty)
fln :: (Fractional a, Enum a, Ord a, Show a) => Int -> a -> a
fln maxN x = if x < 0
             then error ("x = " ++ show x ++ "is not inside domain [0, ..) ")
             else cf maxN 0 eps Nothing 1 0 0 1 (x : [a * x | a <- logAs 1]) [1,2 ..]

eps :: (Fractional a) => a
eps = 1 / 10^(24::Int)

-- | Compute continued fraction using max steps or bounded list of a/b factors.
-- The 'maxN' parameter gives the maximum recursion depth, 'n' gives the current
-- rursion depth, 'lastVal' is the optional last value ('Nothing' for the first
-- iteration). 'aNm2', 'bNm2' are a_{m-2} and b_{m-2}, and 'aNm1' / 'bNm1' are
-- a_{m-1} / b_{m-1} respectively, 'an' / 'bn' are lists of succecsive a_n / b_n
-- values for the recurrence relation:
--
-- A_{-1}  = 1                              B_{-1} = 0
-- A_0     = b_0                            B_0    = 1
-- A_{n+1} = b_{n+1}*A_n + a_{n+1}*A_{n-1}  B_{n+1} = b_{n+1}*B_n + a_{n+1}*B_{n-1}
--
-- the convergent is calculated as x_n = A_n/B_n
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
-- list 'as' or 'bs' is exhausted or 'lastVal' differs less than 'eps' from the
-- new convergent.
cf ::
     (Fractional a, Ord a, Show a)
  => Int
  -> Int
  -> a
  -> Maybe a
  -> a
  -> a
  -> a
  -> a
  -> [a]
  -> [a]
  -> a
cf _ _ _ _ _ _ aNm1 bNm1 _ [] = aNm1 / bNm1
cf _ _ _ _ _ _ aNm1 bNm1 [] _ = aNm1 / bNm1
cf maxN n epsilon lastVal aNm2 bNm2 aNm1 bNm1 (an:as) (bn:bs)
  | maxN == n = convergent
  | otherwise =
      case lastVal of
        Nothing -> cf maxN (n + 1) epsilon (Just convergent) aNm1 bNm1 aN bN as bs
        Just c' -> if abs(convergent - c') < epsilon
                   then convergent
                   else cf maxN (n + 1) epsilon (Just convergent) aNm1 bNm1 aN bN as bs
  where
    ba = bn * aNm1
    aa = an * aNm2
    aN = ba + aa
    bb = bn * bNm1
    ab = an * bNm2
    bN = bb + ab
    convergent = aN / bN

-- | Simple way to find integer powers that bound x. At every step the bounds
-- are doubled. Assumption x > 0, the calculated bound is `factor^l <= x <=
-- factor^u`, initially x' is assumed to be `1/factor` and x'' `factor`, l = -1
-- and u = 1.
bound ::
     (Fractional a, Ord a)
  => a
  -> a
  -> a
  -> a
  -> Integer
  -> Integer
  -> (Integer, Integer)
bound factor x x' x'' l u
  | x' <= x && x'' >= x = (l, u)
  | otherwise = bound factor x (x' * x') (x'' * x'') (2 * l) (2 * u)

-- | Bisect bounds to find the smallest integer power such that
-- `factor^n<=x<factor^(n+1)`.
contract ::
     (Fractional a, Ord a)
  => a
  -> a
  -> Integer
  -> Integer
  -> Integer
contract factor x l u
  | l + 1 == u = l
  | otherwise =
    if x < x'
      then contract factor x l mid
      else contract factor x mid u
  where
    mid = l + ((u - l) `div` 2)
    x' = ipow factor mid

e :: (RealFrac a, Show a) => a
e = exp' 1

-- | find n with `e^n<=x<e^(n+1)`
findE :: (RealFrac a) => a -> a -> Integer
findE eone x = contract eone x lower upper
  where
    (lower, upper) = bound eone x (1 / eone) eone (-1) 1

-- | Compute natural logarithm via continued fraction, first splitting integral
-- part and then using continued fractions approximation for `ln(1+x)`
ln' :: (RealFrac a, Enum a, Show a) => a -> a
ln' x = if x == 0
        then error "0 is not in domain of ln"
        else --trace ("(n, x, x') = (" ++ show n ++ ", " ++ show x ++ ", " ++ show x' ++ ")") $
             fromIntegral n + approxln
  where (n, x') = splitLn x
        approxln = fln 1000 x'

splitLn :: (RealFrac b, Show b) => b -> (Integer, b)
splitLn x = --trace ("(n, x', y') = (" ++ show n ++ ", " ++ show x' ++ ", " ++ show y' ++ ")") $
            (n, x')
    where n = findE e x
          y' = exp' (fromIntegral n)
          x' = (x / y') - 1 -- x / e^n > 1!

exp' :: (RealFrac a, Show a) => a -> a
exp' x
    | x < 0     = 1 / exp' (-x)
    | otherwise = ipow x' n
    where (n, x_) = scaleExp x
          x'      = taylorExp 1000 1 x_ 1 1 1

taylorExp :: (RealFrac a, Show a) => Int -> Int -> a -> a -> a -> a -> a
taylorExp maxN currentN x lastX acc divisor
    | maxN == currentN = acc
    | abs nextX < eps  = acc
    | otherwise = taylorExp maxN (currentN + 1) x nextX (acc + nextX) (divisor + 1)
    where nextX = (lastX * x) / divisor

taylorExpCmp :: (RealFrac a, Show a) => a -> a -> a -> CompareResult
taylorExpCmp boundX cmp x =
  taylorExpCmp' 1000 0 boundX cmp x x 1 1

taylorExpCmp' :: (RealFrac a, Show a) => Int -> Int -> a -> a -> a -> a -> a -> a -> CompareResult
taylorExpCmp' maxN currentN boundX cmp x err acc divisor
  | maxN == currentN = UNKNOWN
  | abs nextX < eps = UNKNOWN
  | otherwise =
    if cmp >= acc' + errorTerm then ABOVE
    else if cmp < acc' - errorTerm then BELOW
         else taylorExpCmp' maxN (currentN + 1) boundX cmp x error' acc' divisor'
              where divisor' = divisor + 1
                    errorTerm = error' * boundX
                    nextX = err
                    error' = (err * x) / divisor'
                    acc' = acc + nextX
