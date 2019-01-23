module NonIntegral
  ( (***)
  , exp'
  , ln'
  ) where

-- | Exponentiation
(***) :: (Fractional a, Enum a, Ord a, Show a) => a -> a -> a
a *** b = exp' $ b * ln' a

-- -- | scale x for exp calculation,
-- scaleExp :: (Fractional a, Enum a, Ord a, Show a) => a -> (Integer, a)
-- scaleExp x = (numerator x, 1 % denominator x)

-- | compute e^x using continued fractions. For x < 0 compute 1/e^(-x). Scaling
-- to x' \in [0,1] is currently not done, did not provide advantage for
-- Rational.
exp' :: (Fractional a, Enum a, Ord a, Show a) => a -> a
exp' x
  | x < 0 = 1 / exp' (-x)
  | x == 0 = 1
  | otherwise
   = euler
  where
    euler = fexp 100 x

-- | Approximate exp(x) via continued fraction.
fexp :: (Fractional a, Enum a, Ord a, Show a) => Int -> a -> a
fexp maxN x =
  cf maxN 0 1 0 1 1 [x * a | a <- 1 : [-1,-2 ..]] (1 : [x + b | b <- [2 ..]])

logAs :: (Fractional a, Enum a, Ord a, Show a) => a -> [a]
logAs a = a' : a' : logAs (a + 1)
  where
    a' = a * a

-- | Approximate ln(1+x) for x \in [1, \infty)
fln :: (Fractional a, Enum a, Ord a, Show a) => Int -> a -> a
fln maxN x = cf maxN 0 1 0 0 1 (x : [a * x | a <- logAs 1]) [1 ..]

-- | Compute continued fraction using max steps or bounded list of a/b factors.
cf ::
     (Fractional a, Enum a, Ord a, Show a)
  => Int
  -> Int
  -> a
  -> a
  -> a
  -> a
  -> [a]
  -> [a]
  -> a
cf _ _ _ _ aNm1 bNm1 _ [] = aNm1 / bNm1
cf _ _ _ _ aNm1 bNm1 [] _ = aNm1 / bNm1
cf maxN n aNm2 bNm2 aNm1 bNm1 (an:as) (bn:bs)
  | maxN == n = aN / bN
  | otherwise = cf maxN (n + 1) aNm1 bNm1 aN bN as bs
  where
    aN = bn * aNm1 + an * aNm2
    bN = bn * bNm1 + an * bNm2

-- | Simple way to find integer powers that bound x. At every step the bounds
-- are doubled. Assumption x > 0, the calculated bound is `factor^l <= x <=
-- factor^u`.
bound ::
     (Fractional a, Enum a, Ord a, Show a)
  => a
  -> a
  -> a
  -> a
  -> Integer
  -> Integer
  -> (Integer, Integer)
bound factor x x' x'' l u
  | x' <= x && x'' >= x = (l, u)
  | otherwise = bound factor x (x' / factor) (x'' * factor) (2 * l) (2 * u)

-- | Bisect bounds to find the smallest integer power such that
-- `factor^n<=x<factor^(n+1)`.
contract ::
     (Fractional a, Enum a, Ord a, Show a)
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
    x' = factor ^^ mid

e :: (Fractional a, Enum a, Ord a, Show a) => a
e = exp' 1

-- | find n with `e^n<=x<e^(n+1)`
findE :: (Fractional a, Enum a, Ord a, Show a) => a -> Integer
findE x = contract e x lower upper
  where
    (lower, upper) = bound e x (1 / e) e (-1) 1

-- | Compute natural logarithm via continued fraction, first splitting integral
-- part and then using continued fractions approximation for `ln(1+x)`
ln' :: (Fractional a, Enum a, Ord a, Show a) => a -> a
ln' x = fromIntegral n + fln 100 x'
  where
    n = findE x
    x' = (x / exp' (fromIntegral n)) - 1 -- x / e^n > 1!
