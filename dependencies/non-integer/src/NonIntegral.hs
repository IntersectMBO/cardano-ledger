module NonIntegral
  ( (***)
  , exp'
  , ln'
  , findE
  , splitLn
  , fexp
  , scaleExp
  ) where

--import Debug.Trace

scaleExp :: (RealFrac b) => b -> (Integer, b)
scaleExp x = (ceiling x, x / fromIntegral (ceiling x :: Integer))

-- | Exponentiation
(***) :: (RealFrac a, Enum a, Show a) => a -> a -> a
a *** b
  | a == 0 = if b == 0 then 1 else 0
  | a == 1 = 1
  | otherwise = --trace (show a ++ " *** " ++ show b) $
                exp' $ b * ln' a

-- | compute e^x using continued fractions. For x < 0 compute 1/e^(-x). Scale to
-- x' \in [0,1] to reduce overflow risk in numerical types with limited values.
exp' :: (RealFrac a, Enum a, Show a) => a -> a
exp' x
  | x < 0 = 1 / exp' (-x)
  | x == 0 = 1
  | x > 1  = --trace ("exp of " ++ show x) $
             let (n, euler) = scaleExp x in exp' euler ^^ n
  | otherwise = fexp 1000 x

-- | Approximate exp(x) via continued fraction.
fexp :: (Fractional a, Enum a, Ord a, Show a) => Int -> a -> a
fexp maxN x = --trace ("fexp " ++ show x) $
  cf maxN 0 Nothing 1 0 1 1 [x * a | a <- 1 : [-1,-2 ..]] (1 : [x + b | b <- [2 ..]])

logAs :: (Num a) => a -> [a]
logAs a = a' : a' : logAs (a + 1)
  where
    a' = a * a

-- | Approximate ln(1+x) for x \in [0, \infty)
fln :: (Fractional a, Enum a, Ord a, Show a) => Int -> a -> a
fln maxN x = if x < 0
             then error ("x = " ++ show x ++ "is not inside domain [0, ..) ")
             else --trace ("fln") $
                  cf maxN 0 Nothing 1 0 0 1 (x : [a * x | a <- logAs 1]) [1 ..]

eps :: (Fractional a) => a
eps = 1 / 10^30

-- | Compute continued fraction using max steps or bounded list of a/b factors.
cf ::
     (Fractional a, Ord a, Show a)
  => Int
  -> Int
  -> Maybe a
  -> a
  -> a
  -> a
  -> a
  -> [a]
  -> [a]
  -> a
cf _ _ _ _ _ aNm1 bNm1 _ [] = aNm1 / bNm1
cf _ _ _ _ _ aNm1 bNm1 [] _ = aNm1 / bNm1
cf maxN n lastVal aNm2 bNm2 aNm1 bNm1 (an:as) (bn:bs)
  | maxN == n = convergent
  | otherwise =
      case lastVal of
        Nothing -> cf maxN (n + 1) (Just convergent) aNm1 bNm1 aN bN as bs
        Just c' -> if abs(convergent - c') < eps
                   then --trace ("cf done n: " ++ show n) $
                        convergent
                   else --trace ("cf n: " ++ show n) $
                        cf maxN (n + 1) (Just convergent) aNm1 bNm1 aN bN as bs
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
-- factor^u`.
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
  | otherwise = bound factor x (x' / factor) (x'' * factor) (2 * l) (2 * u)

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
    x' = factor ^^ mid

e :: (RealFrac a, Enum a, Show a) => a
e = exp' 1

-- | find n with `e^n<=x<e^(n+1)`
findE :: (RealFrac a, Enum a, Show a) => a -> Integer
findE x = contract eone x lower upper
  where
    eone           = e
    (lower, upper) = bound eone x (1 / eone) eone (-1) 1

-- | Compute natural logarithm via continued fraction, first splitting integral
-- part and then using continued fractions approximation for `ln(1+x)`
ln' :: (RealFrac a, Enum a, Show a) => a -> a
ln' x = if x == 0
        then error "0 is not in domain of ln"
        else fromIntegral n + fln 1000 x'
  where (n, x') = splitLn x

splitLn :: (RealFrac b, Enum b, Show b) => b -> (Integer, b)
splitLn x = (n, x')
    where n = findE x
          x' = (x / exp' (fromIntegral n)) - 1 -- x / e^n > 1!
