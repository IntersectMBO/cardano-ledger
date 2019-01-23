module NonIntegral
    ( (***)
    , exp'
    , ln'
    ) where
import Debug.Trace
import Data.Ratio

-- | Exponentiation
(***) :: Rational -> Rational -> Rational
a *** b = exp' $ b * ln' a

-- | scale x for exp calculation,
scaleExp :: Rational -> (Integer, Rational)
scaleExp x = (numerator x, 1 % denominator x)

exp' :: Rational -> Rational
exp' x
    | x < 0     = 1 / exp' (-x)
    | x == 0    = 1
    | otherwise  = --trace (show euler ++ "^" ++ show n) $
                euler -- pown n euler-- euler ^^ n
                --approxRational (((fromRational euler)::Double) ^^ n) 1e-15
    where (n, q) = scaleExp x
          euler  = fexp 100 x

-- | Approximate exp(x) for x \in [0, 1]
fexp :: Int -> Rational -> Rational
fexp maxN x = cf maxN 0 1 0 1 1 [x * a | a <- 1:[-1,-2..]] (1:[x + b | b <- [2..]])

logAs :: Rational -> [Rational]
logAs a = a':a':logAs (a + 1)
    where a' = a * a

-- | Approximate ln(1+x) for x \in [1, \infty)
fln :: Int -> Rational -> Rational
fln maxN x = cf maxN 0 1 0 0 1 (x:[a * x | a <- logAs 1]) [1..]

-- | Compute continued fraction
cf :: Int -> Int -> Rational -> Rational -> Rational -> Rational -> [Rational] -> [Rational] -> Rational

cf _ _ _ _ aNm1 bNm1 _ [] = aNm1 / bNm1
cf _ _ _ _ aNm1 bNm1 [] _ = aNm1 / bNm1

cf maxN n aNm2 bNm2 aNm1 bNm1 (an:as) (bn:bs)
    | maxN == n = --trace ("divide: " ++ show aNm1 ++ " b " ++ show bNm1) $
                  -- approxRational ((fromRational aN::Double) / (fromRational bN::Double)) 1e-16
                  aN / bN
    | otherwise = --trace ("n: " ++ show n) $
             cf maxN (n + 1) aNm1 bNm1 aN bN as bs
    where aN = bn * aNm1 + an * aNm2
          bN = bn * bNm1 + an * bNm2

bound :: Rational -> Rational -> Rational -> Rational -> Integer -> Integer -> (Integer, Integer)
bound factor x x' x'' l u
    | x' <= x && x'' >= x = (l, u)
    | otherwise = bound factor x (x'/factor) (x''*factor) (2*l) (2*u)

contract :: Rational -> Rational -> Integer -> Integer -> Integer
contract factor x l u
    | l + 1 == u = l
    | otherwise  = if x < x'
                   then contract factor x l mid
                   else contract factor x mid u
    where mid = l + ((u - l) `div` 2)
          x'  = factor ^^ mid

e :: Rational
e = exp' 1

findE :: Rational -> Integer
findE x = contract e x lower upper
    where (lower, upper) = bound e x (1/e) e (-1) 1

ln' :: Rational -> Rational
ln' x = fromIntegral n + fln 100 x'
    where n  = findE x
          x' = (x / exp' (fromIntegral n)) - 1 -- x / e^n > 1!

pown :: Integer -> Rational -> Rational
pown 0 _ = 1
pown 1 x = x
pown n x
    | even n = trace (" even n:" ++ show n ++ " " ++ show x) $ let m = pown (n `div` 2) x in let n' = m * m in n'
    | otherwise = trace (" odd n: " ++ show n ++ " " ++ show x)  $ x * pown (n - 1) x
