-- | Szudzik's Elegant Pairing Function
--
-- http://szudzik.com/ElegantPairing.pdf
--
-- For all non-negative integers:
--
-- @
-- uncurry pair . unpair = id
-- unpair . uncurry pair = id
-- @
-- Addapted from https://gist.github.com/klntsky/7026018c3341e6aa17bc237746ee0256#file-pairing-hs
-- We use Int rather than Integer, and need accuracy in the range [0..10000]
module Test.Cardano.Ledger.Constrained.Pairing (pair, unpair) where

-- | Pack two integers into one.
pair :: Int -> Int -> Int
pair y x =
  if y > x
    then y * y + x
    else x * x + x + y

-- | Unpack one integer into two.
unpair :: Int -> (Int, Int)
unpair z =
  if l < q
    then (q, l)
    else (l - q, q)
  where
    q = squareRoot z
    l = z - q ^ two

-- Adapted from https://wiki.haskell.org/Generic_number_type
squareRoot :: Int -> Int
squareRoot n | n < 0 = error ("squareRoot works only for positive Int: " ++ show n)
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
  let twopows = iterate (^ two) two
      (lowerRoot, lowerN) =
        last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
      newtonStep x = div (x + div n x) two
      iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
      isRoot r = r ^ two <= n && n < (r + 1) ^ two
   in case dropWhile (not . isRoot) iters of
        (r : _) -> r
        [] -> error ("Not possible, every positive Int has an Integral square root: " ++ show n)

-- Defeat type defaulting without specifying the type every time.
two :: Int
two = 2
{-# INLINE two #-}
