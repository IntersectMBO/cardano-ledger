-- | Generally useful generators not present in the upstream release
-- of Hedgehog
module Hedgehog.Gen.Double (doubleInc) where

import Data.Bits (shiftL)
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generates a Double in the inclusive range [0, 1]
doubleInc :: Gen Double
doubleInc = do
  b <- Gen.word64 (Range.linear 1 hi)
  a <- Gen.word64 (Range.linear 0 b)
  return $ (fromIntegral a) / (fromIntegral b)
  where
    hi = 2 `shiftL` 48 :: Word64
