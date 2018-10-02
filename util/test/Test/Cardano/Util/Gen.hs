module Test.Cardano.Util.Gen
       ( genBytes
       , genUTF8Byte
       , gen32Bytes
       , genMillisecond
       , genMicrosecond
       , genWord32
       , genWord16
       , genNatural
       ) where

import           Cardano.Prelude

import           Data.Time.Units (Microsecond, Millisecond, fromMicroseconds)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genBytes :: Int -> Gen ByteString
genBytes n = Gen.bytes (Range.singleton n)

genUTF8Byte :: Gen ByteString
genUTF8Byte = Gen.utf8 (Range.constant 0 64) Gen.alphaNum

gen32Bytes :: Gen ByteString
gen32Bytes = genBytes 32

genMillisecond :: Gen Millisecond
genMillisecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genMicrosecond :: Gen Microsecond
genMicrosecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genWord32 :: Gen Word32
genWord32 = Gen.word32 Range.constantBounded

genWord16 :: Gen Word16
genWord16 = Gen.word16 Range.constantBounded

genNatural :: Gen Natural
genNatural = Gen.integral $ Range.linear 0 (fromIntegral (maxBound :: Int64))
