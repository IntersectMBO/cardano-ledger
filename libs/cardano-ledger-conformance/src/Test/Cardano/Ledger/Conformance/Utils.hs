module Test.Cardano.Ledger.Conformance.Utils where

import Cardano.Crypto.Hash (ByteString, Hash, hashToBytes)
import Cardano.Crypto.Util (bytesToNatural, naturalToBytes)
import qualified Data.ByteString.Base16 as B16
import Test.Cardano.Ledger.TreeDiff (Expr, ToExpr (..))

agdaHashToBytes :: Int -> Integer -> ByteString
agdaHashToBytes hashSize = B16.encode . naturalToBytes hashSize . fromInteger

agdaHashToExpr :: Int -> Integer -> Expr
agdaHashToExpr hashSize = toExpr . agdaHashToBytes hashSize

hashToInteger :: Hash a b -> Integer
hashToInteger = toInteger . bytesToNatural . hashToBytes
