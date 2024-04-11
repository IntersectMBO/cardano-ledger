module Test.Cardano.Ledger.Conformance.Utils where

import Cardano.Crypto.Hash (ByteString)
import Cardano.Crypto.Util (naturalToBytes)
import qualified Data.ByteString.Base16 as B16
import Test.Cardano.Ledger.TreeDiff (Expr, ToExpr (..))

agdaHashToBytes :: Integer -> ByteString
agdaHashToBytes = B16.encode . naturalToBytes hashSize . fromInteger
  where
    -- TODO is there a way to get this from a `Crypto` instead of hard-coding?
    hashSize = 28

agdaHashToExpr :: Integer -> Expr
agdaHashToExpr = toExpr . agdaHashToBytes
