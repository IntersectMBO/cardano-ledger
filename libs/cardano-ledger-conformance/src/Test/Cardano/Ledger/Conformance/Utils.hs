{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Utils where

import Cardano.Crypto.Hash (ByteString, Hash, HashAlgorithm, hashFromBytes, hashToBytes, sizeHash)
import Cardano.Crypto.Util (bytesToNatural, naturalToBytes)
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import qualified Data.ByteString.Base16 as B16
import Data.Data (Proxy (..))
import Test.Cardano.Ledger.TreeDiff (Expr, ToExpr (..))

standardHashSize :: Int
standardHashSize = fromIntegral . sizeHash $ Proxy @(HASH StandardCrypto)

agdaHashToBytes :: Int -> Integer -> ByteString
agdaHashToBytes hs = B16.encode . naturalToBytes hs . fromInteger

agdaHashToExpr :: Int -> Integer -> Expr
agdaHashToExpr hs = toExpr . agdaHashToBytes hs

hashToInteger :: Hash a b -> Integer
hashToInteger = toInteger . bytesToNatural . hashToBytes

integerToHash :: forall h a. HashAlgorithm h => Integer -> Maybe (Hash h a)
integerToHash = hashFromBytes . naturalToBytes (fromIntegral . sizeHash $ Proxy @h) . fromInteger
