{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Utils where

import Cardano.Crypto.Hash (ByteString, Hash, HashAlgorithm, hashFromBytes, hashToBytes, sizeHash)
import Cardano.Crypto.Util (bytesToNatural, naturalToBytes)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import qualified Data.ByteString.Base16 as B16
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Proxy (..))
import Data.Foldable (Foldable (..))
import Data.List (sortOn)
import MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.TreeDiff (Expr, ToExpr (..))

agdaHashToBytes :: Int -> Integer -> ByteString
agdaHashToBytes hs = B16.encode . naturalToBytes hs . fromInteger

agdaHashToExpr :: Int -> Integer -> Expr
agdaHashToExpr hs = toExpr . agdaHashToBytes hs

hashToInteger :: Hash a b -> Integer
hashToInteger = toInteger . bytesToNatural . hashToBytes

integerToHash :: forall h a. HashAlgorithm h => Integer -> Maybe (Hash h a)
integerToHash = hashFromBytes . naturalToBytes (fromIntegral . sizeHash $ Proxy @h) . fromInteger

-- HSMap utils

unionsHSMap :: Ord k => [Agda.HSMap k v] -> Agda.HSMap k v
unionsHSMap = Agda.MkHSMap . sortOn fst . nubOrdOn fst . foldr' (\(Agda.MkHSMap x) y -> x <> y) []

unionHSMap :: Ord k => Agda.HSMap k v -> Agda.HSMap k v -> Agda.HSMap k v
unionHSMap x y = unionsHSMap [x, y]

mapHSMapKey :: (k -> l) -> Agda.HSMap k v -> Agda.HSMap l v
mapHSMapKey f (Agda.MkHSMap l) = Agda.MkHSMap $ first f <$> l

bimapMHSMap :: Applicative m => (k -> m k') -> (v -> m v') -> Agda.HSMap k v -> m (Agda.HSMap k' v')
bimapMHSMap f g (Agda.MkHSMap m) = Agda.MkHSMap <$> traverse (bimapM f g) m
